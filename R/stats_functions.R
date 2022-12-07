#' Fit (generalised) linear random effects model robustly, checking for convergence
#'
#' This function fits a (G)LME. If the fit does not converge, it will try different
#' optimisers, and if still not converging will remove the integration over random effects.
#'
#' @param mdl_formula Complete model formula
#' @param mdl_data Data frame
#' @param mdl_family Error family (default: Gaussian)
#'
#' @return List with fitted fixed- and random-effects models, AIC difference
#' (negative in favour of random-effects), converging optimiser, estimated
#' marginal means
#' @export
#'
#' @examples
fit_mdl_robust <-
  function(mdl_formula, mdl_data, mdl_family = "Gaussian") {

    find_optimiser <-
      function(mdl_formula, mdl_data, mdl_family, nAGQ) {
        if (mdl_family != "Gaussian") {
          mdl_prelim <- lme4::glmer(mdl_formula,
                                    data = mdl_data,
                                    family = mdl_family,
                                    nAGQ = nAGQ)
        } else{
          mdl_prelim <- lme4::lmer(mdl_formula,
                                   data = mdl_data)
        }

        # if model does not converge (courtesy to https://joshua-nugent.github.io/allFit/)
        if (is.null(mdl_prelim@optinfo$conv$lme4$messages)) {
          mdl <- mdl_prelim
        } else {
          diff_optims <-
            lme4::allFit(
              mdl_prelim,
              maxfun = 1e5,
              parallel = 'multicore',
              ncpus = parallel::detectCores()
            )
          is.OK <- sapply(diff_optims, methods::is, "merMod")
          diff_optims.OK <- diff_optims[is.OK]
          lapply(diff_optims.OK, function(x)
            x@optinfo$conv$lme4$messages)

          if (length(diff_optims.OK) > 0) {
            convergence_results <-
              lapply(diff_optims.OK, function(x)
                x@optinfo$conv$lme4$messages)
            working_indices <- sapply(convergence_results, is.null)
          } else {
            working_indices <- 0
          }

          if (sum(working_indices) == 0) {
            print("No algorithms from allFit converged.")
            mdl <- NULL
          } else {
            mdl <- diff_optims[working_indices][[1]]
          }
        }
      }

    mdl <-
      find_optimiser(
        mdl_formula = mdl_formula,
        mdl_data = mdl_data,
        mdl_family = mdl_family,
        nAGQ = 1
      )

    if (is.null(mdl) &  (mdl_family != "Gaussian")) {
      print(
        "Now using fast approximation without evaluating the adaptive Gauss-Hermite approximation."
      )
      mdl <-
        find_optimiser(
          mdl_formula = mdl_formula,
          mdl_data = mdl_data,
          mdl_family = mdl_family,
          nAGQ = 0
        )
    }

    return(mdl)
  }


#' Fit (generalised) linear model, checking for convergence, and obtain marginal means
#'
#' This function builds a model formula, evaluates FFX and RFX versions robustly,
#' and estimates marginal means from the model with higher model evidence (FFX or RFX)
#'
#' @param DV Dependent variable (string)
#' @param IV Independent variables (vector of string)
#' @param df Trial results data frame
#' @param family Error family (default: Gaussian)
#'
#' @return List with fitted fixed- and random-effects models, AIC difference
#' (negative in favour of random-effects), converging optimiser, estimated
#' marginal means
#' @export
#'
#' @examples
fit_mdl <- function(DV,
                    IV,
                    df,
                    family = "Gaussian") {

  # build model formula
  formula_base <-
    stringr::str_c(" ~ ", stringr::str_flatten(IV, collapse = " * "))
  formula_ffx <-
    stats::as.formula(stringr::str_c(DV, formula_base, "+ 1 "))
  formula_rfx <-
    stats::as.formula(stringr::str_c(DV, formula_base, "+ (1 | ppid)"))

  # estimate FFX
  if (family != "Gaussian") {
    mdl_0 <- stats::glm(formula_ffx,
                 data = df,
                 family = family)
  } else{
    mdl_0 <- stats::lm(formula_ffx,
                data = df)
  }

  # estimate RFX
  mdl_1 <-
    fit_mdl_robust(mdl_formula = formula_rfx,
                   mdl_data = df,
                   mdl_family = family)

  # compare models
  if (is.null(mdl_1)) {
    optimiser <- NULL
    if (family != "Gaussian") nAGQ <- NULL
    AIC_rfx <- NA
  } else {
    optimiser <- mdl_1@optinfo$optimizer
    if (family != "Gaussian") nAGQ <- mdl_1@devcomp$dims["nAGQ"]
    AIC_rfx <- stats::AIC(mdl_1) - stats::AIC(mdl_0)
  }

  # estimate marginal means
  if (is.na(AIC_rfx) || AIC_rfx > 0) {
    emms <- emmeans::emmeans(mdl_0, stats::as.formula(formula_base))
  } else {
    emms <- emmeans::emmeans(mdl_1, stats::as.formula(formula_base))
  }

  # assemple and return results
  results <- list(
    mdl_ffx = mdl_0,
    mdl_rfx = mdl_1,
    AIC_rfx = AIC_rfx,
    optimiser = optimiser,
    emms = emms
  )

  if (family != "Gaussian") results$nAGQ <- nAGQ

  return(results)
}
