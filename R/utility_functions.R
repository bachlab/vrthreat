
#' Find Hierarchy Object
#'
#' Search an object hierarchy for an object by name. This is useful for searching for a specific object in a saved scenario `.json` file.
#'
#' @param x The object hierarchy to search. This is typically a `list` loaded from a scenario `.json` file.
#' @param name The object name to search for.
#' @param include_children Should children be included in the result?
#' @param partial_match Should the name match exactly (FALSE, default) or can it be a part of the actual name?
#'
#' @return The matching object, or NULL if none found.
#' @export
#'
#' @examples
find_hierarchy_object <-
  function(x,
           name,
           include_children = FALSE,
           partial_match = FALSE) {
    if (is.null(x[["name"]]))
      stop("'x' has incorrect format: no 'name' key.")

    if ((x[["name"]] == name) |
        (partial_match == TRUE &
         stringr::str_detect(x[["name"]], name))) {
      # if x is the desired object, return x
      if (!include_children) {
        x[["children"]] <- NULL
      }
      return(x)

    } else {
      for (child in x[["children"]]) {
        # return first with matching name
        search_result <-
          find_hierarchy_object(child, name, include_children = include_children, partial_match = partial_match)
        if (!is.null(search_result)) {
          return(search_result)
        }
      }

      # return null if none found
      return(NULL)
    }
  }


#' Find Hierarchy Object Matching Any Name
#'
#' Extended version of [find_hierarchy_object], returning the first object that
#' matches any of a range of names.
#'
#' @param x The object hierarchy to search. This is typically a `list` loaded
#'   from a scenario `.json` file.
#' @param any_names A vector of object names to search for.
#' @param include_children Should children be included in the result?
#'
#' @return The matching object, or NULL if none found.
#' @export
#'
#' @examples
find_hierarchy_object_any <- function(x, any_names, include_children = FALSE) {
  for (name in any_names) {
    # search for each name
    search_result <- find_hierarchy_object(x, name, include_children = include_children)
    if (!is.null(search_result)) {
      return(search_result)
    }
  }
  # return null if none found
  return(NULL)
}


#' Find messages in a session log and assign to trials
#'
#' @param df A session log data frame
#' @param messages A vector of string
#' @param .colnames A vector of column names, the same size as `messages`
#'
#' @return A data frame with `trial_num` and `.colnames` (boolean)
#' @export
#'
#' @examples
find_log_message <- function(df, messages, .colnames) {
  allmessages <- df %>%
    pull("message")

  # find start and end of trials
  trl_start_indx <-
    which(stringr::str_detect(allmessages, "Starting trial "))
  trl_end_indx <-
    which(stringr::str_detect(allmessages, "Ending trial "))

  trl_start_no <-
    stringr::str_extract(allmessages[trl_start_indx], "Starting trial \\d+") %>%
    str_extract("\\d+") %>%
    as.double()

  trl_end_no <-
    stringr::str_extract(allmessages[trl_end_indx], "Ending trial \\d+") %>%
    str_extract("\\d+") %>%
    as.double()

  trl_no <- intersect(trl_start_no, trl_end_no)

  unmatched_trl_start <- which(!(trl_start_no %in% trl_no))
  unmatched_trl_end   <- which(!(trl_end_no %in% trl_no))

  if (length(unmatched_trl_start) > 0) {
    trl_start_no <- trl_start_no[-unmatched_trl_start]
    trl_start_indx <- trl_start_indx[-unmatched_trl_start]
  }


  if (length(unmatched_trl_end) > 0) {
    trl_end_no <- trl_end_no[-unmatched_trl_end]
    trl_end_indx <- trl_end_indx[-unmatched_trl_end]
  }


  # find message and assign to trials
  find_message <-
    function(message,
             .colnames,
             allmessages,
             trl_no,
             trl_start_indx,
             trl_end_indx) {

      find_indx <- function(indx, trl_start_indx, trl_end_indx) {
        trl_indx <- which(indx > trl_start_indx & indx < trl_end_indx)
        if (length(trl_indx) == 0)
          trl_indx <- NA_real_

        return(trl_indx)
      }

      trl_indx <- which(stringr::str_detect(allmessages, message)) %>%
        purrr::map_dbl(find_indx, trl_start_indx, trl_end_indx) %>%
        unique()

      trl_no %in% trl_indx

  }

  df <-
    purrr::map2(messages,
                .colnames,
                find_message,
                allmessages,
                trl_no,
                trl_start_indx,
                trl_end_indx)
  names(df) <- .colnames
  df %>%
    tibble::as_tibble() %>%
    dplyr::mutate(trial_num = trl_no)
}
