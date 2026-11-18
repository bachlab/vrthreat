#' Check for NULLs
#'
#' @param ... variable(s) to check
#' 
#' @returns TRUE if NULL found, FALSE otherwise.
#' 
#' Is equivalent to is.null. Available for readability purposes.
#'
#' @export
#' @examples
validate_skip_on_null <- function(...){
  any(sapply(list(...), is.null))
}

#' Validates that an option parameter is one of the allowed values.  
#'
#' @param option option to check
#' @param ... valid options
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_option <- function(option, ..., .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""
 
  symb <- rlang::enexpr(option)
  if(rlang::is_symbol(symb)){
    variable_name <- rlang::as_label(symb)
    if (!option %in% list(...)) {
      warning(prefix, stringr::str_glue("`{variable_name}`:{option} invalid option"), call. = FALSE)
      return(FALSE)
    }
  } else {
    if (!option %in% list(...)) {
      warning(prefix, stringr::str_glue("{option} invalid option"), call. = FALSE)
      return(FALSE)
    }
  }

  TRUE
}

#' Validates that the argument is numeric. Fail quietly if NA_real_
#'
#' @param x Variable to check
#' @param min Minimum value allowed (inclusive) (default -Inf)
#' @param max Maximum value allowed (inclusive) (default +Inf)
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_numeric <- function(x, min = -Inf, max = Inf, .fun_name = NULL){
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""
  symb <- rlang::enexpr(x)

  if (length(x) == 1 && is.na(x) && is.numeric(x)){
    return(FALSE) #fail quietly
  }

  if(rlang::is_symbol(symb) || rlang::is_call(symb)){
    variable_name <- rlang::as_label(symb)
    if(is.null(x)){
      warning(prefix, stringr::str_glue("`{variable_name}` must be numeric - NULL instead"), call. = FALSE)
      return(FALSE)
    }
    for (temp in x){
      if (!is.numeric(temp)) {
        warning(prefix, stringr::str_glue("`{variable_name}`: {temp} must be numeric"), call. = FALSE)
        return(FALSE)
      }
      if (temp < min) {
        warning(prefix, stringr::str_glue("Input `{variable_name}`: {temp} required to be greater than or equal to {min}"), call. = FALSE)
        return(FALSE)
      }
      if (temp > max) {
        warning(prefix, stringr::str_glue("Input `{variable_name}`: {temp} required to be lesser than or equal to {max}"), call. = FALSE)
        return(FALSE)
      }
    }
  } else {
    if(is.null(x)){
      warning(prefix, stringr::str_glue("Input must be numeric - NULL instead"), call. = FALSE)
      return(FALSE)
    }
    for (temp in x){
      if (!is.numeric(temp)) {
        warning(prefix, stringr::str_glue("{temp} must be numeric"), call. = FALSE)
        return(FALSE)
      }
      if (temp < min) {
        warning(prefix, stringr::str_glue("Input {temp} required to be greater than or equal to {min}"), call. = FALSE)
        return(FALSE)
      }
      if (temp > max) {
        warning(prefix, stringr::str_glue("Input {temp} required to be lesser than or equal to {max}"), call. = FALSE)
        return(FALSE)
      }
    }
  }

  TRUE
}

#' Validate tibble, ensure minimum length and check for fully NA columns
#' 
#' The check for fully NA columns has to be enabled
#'
#' @param df The tibble to check
#' @param min_length Ensure tibble has a minimum length. Default is 1
#' @param check_na_columns A boolean flag to check for fully NA columns (position/rotation/gaze_origin/gaze_orientation). Default is FALSE
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_tibble_non_empty <- function(df, min_length = 1, check_na_columns = FALSE, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  if (nrow(df) < min_length) {
    warning(prefix, stringr::str_glue("Input contains {nrow(df)} row(s)"), call. = FALSE)
    return(FALSE)
  }

  #if any fully NA columns found
  if (check_na_columns){
    na_cols <- df %>%
      dplyr::ungroup() %>%
      dplyr::select(tidyr::any_of(c("pos_x", "pos_y", "pos_z", 
                                    "rot_x", "rot_y", "rot_z", 
                                    "gaze_origin_x", "gaze_origin_y", "gaze_origin_z", 
                                    "gaze_direction_x", "gaze_direction_y", "gaze_direction_z"))) %>%
      dplyr::summarise(dplyr::across(tidyr::everything(), ~ all(is.na(.x))))

    if(any(na_cols)){
      na_colnames <- paste(colnames(na_cols)[na_cols==TRUE], collapse = ", ")
      warning(prefix, stringr::str_glue("Input contains NA only cols {na_colnames}"), call. = FALSE)
      return(FALSE)
    }
  }
  TRUE
}

#' Validate if given tibble has required columns.
#' 
#' @param df The tibble to check
#' @param columns Vector strings containing names of columns to check presence of.
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_columns <- function(df, columns, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  missing_cols <- setdiff(columns, names(df))
  if (length(missing_cols) > 0) {
    warning(prefix, stringr::str_glue("Input df is missing required column(s): {paste(missing_cols, collapse = ', ')}"), call. = FALSE)
    return(FALSE)
  }

  TRUE
  }

#' Validate if given tibble has a time column, checks if all NA and ensures minimum length
#' 
#' Is stricter and warns if all timestamps are NA.
#'
#' @param df The tibble to check
#' @param min_length Ensure time column has a minimum length. Default is 1
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_time <- function(df, min_length = 1L, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  if (!"time" %in% names(df)) {
    warning(prefix, "Missing 'time' column.", call. = FALSE)
    return(FALSE)
  }

  if (length(df$time) < min_length) {
    warning(prefix, stringr::str_glue("'time' column is too short (length < {min_length})."), call. = FALSE)
    return(FALSE)
  }

  if (all(is.na(df$time))){
    warning(prefix, "'time' column is all NA", call. = FALSE)
    return(FALSE)
  }

  TRUE
}

#' Validate if given tibble has enough timestamps in
#'
#' @param df The tibble to check
#' @param min_time Ensures time column has min_length many timestamps after min_time (default -Inf)
#' @param max_time Ensures time column has max_length many timestamps before max_time (default Inf)
#' @param min_length Ensure time column has a minimum length. Default is 1
#'
#' @returns TRUE if passes checks, FALSE otherwise. Is quiet (no warnings generated)
#'
#' @export
#' @examples
validate_time_interval <- function(df, min_time = -Inf, max_time = Inf, min_length = 1L) {
  if (length(df[df$time >= min_time ,]$time) == 0) {
    return(FALSE)
  }

  if (length(df[df$time <= max_time ,]$time) == 0) {
    return(FALSE)
  }

  TRUE
}

#' Check if two data frames with `time` column overlap in time
#'
#' @param df1 First data frame
#' @param df2 Second data frame
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_overlap <- function(df1, df2, .fun_name = NULL) {

  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (max(df1$time, na.rm = TRUE) < min(df2$time, na.rm = TRUE) ||
      max(df2$time, na.rm = TRUE) < min(df1$time, na.rm = TRUE)) {
    warning(prefix, "Data frames have no overlap in time.", call. = FALSE)
    return(FALSE)
  }
  TRUE
}

#' Ensures if given tibble has `pos_x`, `pos_y` and `pos_z` columns
#'
#' @param df The tibble to check
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_position <- function(df, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df) && !is.list(df)) {
    warning(prefix, "Input must be a list, data frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  if (!"pos_x" %in% names(df)) {
    warning(prefix, "Missing 'pos_x' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"pos_y" %in% names(df)) {
    warning(prefix, "Missing 'pos_y' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"pos_z" %in% names(df)) {
    warning(prefix, "Missing 'pos_z' column.", call. = FALSE)
    return(FALSE)
  }

  TRUE
}

#' Ensures if given tibble has `rot_x`, `rot_y` and `rot_z` columns
#'
#' @param df The tibble to check
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_rotation <- function(df, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  if (!"rot_x" %in% names(df)) {
    warning(prefix, "Missing 'rot_x' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"rot_y" %in% names(df)) {
    warning(prefix, "Missing 'rot_y' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"rot_z" %in% names(df)) {
    warning(prefix, "Missing 'rot_z' column.", call. = FALSE)
    return(FALSE)
  }

  TRUE
}

#' Ensures if given tibble has either `rot_x`, `rot_y` and `rot_z` columns or 
#' `pos_x`, `pos_y` and `pos_z` columns
#'
#' @param df The tibble to check
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_position_or_rotation <- function(df, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  #if both fail generate all warnings
  if (!suppressWarnings(validate_has_position(df, .fun_name)) &&
      !suppressWarnings(validate_has_rotation(df, .fun_name))) {
    validate_has_position(df, .fun_name)
    validate_has_rotation(df, .fun_name)
    return(FALSE)
  }

  TRUE
}

#' Ensures if given tibble has either `rot_x`, `rot_y` and `rot_z` columns or 
#' `gaze_direction_x`, `gaze_direction_y` and `gaze_direction_z` columns
#'
#' @param df The tibble to check
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_gaze_or_rotation <- function(df, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  #if both fail generate all warnings
  if (!suppressWarnings(validate_has_gaze_direction(df, .fun_name)) &&
      !suppressWarnings(validate_has_rotation(df, .fun_name))) {
    validate_has_gaze_direction(df, .fun_name)
    validate_has_rotation(df, .fun_name)
    return(FALSE)
  }

  TRUE
}



#' Ensures if given tibble has `gaze_direction_x`, `gaze_direction_y` and `gaze_direction_z` columns
#'
#' @param df The tibble to check
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_has_gaze_direction <- function(df, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(df)) {
    warning(prefix, "Input must be a data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  if (!"gaze_direction_x" %in% names(df)) {
    warning(prefix, "Missing 'gaze_direction_x' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"gaze_direction_y" %in% names(df)) {
    warning(prefix, "Missing 'gaze_direction_y' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"gaze_direction_z" %in% names(df)) {
    warning(prefix, "Missing 'gaze_direction_z' column.", call. = FALSE)
    return(FALSE)
  }

  TRUE
}


#' Ensures if given list has `pos_x`, `pos_y` and `pos_z` columns of the same length. Ensures minimum number of rows
#'
#' @param pos_list Positional list to check
#' @param min_length Ensure tibble has a minimum length. Default is 1
#' @param drop_y Drop checks on `pos_y`. Default is FALSE
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples
validate_positional_list <- function (pos_list, min_length = 1, drop_y = FALSE, .fun_name = NULL) {
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.list(pos_list) && !is.data.frame(pos_list)) {
    warning(prefix, "Positional list must be a list, data.frame or tibble.", call. = FALSE)
    return(FALSE)
  }

  if (!"pos_x" %in% names(pos_list)) {
    warning(prefix, "Positional list missing 'pos_x' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"pos_y" %in% names(pos_list) && !drop_y) {
    warning(prefix, "Positional list missing 'pos_y' column.", call. = FALSE)
    return(FALSE)
  }

  if (!"pos_z" %in% names(pos_list)) {
    warning(prefix, "Positional list missing 'pos_z' column.", call. = FALSE)
    return(FALSE)
  }

  # Warn if columns have different lengths
  length_xs <- length(pos_list$pos_x)
  length_ys <- ifelse(drop_y, length_xs, length(pos_list$pos_y))
  length_zs <- length(pos_list$pos_z)
  if (length_xs != length_zs || length_xs != length_ys) {
    wrng_message <- stringr::str_glue("Positional list columns have lengths pos_x:{length_xs}, pos_y:{length_ys} and pos_z:{length_zs}")
    if(drop_y){
      wrng_message <- stringr::str_glue("Positional list columns have lengths pos_x:{length_xs}, and pos_z:{length_zs}")
    }
    warning(prefix, wrng_message, call. = FALSE)
    return(FALSE)
  }
  
  # Ensure minimum length across columns
  min_length_measured <- min(length_xs, length_ys, length_zs)
  if (!min_length_measured >= min_length) {
    warning(prefix, stringr::str_glue("Positional list contains {min_length_measured} rows"), call. = FALSE)
    return(FALSE)
  }

  TRUE
}

validate_timemask <- function(mask, .fun_name = NULL){
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  if (!is.data.frame(mask$intervals)){
    warning(prefix, "Timemask missing intervals", call. = FALSE)
    return(FALSE)
  }

  if (!"start_time" %in% names(mask$intervals)){
    warning(prefix, "Timemask intervals missing start_time column", call. = FALSE)
    return(FALSE)
  }

  if (!"end_time" %in% names(mask$intervals)){
    warning(prefix, "Timemask intervals missing end_time column", call. = FALSE)
    return(FALSE)
  }

  if(all(is.na(mask$intervals$start_time), is.na(mask$intervals$end_time))){
    warning(prefix, "Timemask intervals contain only NA values", call. = FALSE)
    return(FALSE)
  }

  if (!validate_numeric(mask$total_on_duration, .fun_name = .fun_name)){
    return(FALSE)
  }

  if (mask$total_on_duration < 0 || is.na(mask$total_on_duration)){
    warning(prefix, stringr::str_glue("Timemask total on duration is {mask$total_on_duration} < 0"), call. = FALSE)
    return(FALSE)
  }



  TRUE
}

#' Ensure every vector has the same length or has length 1
#'
#' @param ... vectors whose lengths will be compared
#' @param .fun_name A string containing the name of the calling to use as a prefix in the warning. Default is NULL
#'
#' @returns TRUE if passes checks, FALSE otherwise. Generates warnings.
#'
#' @export
#' @examples

validate_vector_lengths <- function(..., .fun_name = NULL){
  prefix <- if (!is.null(.fun_name)) paste0("[", .fun_name, "] ") else ""

  max_length <- max(purrr::map_vec(list(...), length))
  check_condition <- function(x){length(x) == 1 || length(x) == max_length}
  if (!all(purrr::map_vec(list(...), check_condition))) {
    warning(prefix, "Vector lengths mismatch (All vector inputs should have same length or length 1)", call. = FALSE)
    return(FALSE)
  }

  TRUE
}