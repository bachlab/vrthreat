
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
#' @return A data frame with `trial_num`, `.colnames` (boolean), `.colnames_message`
#' `.colnames_time` , `.colnames_number`, where the last three refer to the first
#' detected message per trial
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
             .colname,
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

      msg_indx <-
        which(stringr::str_detect(allmessages$message, message))

      if (length(msg_indx) > 0) {
        trl_indx <- msg_indx %>%
          purrr::map_dbl(find_indx, trl_start_indx, trl_end_indx)

        tibble::tibble(
          trlindx = trl_indx,
          "{.colname}"         := TRUE,
          "{.colname}_message" := allmessages$message[msg_indx],
          "{.colname}_time"    := allmessages$timestamp[msg_indx]
        ) %>%
          # log number of occurrences per trial but only use the first one
          dplyr::group_by(trlindx) %>%
          dplyr::mutate("{.colname}_number" := dplyr::n()) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          # integrate into all trials
          dplyr::right_join(tibble::tibble(trlindx = trl_no)) %>%
          dplyr::mutate("{.colname}" := ifelse(is.na(.data[[.colname]]),
                                          FALSE, TRUE)) %>%
          dplyr::arrange(trlindx) %>%
          dplyr::select(!trlindx)

      } else {
        tibble::tibble(
          trlindx = trl_no,
          "{.colname}"         := FALSE,
          "{.colname}_message" := NA,
          "{.colname}_time"    := NA,
          "{.colname}_number"  := NA
        ) %>%
          dplyr::select(!trlindx)
      }
    }

  purrr::map2(messages,
              .colnames,
              find_message,
              df,
              trl_no,
              trl_start_indx,
              trl_end_indx) %>%
    dplyr::bind_cols() %>%
    dplyr::mutate(trial_num = trl_no)

}

#' Load and Optionally Clean Trial Results
#'
#' This function loads trial results from a specified file and optionally cleans the data.
#' It ensures that the file exists, loads the data into memory, and applies basic cleaning steps
#' such as standardizing column names and removing location-related columns, if cleaning is enabled.
#'
#' @param trial_results_path A string specifying the file path to the trial results file.
#' The file must exist and be a valid R data file.
#' @param cleaning A logical value indicating whether to clean the loaded data. Defaults to `TRUE`.
#' If `TRUE`, column names are standardized to snake_case, and columns matching "location" are removed.
#'
#' @return A data frame containing the trial results, optionally cleaned.
#' @export
#'
#' @examples
#' # Load and clean trial results
#' trial_data <- load_trial_results("path/to/trial_results.RData")
#'
#' # Load trial results without cleaning
#' trial_data <- load_trial_results("path/to/trial_results.RData", cleaning = FALSE)
load_trial_results <- function(trial_results_path, cleaning = TRUE) {
  if (!file.exists(trial_results_path)) {
    stop("File not found: ", trial_results_path)
  }

  load(trial_results_path)
  if (cleaning) {
    trial_results <- trial_results %>%
      janitor::clean_names() %>%
      select(-matches("location"))
  }

  return(trial_results)
}


#' Source Functions, Run Tests, and Report Coverage
#'
#' This main function sources R scripts from a specified functions directory, runs
#' test files from a tests directory, and generates a test coverage report. It stops
#' execution if no function files are found and issues a warning if no test files are found.
#'
#' @param functions_dir The directory path containing function scripts.
#' @param tests_dir The directory path containing test scripts.
#'
#' @return Does not return a value; outputs coverage report.
#' @export
#'
#' @examples
#' test_source_fun_run_tests_rep_coverage("relative_path/to/functions", "relative_path/to/tests")
test_source_fun_run_tests_rep_coverage <-
  function(functions_dir, tests_dir, print_report=FALSE) {
    # Source function files. Stop if none are found.
    test_source_function_files <- function(directory) {
      script_files <-
        list.files(directory, pattern = "\\.R$", full.names = TRUE)

      if (length(script_files) == 0) {
        stop("No R scripts found in ", directory)
      }

      lapply(script_files, source)
      return(TRUE)
    }
    test_files_in_proj <- function(directory) {
      test_files <-
        list.files(directory, pattern = "\\.R$", full.names = TRUE)

      if (length(test_files) == 0) {
        warning("No test files found in ", directory)
      }

      for (file in test_files) {
        cat("\n", "Running tests in:", file, "\n")
        testthat::test_file(file)
      }
    }
    test_source_function_files(functions_dir)
    # Run tests. Warning if none are found.
    test_files_in_proj(tests_dir)

    if (print_report) {
      # Generate coverage report
      report <- covr::file_coverage(
        test_files   = list.files(tests_dir, pattern = "\\.R$", full.names = TRUE),
        source_files = list.files(functions_dir, pattern = "\\.R$", full.names = TRUE)
      )
      covr::report(report)
    }


  }

#' Identify Columns with Missing Values
#'
#' This function identifies columns in a data frame that contain missing (NA) values.
#'
#' @param data A data frame or tibble to inspect for missing values.
#' @param print_output Logical. If `TRUE`, prints the columns with their NA counts. Defaults to `TRUE`.
#' @param return_counts Logical. If `TRUE`, returns a named vector with column names and their respective NA counts. If `FALSE`, returns a character vector of column names. Defaults to `FALSE`.
#'
#' @return A character vector of column names with missing values, or a named vector with NA counts if `return_counts = TRUE`.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, NA), b = c(NA, 2, 3), c = c(1, 2, 3))
#' identify_na_columns(df)
#' identify_na_columns(df, return_counts = TRUE)
#'
#' @export
identify_na_columns <- function(data, print_output = TRUE, return_counts = FALSE) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }

  if (ncol(data) == 0) {
    message("The data frame has no columns.")
    return(character(0))
  }

  na_counts <- colSums(is.na(data))
  columns_with_na <- na_counts[na_counts > 0]

  if (print_output) {
    if (length(columns_with_na) > 0) {
      message("Columns with NA values:")
      for (col in names(columns_with_na)) {
        message("Column: ", col, " - NA count: ", columns_with_na[col])
      }
    } else {
      message("No columns with NA values found.")
    }
  }

  if (return_counts) {
    return(columns_with_na)
  } else {
    return(names(columns_with_na))
  }
}

#' Create Directory Paths Recursively
#'
#' Ensures subdirectories exist within a base path, creating them if necessary.
#'
#' @param base_path String. Root path to build from.
#' @param sub_paths Character vector of subdirectory names.
#'
#' @return Named list of full paths corresponding to each `sub_path`.
#'
#' @examples
#' paths <- create_file_paths("data/output", c("plots", "tables"))
create_file_paths <- function(base_path, sub_paths) {
  paths <- list()
  for (sub_path in sub_paths) {
    full_path <- file.path(base_path, sub_path)
    # Check if the directory already exists
    if (!dir.exists(full_path)) {
      dir.create(full_path,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
    paths[[sub_path]] <- full_path
  }
  return(paths)
}

#' Remove Columns with All Missing Values
#'
#' Removes columns that are entirely `NA` from a data frame and prints their names.
#'
#' @param data A data frame.
#'
#' @return A data frame with all-NA columns removed.
#'
#' @examples
#' df <- remove_missing_columns(df)
remove_missing_columns <- function(data) {
  missing_cols <- data %>%
    dplyr::select_if( ~ all(is.na(.))) %>%
    names()
  cat("Removing columns with all missing values:\n")
  print(missing_cols)
  return(data %>% dplyr::select(-all_of(missing_cols)))
}

#' Compare Column Names Between Two Data Frames
#'
#' Identifies common and unique column names between two datasets.
#'
#' @param df1 First data frame.
#' @param df2 Second data frame.
#'
#' @return A named list with elements: `common_columns`, `unique_to_df1`, and `unique_to_df2`.
#'
#' @examples
#' compare_colnames(df1, df2)
compare_colnames <- function(df1, df2) {
  # Get column names of the data frames
  colnames_df1 <- colnames(df1)
  colnames_df2 <- colnames(df2)
  
  # Find common column names
  common_cols <- intersect(colnames_df1, colnames_df2)
  
  # Find columns unique to each data frame
  unique_to_df1 <- setdiff(colnames_df1, colnames_df2)
  unique_to_df2 <- setdiff(colnames_df2, colnames_df1)
  
  # Create a summary list
  comparison_result <- list(
    common_columns = common_cols,
    unique_to_df1 = unique_to_df1,
    unique_to_df2 = unique_to_df2
  )
  
  # Return the result
  return(comparison_result)
}

#' Extract and annotate weapon-related details from raw event messages
#'
#' This function processes a data frame containing raw weapon event messages
#' (in the column `weapon_message`) and adds new columns describing the type,
#' effect, and usage details of the weapon. Specifically, it:
#' - Extracts the `weapon_type` from the message using a regex pattern.
#' - Classifies the `weapon_effect` (e.g., kill, stop, noEffect).
#' - Determines which hand (`weapon_hand`) the weapon was used with.
#' - Flags whether a weapon was picked up (`weapon_picked_up`).
#' 
#' @param df A data frame with at least a `weapon_message` column.
#' 
#' @return The input data frame with four new factor columns:
#'   `weapon_type`, `weapon_effect`, `weapon_hand`, and `weapon_picked_up`.
#' 
#' @examples
#' df <- create_weapon_details(df)
create_weapon_details <- function(df) {
  df %>%
    mutate(
      weapon_type = stringr::str_extract(weapon_message, "(?<=@WEAPONATTACHEVENT\\s)[^:]*"),
      weapon_effect = case_when(
        is.na(weapon_type) ~ "noWeapon",
        stringr::str_detect(weapon_type, "_Null") ~ "noEffect",
        stringr::str_detect(weapon_type, "_StopMoving") ~ "stop",
        !stringr::str_detect(weapon_type, "_") ~ "kill",
        TRUE ~ NA_character_
      ),
      weapon_type = case_when(
        is.na(weapon_type) ~ "noWeapon",
        stringr::str_detect(weapon_type, "Stick") ~ "stick",
        stringr::str_detect(weapon_type, "Stone") ~ "stone",
        stringr::str_detect(weapon_type, "Torch") ~ "torch",
        TRUE ~ "noWeapon"
      ),
      weapon_hand = case_when(
        stringr::str_detect(weapon_message, "Hand Anchor Right") ~ "right",
        stringr::str_detect(weapon_message, "Hand Anchor Left") ~ "left",
        TRUE ~ "noWeapon"
      ),
      weapon_picked_up = case_when(
        stringr::str_detect(weapon_message, "noWeapon") ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    mutate(weapon_type = as.factor(weapon_type),
           weapon_effect = as.factor(weapon_effect),
           weapon_hand = as.factor(weapon_hand),
           weapon_picked_up = as.factor(weapon_picked_up))
}


