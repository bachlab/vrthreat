#' Split a path name into into its components
#'
#' @param path A path name
#'
#' @return a vector of path components (starting with the last component)
#' @export
#'
#' @examples
split_path <- function(path) {
  if (dirname(path) %in% c(".", "/", "//", "\\" , "\\\\", path))
    return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}

#' Remove top-level directory in a full file path.
#'
#' Useful when top-level directory
#' name was changed during data curation
#'
#' @param pathname A string path
#'
#' @return New pathname with top-level directory removed
#' @export
#'
#' @examples
remove_tld <- function(pathname) {
  path_comps <- split_path(pathname)
  do.call('file.path', as.list(path_comps[(length(path_comps) - 1):1]))
}


#' Standardise file path
#' 
#' This function ensures standardised path separators and capitalised drive 
#' letters. This is important for string operations on path definitions.
#'
#' @param path 
#'
#' @returns standardised path
#' @export
#'
#' @examples
standardise_path <- function(path) {
  path_comps <- split_path(path)
  # ensure drive letter is capitalised
  if (str_detect(path_comps[length(path_comps)], "^[A-Za-z]:$")) {
    path_comps[length(path_comps)] <- toupper(path_comps[length(path_comps)])
     }
 
     do.call('file.path', as.list(path_comps[(length(path_comps)):1]))
  }

#' Read Trial Results
#'
#' Read & combine trial results files for a given experiment within a data
#' folder.
#'
#' @param data_dir Parent directory where data are stored
#' @param ... Arguments to be passed on to read_csv
#'
#' @return A combined data frame of all trial results.
#' @export
#'
#' @examples
read_trial_results <- function(data_dir, ...) {
  read_results_file <- function(fn, ...) {
    fn %>%
      readr::read_csv(show_col_types = FALSE, col_names = TRUE, ...) %>%
      dplyr::mutate(res_path = dirname(fn))
  }

  list.files(
    path = file.path(data_dir),
    pattern = "*trial_results.csv",
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    purrr::map(read_results_file, ...) %>%
    purrr::list_rbind()
}

#' Read log messages
#'
#' Read log messages from log files to combine with trial results
#'
#' @param df Data frame of trial results, containing column `res_path`
#' @param messages Vector of string messages to detect
#' @param .colnames Vector of column names to insert into `df`
#'
#' @return Original data frame with the following columsn added:
#'  `.colnames` (boolean), `.colnames_message`, `.colnames_time` ,
#'  `.colnames_number`, where the last three refer to the first detected
#'  message per trial
#' @export
#'
#' @examples
#' # find "killed by magical force" messages in study 2
#' trials_with_log_messages <-
#' trials_raw %>%
#'    read_log_messages("ConfrontedThreat 0 You were killed by a magical force",
#'                      "killed_by_magical_force" = "...1")

read_log_messages <- function(df, messages, .colnames) {
    read_log_file <- function(res_path, messages, .colnames) {
      suppressWarnings(readr::read_csv(file.path(res_path, "sessionlog", "log.csv"))) %>%
        find_log_message(messages, .colnames) %>%
        dplyr::mutate(res_path = res_path)
    }

    df %>%
      dplyr::pull(res_path) %>%
      unique() %>%
      purrr::map(read_log_file,
                 messages, .colnames) %>%
      purrr::list_rbind() %>%
      dplyr::left_join(df, ., by = c("res_path", "trial_num"))
  }

#' Read and Join Weapon Log Messages
#'
#' Extracts weapon-related log messages from session log files and joins them with the provided trial data.
#' This function reads log files located in each `res_path` directory, searches for specific weapon
#' events based on the provided message identifier, and appends the extracted information to the
#' original data frame.
#'
#' @param df A data frame containing trial results. Must include a `res_path` column indicating
#'   the results path for each trial and a `trial_num` column for trial numbering.
#' @param messages A string specifying the log message identifier to search for (e.g., `"@WEAPONATTACHEVENT"`).
#' @param .colnames A string specifying the name to assign to the extracted weapon message column (e.g., `"weapon"`).
#'
#' @return A data frame identical to the input `df` but augmented with new columns containing
#'   the extracted weapon messages (`weapon_message`) and the corresponding `res_path`. The
#'   joined data includes the weapon messages aligned with their respective trials based on
#'   `res_path` and `trial_num`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `trials_raw` is your initial data frame with columns `res_path`, `trial_num` and `missing_log_file`
#' trials_raw <- trials_raw %>%
#'   read_log_messages_weapons("@WEAPONATTACHEVENT", "weapon")
#' }
read_log_messages_weapons <- function(df, messages, .colnames) {
  read_log_file <- function(res_path, messages, .colnames) {
    log_path <- file.path(res_path, "sessionlog", "log.csv")
    if (!file.exists(log_path)) {
      # Track the missing file by returning a tibble with an indicator
      warning("File not found: ", log_path)
      return(tibble(res_path = res_path, missing_log_file = TRUE))
    }
    
    suppressWarnings(read_csv(log_path)) %>%
      find_log_message(messages, .colnames) %>%
      dplyr::mutate(
        res_path = res_path,
        weapon_message = as.character(weapon_message),
        missing_log_file = FALSE
      )
  }
  
  df %>%
    pull(res_path) %>%
    unique() %>%
    map(read_log_file, messages, .colnames) %>%
    list_rbind() %>%
    dplyr::mutate(weapon_message = as.character(weapon_message)) %>%
    dplyr::left_join(df, ., by = c("res_path", "trial_num"))
}

#' Update file pointers
#'
#' Update pointers to CSV files associated with trials (e.g. Movement files)
#' in supplied columns. This updates the absolute paths (which might have been changed)
#' stored in columns provided combines with the column `res_path` as generated by
#' `read_trial_results`. This function takes care with changes in file location
#' or renaming of individual participant folders.
#'
#' @param data  A dataframe of trial results
#' @param .cols A tidyselect specification of columns (e.g. `c(col1, col2)`).
#'              Default are columns that end with `_location_0`
update_file_pointers <- function(data, .cols = tidyselect::ends_with("_location_0")) {
  update_path <- function(old_path, res_path) {
    if (is.na(old_path))
      return(NA)
    subdir_list <-
      c(
        "camera",
        "othersessiondata",
        "othertrialdata",
        "participantdetails",
        "screen",
        "sessionlog",
        "settings",
        "trackers"
      )
    path_parts <- split_path(old_path)
    indx <- which(path_parts %in% subdir_list)[1]
    do.call(file.path, as.list(c(res_path, path_parts[seq(from = indx, to = 1)])))
  }

  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across({{.cols}}, ~ purrr::map2_chr(., res_path, update_path))) %>%
    dplyr::ungroup()
}

#' Remove study path from file path
#' 
#' This function takes a file path (in the format of a file pointer in a trial_results tibble)
#' and removes all path components above the ppid path. This is useful when 
#' combining information from multiple sources, for example manual sound classifications
#' obtained on different computers. 
#'
#' @param old_path 
#'
#' @returns new_path with study path removed
#' @export
#'
#' @examples
remove_study_path <- function(old_path) {
  if (is.na(old_path))
    return(NA)
  
  subdir_list <-
    c(
      "camera",
      "othersessiondata",
      "othertrialdata",
      "participantdetails",
      "screen",
      "sessionlog",
      "settings",
      "trackers"
    )
  path_parts <- split_path(old_path)
  indx <- which(path_parts %in% subdir_list)[1]
  
  # subdir path is two levels down from ppid path - we remove everything above ppid path
  do.call(file.path, as.list(c(path_parts[seq(from = (indx + 2), to = 1)])))
}


#' Read CSV Files
#'
#' Read CSV files associated with trials (e.g. Movement files) from paths stored
#' in supplied columns. Expects absolute paths, use `update_file_pointers` to
#' update them if necessary. The function gives a warning if a file is not found
#' or empty.
#' Function uses `read_csv` to read files.
#'
#' @param data A dataframe of trial results
#' @param .cols A tidyselect specification of columns (e.g. `c(col1, col2)`).
#' @param ... Additional arguments passed to `read_csv`.
#' Default are columns that end with `_location_0`
read_csv_files <- function (data,
                            .cols = tidyselect::ends_with("_location_0"),
                            ...) {
  # take care of duplicated "x" column names in early versions of the VRthreat
  repair_names <- function(names) {
    x_cols <- which(names == "pos_x")
    if (length(x_cols) == 2)
      names[x_cols[2]] <- "pos_z"
    vctrs::vec_as_names(names, repair = "unique")
  }

  read_fn <- function(fname, ...) {
    if (is.na(fname))
      return(NULL)
    if (!file.exists(fname)) {
      warning(paste("File not found:", fname))
      return(NULL)
    }
    if (file.info(fname)$size == 0) {
      warning(paste("File is empty:", fname))
      return(NULL)
    }
    suppressWarnings(readr::read_csv(
      fname,
      n_max = 1e+06,
      lazy = FALSE,
      name_repair = repair_names,
      ...
    ))
  }

  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across({{ .cols }}, ~ list(read_fn(., ...)), .names = "{stringr::str_replace(.col, '_location', '_data')}")
    ) %>%
    dplyr::ungroup()
}

#' Read JSON Files
#'
#' Read json files associated with trials from paths stored in supplied
#' columns. Expects absolute paths, use `update_file_pointers` to
#' update them if necessary. Function uses
#' `jsonlite::read_json` to read files. Corrupted files are ignored, and a list
#' of corrupted files is printed at the end.
#'
#' @param data A dataframe of trial results
#' @param .cols A tidyselect specification of columns (e.g. `c(col1, col2)`).
#' @param ... Additional arguments passed to `read_json`.
#' Default are columns that end with `_json_location_0`
read_json_files <-  function(data, .cols = ends_with("_json_location_0"), ...) {
  corrupted_files <- c()  # List to store corrupted files
  
  read_fn <- function(fname) {
    if (is.na(fname) || !file.exists(fname)) {
      corrupted_files <<- unique(c(corrupted_files, fname))  # Log missing file
      return(NULL)
    }
    
    fname <- ifelse(endsWith(fname, ".json"), fname, paste0(fname, ".json"))
    
    # Check if file is empty or truncated
    if (file.info(fname)$size == 0) {
      corrupted_files <<- unique(c(corrupted_files, fname))  # Log empty file
      return(NULL)
    }
    
    # Attempt to read JSON safely and catch EOF errors
    json_data <- tryCatch(
      jsonlite::read_json(fname),
      error = function(e) {
        if (grepl("premature EOF", e$message)) {
          message("Skipping corrupted JSON file due to EOF error: ", fname)
          corrupted_files <<- unique(c(corrupted_files, fname))  # Log corrupted file
          return(NULL)
        } else {
          stop(e)  # Re-throw other errors
        }
      }
    )
    
    return(json_data)
  }
  
  # Apply function across the correct columns (without modifying selection)
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across({{ .cols }}, # Keep original column selection logic
                    ~ list(read_fn(.)), .names = "{stringr::str_replace(.col, '_location', '_data')}")
    ) %>%
    dplyr::ungroup()
  
  # Print list of corrupted files
  if (length(corrupted_files) > 0) {
    message("Skipped ",
            length(corrupted_files),
            " corrupted JSON files:")
    print(corrupted_files)
  }
  
  return(data)  # Return modified dataset
}


#' Read participant details from files
#'
#' Will assume directory names assume to original participant ids, don''t use if
#' these have been changed.
#'
#' @param df A dataframe of trial results
#' @param data_dir Parent directory where data are stored
#'
#' @return Data frame with new columns
#' @export
#'

#' @examples
read_participant_details <- function(df, data_dir) {
  tibble::tibble(ppid = unique(df$ppid)) %>%
    dplyr::mutate(fn = file.path(
      data_dir,
      ppid,
      "S001",
      "participantdetails",
      "participant_details.csv"
    )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("fn"),
      ~ suppressWarnings(readr::read_csv(., col_types = readr::cols()))
    ))  %>%
    tidyr::unpack("fn") %>%
    dplyr::select(!matches("trackers_enabled")) %>%
    dplyr::right_join(df, by = "ppid")
}


#' Read complete study data
#'
#' This function reads all VRthreat output data from one study into a data frame.
#' The
#'
#' @param rawpath  The path that contains the individual subjects' directories.
#' The path name may have been changed during data curation, and the original
#' top-level directory name in the VR output will be replaced by this path.
#' @param metapath The path that contains the meta data. This must contain the
#' following csv files, which are used in their order of appeareance:
#' 1. replacements.csv: joins interrupted participants. Must contain columns
#'    ppid_old, ppid_new, session_num_new, last_trial_exclude
#' 2. participants_excluded.csv: participants to exclude. Must contain column
#'    ppid
#' 3. trial_exclusions.csv: individual trials to exclude. Must contain columns
#'    ppid, session_num, trial_num
#' @param remove_duplicates Whether or not to remove duplications in replaced 
#'    ppids, see `apply_replacements` for more information. Default: `TRUE`. 
#' @param remove_tutorial_epochs whether or not to remove tutorial epochs from the data 
#'    set. Default: `TRUE`.
#' @return A data frame with all experiment data
#' @export
#'
#' @examples
read_study_data <- function (rawpath,
                             metapath,
                             remove_duplicates = TRUE,
                             remove_tutorial_epochs = TRUE)
{
  cat("Loading replacements data...\n")
  replacements <-
    readr::read_csv(
      file.path(metapath, "replacements.csv"),
      col_types = readr::cols(
        ppid_old = readr::col_character(),
        ppid_new = readr::col_character(),
        session_num_new = readr::col_double(),
        last_trial_exclude = readr::col_double()
      ),
      n_max = 1000
    )

  cat("Loading participants excluded data...\n")
  participants_excluded <- readr::read_csv(
    file.path(metapath, "participants_excluded.csv"),
    col_types = readr::cols(ppid = readr::col_character()),
    n_max = 1000
  )

  cat("Loading trials excluded data...\n")
  trials_excluded <-
    readr::read_csv(
      file.path(metapath, "trial_exclusions.csv"),
      col_types = cols(
        ppid = col_character(),
        session_num = col_double(),
        trial_num = col_double()
      )
    ) %>% mutate(excl_trial = 1)

  cat("Loading trial results...\n")
  column_spec <- readr::cols(ppid = readr::col_character(),
                             timeout = readr::col_double())
  trials_raw <- read_trial_results(rawpath,
                                   na = c("", "NA", "Infinity"),
                                   col_types = column_spec)

  cat("Updating results structure and reading csv/json files...\n")
  trials_raw <- trials_raw %>%
    update_file_pointers() %>%
    apply_replacements(replacements, remove_duplicates) %>%
    dplyr::anti_join(participants_excluded, by = "ppid") %>%
    left_join(trials_excluded, by = c("ppid", "session_num", "trial_num")) %>%
    filter(is.na(excl_trial) | excl_trial != 1) %>%
    select(!excl_trial) %>%
    {if (remove_tutorial_epochs) {
          remove_tutorials(.)
        } else { . }
      } %>%
     read_csv_files(
      c(
        head_movement_location_0,
        threat_movement_location_0,
        waist_movement_location_0
      ),
      col_types = readr::cols(
        time = readr::col_double(),
        pos_x = readr::col_double(),
        pos_y = readr::col_double(),
        pos_z = readr::col_double(),
        rot_x = readr::col_double(),
        rot_y = readr::col_double(),
        rot_z = readr::col_double(),
      )
    ) %>%
    read_csv_files(
      c(
        righthand_movement_location_0,
        lefthand_movement_location_0,
        rightfoot_movement_location_0,
        leftfoot_movement_location_0
      ),
      col_select = c("time", "pos_x", "pos_y", "pos_z"),
      col_types = readr::cols_only(
        time = readr::col_double(),
        pos_x = readr::col_double(),
        pos_y = readr::col_double(),
        pos_z = readr::col_double()
      )
    ) %>%
    read_csv_files(
      tidyselect::any_of("fruittask0.csv_location_0"),
      col_select = c("time", "event"),
      col_types = readr::cols_only(
        time = readr::col_double(),
        event = readr::col_character()
      )
    ) %>%
    {
      if ("botheyes_eye_tracking_location_0" %in% names(trials_raw))
        read_csv_files(
          .,
          tidyselect::any_of("botheyes_eye_tracking_location_0"),
          col_types = readr::cols(
            time = readr::col_double(),
            gaze_origin_x = readr::col_double(),
            gaze_origin_y = readr::col_double(),
            gaze_origin_z = readr::col_double(),
            gaze_direction_x = readr::col_double(),
            gaze_direction_y = readr::col_double(),
            gaze_direction_z = readr::col_double(),
            eye_openness_left = readr::col_double(),
            eye_openness_right = readr::col_double(),
            pupil_diameter_left = readr::col_double(),
            pupil_diameter_right = readr::col_double(),
            focus_object_raw = readr::col_character(),
            focus_point_x = readr::col_double(),
            focus_point_y = readr::col_double(),
            focus_point_z = readr::col_double(),
            focus_distance = readr::col_double(),
            focus_threat = readr::col_character()
          )
        )
      else
        .
    } %>%
    # read nested JSON files (scenario, sequence)
    read_json_files(c(
      scenario_location_0,
      tidyselect::starts_with("sequence") &
        tidyselect::ends_with("location_0")
    ))
}


#' Create output folders under an experiment name
#'
#' @param experiment_name A string representing the experiment name.
#'
#' @return Nothing.
#' @export
#'
#' @examples
create_out_folders <- function(experiment_name) {
  out_folder <- "out"
  experiment_folder <- file.path("out", experiment_name)
  figs_folder <- file.path("out", experiment_name, "figs")
  data_folder <- file.path("out", experiment_name, "data")

  if (!dir.exists(out_folder))
    dir.create(out_folder)
  if (!dir.exists(experiment_folder))
    dir.create(experiment_folder)
  if (!dir.exists(figs_folder))
    dir.create(figs_folder)
  if (!dir.exists(data_folder))
    dir.create(data_folder)
}


#' Wrapper around `ggsave` to save figures.
#'
#' @param plt The plot object.
#' @param experiment_name The experiment name.
#' @param fname The file name including extension.
#' @param type Type passed to `ggsave`
#' @param device Device passed to `ggsave` (default: "png")
#' @param dpi dpi passed to `ggsave`
#' @param width width of the plot (default units are inches)
#' @param height height of the plot (default units are inches)
#' @param ... Other arguments passed to `ggsave`.
#'
#' @return
#' @export
#'
#' @examples
save_fig <-
  function(plt,
           experiment_name,
           fname,
           type = "cairo",
           device = "png",
           dpi = 600,
           width = 4,
           height = 4,
           ...) {
    ggsave(
      file.path("out", experiment_name, "figs", fname),
      plt,
      type = type,
      dpi = dpi,
      width = width,
      height = height,
      device = device,
      ...
    )
  }


#' Join CSV Files to Trial Results
#'
#' Recursively loads and joins trial-level data (usually additional movement data recorded outside Unity, e.g., MoCap data) to an existing tibble. 
#' Matches files based on `ppid`, `session_num`, and `trial_num`, using both folder structure and filename information.
#' File names and file tree must follow a standard format (see below).
#'
#' @param trial_results A data frame or tibble containing trial-level metadata with columns `ppid`, `session_num`, and `trial_num`.
#' @param processed_base_folder A character string specifying the path to the base folder containing processed `.csv` movement files.
#' @param column_name A character string giving the name for the new column containing nested tibbles of movement data. Default is `"processed_movement_data"`.
#' @param verbose Whether to save a csv of unmatched files
#' 
#' @return A tibble: the original `trial_results` with an added column (named by `column_name`) containing nested data frames of csv data, and
#' a column ((named by `column_name` + "_filepath") with the source path of this nested tibble
#' Files that could not be matched or read are skipped, with warnings shown.
#'
#' @details
#' - File name must start with `"T"` (for trial), followed by the trial number
#' - Files starting with `"R"` (for run) are actively ignored (e.g., `R001_vrthreat.csv`), as they refer to data not recorded trial-by-trial 
#' - Session numbers are inferred either from the folder name (`.../1_2/S001/...`) or directly from the session folder (`.../1/S002/...`).
#' - Trial numbers are extracted from filenames like `T034_*.csv`.
#' - Data loading is performed safely with `purrr::possibly()` to avoid stopping on corrupted files.
#'
#' @examples
#' \dontrun{
#' df_with_movement <- join_data(
#'   trial_results = trial_results,
#'   processed_base_folder = ".../study5/processed/vrthreat_reformatted",
#'   column_name = "processed_movement_data"
#' )
#' }
#'
#' @export
join_data <- function(trial_results, 
                      processed_base_folder, 
                      column_name = "processed_movement_data",
                      verbose = FALSE) {
  # Input checks
  if (!is.data.frame(trial_results)) {
    stop("`trial_results` must be a data frame or tibble.")
  }
  required_cols <- c("ppid", "session_num", "trial_num")
  missing_cols <- setdiff(required_cols, names(trial_results))
  if (length(missing_cols) > 0) {
    stop("`trial_results` is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.character(processed_base_folder) || length(processed_base_folder) != 1) {
    stop("`processed_base_folder` must be a single character string (a folder path).")
  }
  if (!dir.exists(processed_base_folder)) {
    stop("The folder `processed_base_folder` does not exist: ", processed_base_folder)
  }
  if (!is.character(column_name) || length(column_name) != 1) {
    stop("`column_name` must be a single character string.")
  }
  if (column_name %in% colnames(trial_results)) {
    stop("Column named `column_name` already exists in `trial_results`.")
  }
  
  filepath_colname <- paste0(column_name, "_filepath")
  
  # standardise path string for string operations
  processed_base_folder <- standardise_path(processed_base_folder)
  
  # load filepaths
  all_files <- fs::dir_ls(processed_base_folder, recurse = TRUE, regexp = "\\.csv$")
  
  # Remove files that start with 'R'
  r_files <- all_files[grepl("/R\\d+_", basename(all_files))]
  if (length(r_files) > 0) {
    warning(
      length(r_files),
      " files start with 'R' and were skipped:\n",
      paste(r_files, collapse = "\n")
    )
    all_files <- setdiff(all_files, r_files)
  }
  
  
  parsed <- tibble::tibble(
    !!filepath_colname := all_files,
    file_name = basename(all_files),
    ppid_raw = stringr::str_match(
      all_files,
      paste0(processed_base_folder, "/([^/]+)/S\\d{3}/")
    )[, 2],
    trial_num = stringr::str_extract(basename(all_files), "T\\d{3}") %>%
      stringr::str_remove("T") %>%
      as.integer()
  ) %>%
    dplyr::filter(!is.na(ppid_raw)) %>%
    dplyr::mutate(
      ppid_parts = stringr::str_split(ppid_raw, "_"),
      ppid = purrr::map_chr(ppid_parts, ~ .x[1]),
      session_from_ppid = purrr::map_chr(ppid_parts, ~ if (length(.x) >= 2) .x[2] else NA_character_),
      session_from_folder = stringr::str_extract(.data[[filepath_colname]], "/S(\\d{3})") %>%
        stringr::str_remove_all("/S"),
      session_num = dplyr::coalesce(session_from_ppid, session_from_folder)
    ) %>%
    dplyr::select(all_of(filepath_colname), ppid, session_num, trial_num)
  
  # Match column types to trial_results
  trial_ppid_type <- typeof(trial_results$ppid)
  trial_session_type <- typeof(trial_results$session_num)
  trial_number_type <- typeof(trial_results$trial_num)
  parsed <- parsed %>%
    dplyr::mutate(
      ppid = match.fun(paste0("as.", trial_ppid_type))(ppid),
      session_num = suppressWarnings(match.fun(paste0("as.", trial_session_type))(session_num)),
      trial_num = match.fun(paste0("as.", trial_number_type))(trial_num)
    )
  
  # Checks
  matched <- dplyr::semi_join(parsed, trial_results, by = c("ppid", "session_num", "trial_num"))
  unmatched_parsed <- dplyr::anti_join(parsed, trial_results, by = c("ppid", "session_num", "trial_num"))
  unmatched_trial_results <- dplyr::anti_join(trial_results, parsed, by = c("ppid", "session_num", "trial_num"))
  
  # Warnings
  missing_ppid_raw <- sum(is.na(parsed$ppid))
  if (missing_ppid_raw > 0) {
    warning(
      missing_ppid_raw,
      " file paths could not be parsed for ppid/session. These will be skipped."
    )
  }
  num_skipped_trials <- sum(is.na(parsed$trial_num))
  if (num_skipped_trials > 0) warning(num_skipped_trials, " files had no recognizable trial number.")
  
  # Load movement data
  safe_read <- purrr::possibly(readr::read_csv, otherwise = NULL)
  parsed <- parsed %>%
    dplyr::mutate(!!column_name := purrr::map(
      .data[[filepath_colname]],
      ~ safe_read(.x, show_col_types = FALSE)
    )) %>%
    dplyr::select(ppid,
                  session_num,
                  trial_num,
                  dplyr::all_of(filepath_colname),
                  dplyr::all_of(column_name))
  
  failed_reads <- sum(sapply(parsed[[column_name]], is.null))
  if (failed_reads > 0) warning(failed_reads, " files failed to read and were set to NULL.")
  
  # Join to trial_results
  df <- dplyr::left_join(trial_results, parsed, by = c("ppid", "session_num", "trial_num")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      !!column_name := purrr::map(
        .data[[column_name]],
        ~ {
          if (is.null(.x) || !inherits(.x, "data.frame")) return(.x)
          .x %>% rename_with(tolower, matches("^Time$"))
        }
      )
    )
  
  # Summary report
  unmatched <- dplyr::filter(df, is.na(.data[[filepath_colname]]))
  if (nrow(unmatched) > 0) {
    message("Example of unmatched trials:")
    print(head(dplyr::select(unmatched, ppid, session_num, trial_num)))
  }
  missing_data_percentage <- sum(is.na(df[[filepath_colname]])) / nrow(df) * 100
  cat(
    "Percentage of rows with missing", column_name, ":",
    round(missing_data_percentage, 2), "%\n"
  )
  cat("Trials present in both:", nrow(matched), "\n")
  cat("Trials present in parsed but not present in trial_results:", nrow(unmatched_parsed), "\n")
  cat("Trials present in trial_results but not present in parsed:", nrow(unmatched_trial_results), "\n")
  
  if (verbose) {
    unmatched_parsed %>%
      dplyr::select(dplyr::all_of(filepath_colname)) %>%
      readr::write_csv(file = "unmatched_data.csv")
  }
  return(df)
}


#' Clean Scenario Data Column
#'
#' Checks for and removes rows in which the scenario data column has malformed or missing spatial information.
#'
#' @param df A data frame containing the scenario data column.
#' @param column_name Name of the column to inspect (default is `"scenario_data_0"`).
#'
#' @return The data frame with problematic rows removed.
#'
#' @examples
#' df <- clean_scenario_data(df)
clean_scenario_data <- function(df, column_name = "scenario_data_0") {
  # Ensure the column exists in the dataframe
  if (!(column_name %in% names(df))) {
    stop(paste("Column", column_name, "not found in dataframe!"))
  }
  
  message("Checking for issues in column: ", column_name, "...")
  
  # Internal function to check scenario data validity
  check_scenario_data <- function(data_column) {
    issues <- list()
    
    for (i in seq_along(data_column)) {
      entry <- data_column[[i]]
      
      # Ensure entry is a list
      if (!is.list(entry)) {
        issues[[length(issues) + 1]] <- list(index = i, issue = "Not a list")
        next
      }
      
      # Try extracting StartPoint
      start_pos <- tryCatch({
        vrthreat::find_hierarchy_object_any(entry, "StartPoint", include_children = TRUE)
      }, error = function(e) {
        NULL
      })
      
      # Check if StartPoint is found
      if (is.null(start_pos)) {
        issues[[length(issues) + 1]] <- list(index = i, issue = "StartPoint not found")
        next
      }
      
      # Check `position` values
      if (!("position" %in% names(start_pos)) ||
          !is.list(start_pos$position) ||
          !all(c("x", "y", "z") %in% names(start_pos$position)) ||
          !all(sapply(start_pos$position, is.numeric))) {
        issues[[length(issues) + 1]] <- list(index = i, issue = "Invalid position data")
        next
      }
      
      # Check `euler_angles` values
      if (!("euler_angles" %in% names(start_pos)) ||
          !is.list(start_pos$euler_angles) ||
          !all(c("x", "y", "z") %in% names(start_pos$euler_angles)) ||
          !all(sapply(start_pos$euler_angles, is.numeric))) {
        issues[[length(issues) + 1]] <- list(index = i, issue = "Invalid euler_angles data")
        next
      }
    }
    
    if (length(issues) > 0) {
      message("Issues found in the following rows:")
      print(issues)
    } else {
      message("All entries are correctly formatted.")
    }
    
    return(unlist(lapply(issues, function(x)
      x$index)))  # Return indices of problematic rows
  }
  
  # Run the check on the column
  problematic_rows <- check_scenario_data(df[[column_name]])
  
  # Remove problematic rows
  if (length(problematic_rows) > 0) {
    message(length(problematic_rows),
            " problematic rows found. Removing them...")
    
    # Print problematic row indices
    print(problematic_rows)
    
    # Remove the problematic rows
    df <- df[-problematic_rows, ]
    message(length(problematic_rows),
            " rows successfully removed from ",
            column_name,
            ".")
  } else {
    message("No problematic rows found in ", column_name, ". Data is clean.")
  }
  
  # Return the cleaned dataframe
  return(df)
}