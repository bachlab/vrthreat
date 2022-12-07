

#' Split a path name into into its components
#'
#' @param path A path name
#'
#' @return a vector of path components
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
  do.call('file.path', as.list(path_comps[(length(path_comps)-1):1]))
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
read_trial_results <- function(data_dir, ...){

  column_spec <- readr::cols(ppid = readr::col_character())

  list.files(path = file.path(data_dir),
             pattern = "*trial_results.csv",
             recursive = TRUE,
             full.names = TRUE) %>%
    purrr::map_dfr(~readr::read_csv(., col_types = column_spec), ...)
}

#' Read log messages
#'
#' Read log messages from log files to combine with trial results
#'
#' @param data_dir Parent directory where data are stored
#' @param messages Vector of string messages to detect
#'
#' @return
#' @export
#'
#' @examples
read_log_messages <- function(data_dir, messages) {

  log_file_list <- list.files(path = file.path(data_dir),
             pattern = "*log.csv",
             recursive = TRUE,
             full.names = TRUE)

  read_log_file <- function(filename, ppid, session_num, messages) {
    suppressWarnings(
      read_csv(filename)) %>%
      find_log_message(messages) %>%
      dplyr::mutate(ppid = ppid,
                    session_num = session_num)
  }

  # extract ppid and session_num from path names
  df <- tibble::tibble(files = log_file_list) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      path = stringr::str_remove(files, stringr::fixed(data_dir)),
      # avoid backslashes getting in the way
      ppid = utils::tail(split_path(path), 1),
      snum = stringr::str_extract(path, paste0(file.path(
        data_dir, ppid, "S00\\d+"
      ))),
      snum = stringr::str_extract(path, "00\\d+"),
      snum = as.double(snum)
    ) %>%
    dplyr::select(!"path")

  purrr::pmap_dfr(
    list(
      filename = df$files,
      ppid = df$ppid,
      session_num = df$snum
    ),
    read_log_file,
    messages
  )
}

#' Read CSV Files
#'
#' Read CSV files associated with trials (e.g. Movement files) from paths stored
#' in supplied columns (with top-level directory removed).
#' Column `directory` must be present in dataframe.
#' Function uses `read_csv` to read files.
#'
#' @param data A dataframe of trial results
#' @param data_dir Parent directory where data are stored
#' @param .cols A tidyselect specification of columns (e.g. `c(col1, col2)`).
#' @param drop_tld Drop top-level directory in stored file location? (default TRUE)
#' @param ... Additional arguments passed to `read_csv`.
#' Default are columns that end with `_location_0`
read_csv_files <-
  function(data,
           data_dir,
           .cols = tidyselect::ends_with("_location_0"),
           drop_tld = TRUE,
           ...) {

    # take care of duplicated "x" column names in early versions of the VRthreat
    repair_names <- function(names) {
      x_cols <- which(names == "pos_x")
      if (length(x_cols) == 2) names[x_cols[2]] <- "pos_z"
      vctrs::vec_as_names(names, repair = "unique")
      }

  read_fn <- function(fname, drop_tdl, ...){
    if (is.na(fname)) return(NA)
    if (drop_tld) fname <- remove_tld(fname)
    fname <- file.path(data_dir,fname)
    if (!file.exists(fname)) return(NA)
    suppressWarnings(
      readr::read_csv(
        fname,
        n_max = 1e6,
        lazy = FALSE,
        name_repair = repair_names,
        ...
      )
    )
  }

  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(
      {{ .cols }},
      ~list(read_fn(., drop_tld, ...)),
      .names = "{str_replace(.col, '_location', '_data')}"
    )) %>%
    dplyr::ungroup()
}

#' Read JSON Files
#'
#' Read json files associated with trials from paths stored in supplied
#' columns (with top-level directory removed).
#' #' Column `directory` must be present in dataframe. Function uses
#' `jsonlite::read_json` to read files.
#'
#' @param data A dataframe of trial results
#' @param data_dir Parent directory where data are stored
#' @param .cols A tidyselect specification of columns (e.g. `c(col1, col2)`).
#' @param drop_tld Drop top-level directory from path
#' @param ... Additional arguments passed to `read_json`.
#' Default are columns that end with `_json_location_0`
read_json_files <-
  function(data,
           data_dir,
           .cols = ends_with("_json_location_0"),
           drop_tld = TRUE,
           ...) {
    read_fn <- function(fname, drop_tld) {
      if (is.na(fname))
        return(NA)
      if (drop_tld)
        fname <- remove_tld(fname)
      fname <- file.path(data_dir,
                         ifelse(endsWith(fname, ".json"),
                                fname,
                                paste0(fname, ".json")))
      if (!file.exists(fname))
        return(NA)
      jsonlite::read_json(fname,
                          ...)
    }

    data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across({
        {
          .cols
        }
      },
      ~ list(read_fn(., drop_tld)),
      .names = "{str_replace(.col, '_location', '_data')}")) %>%
      dplyr::ungroup()
  }

#' Read participant details from files
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
    dplyr::mutate(dplyr::across(.cols = tidyselect::matches("fn"),
                  ~ suppressWarnings(readr::read_csv(
                    .,
                    col_types = readr::cols()
                  ))))  %>%
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
#' following 4 csv files: replacements.csv, participants_excluded.csv,
#' episodes_info.csv, threats_info.csv
#' @return A data frame with all experiment data
#' @export
#'
#' @examples
read_study_data <- function(rawpath, metapath) {

# read replacements file
  replacements <- readr::read_csv(
    file.path(metapath,
              "replacements.csv"),
    col_types = readr::cols(
      ppid_old = readr::col_character(),
      ppid_new = readr::col_character(),
      session_num_new = readr::col_double(),
      last_trial_exclude = readr::col_double()
    ),
    n_max = 1000
  )
  # read episodes info
  episodes_info <-
    readr::read_csv(
      file.path(metapath,
                "episodes_info.csv"),
      col_types = readr::cols(episode = readr::col_character(),
                       threat = readr::col_character())
    )

  # read threats info
  threats_info <-
    readr::read_csv(
      file.path(metapath,
                "threats_info.csv"),
      col_types = readr::cols(
        threat = readr::col_character(),
        threat_speed = readr::col_double(),
        slow_threat = readr::col_logical(),
        domestic_threat = readr::col_logical(),
        predatory_threat = readr::col_logical(),
        defensive_threat = readr::col_logical(),
        disgust_threat = readr::col_logical()
      )
    )

  # read excluded participants
  participants_excluded <-
    readr::read_csv(
      file.path(metapath,
                "participants_excluded.csv"),
      col_types = readr::cols(ppid = readr::col_character()),
      n_max = 1000
    )

# read raw data
trials_raw <- read_trial_results(rawpath, na = c("", "NA", "Infinity")) %>%
  # read log messages for outcomes not logged in trial results data frame
  dplyr::left_join(
    read_log_messages(rawpath, "ConfrontedThreat 0 You were killed by a magical force"),
    by = c("ppid", "session_num", "trial_num")
  ) %>%
  dplyr::rename("killed_by_magical_force" = "...1") %>%
# filter out excluded participants
  dplyr::anti_join(participants_excluded, by = "ppid") %>%
  apply_replacements(replacements) %>%
  remove_tutorials() %>%
  dplyr::left_join(episodes_info, by = "episode", suffix = c("_raw", "")) %>%
  dplyr::left_join(threats_info, by = "threat", suffix = c("_raw", "")) %>%
  # read nested CSV files (movement files)
  read_csv_files(
    rawpath,
    c(
      head_movement_location_0,
      threat_movement_location_0,
      waist_movement_location_0
    ), drop_tld = TRUE,
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
  # read nested CSV files (tracker files for which rotations are never used, to save memory)
  read_csv_files(
    rawpath,
    c(
      righthand_movement_location_0,
      lefthand_movement_location_0,
      rightfoot_movement_location_0,
      leftfoot_movement_location_0
    ),
    drop_tld = TRUE,
    col_select = c("time", "pos_x", "pos_y", "pos_z"),
    col_types = readr::cols_only(
      time = readr::col_double(),
      pos_x = readr::col_double(),
      pos_y = readr::col_double(),
      pos_z = readr::col_double(),
    )
  ) %>%
  # read nested CSV files (selected columns for fruit task, to save memory)
  read_csv_files(
    rawpath,
    tidyselect::any_of("fruittask0.csv_location_0"),
    drop_tld = TRUE,
    col_select = c("time", "event"),
    col_types = readr::cols_only(
      time = readr::col_double(),
      event = readr::col_character()
    )
  ) %>%
  {
    if ("botheyes_eye_tracking_location_0" %in% names((.)))
      read_csv_files(
        .,
        rawpath,
        tidyselect::any_of("botheyes_eye_tracking_location_0"),
        drop_tld = TRUE,
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
          focus_point_z  = readr::col_double(),
          focus_distance = readr::col_double(),
          focus_threat = readr::col_character()
        )
      )
    else
      (.)
  } %>%
  # read nested JSON files (scenario, sequence)
  read_json_files(rawpath, c(scenario_location_0,
                             sequence0_location_0),
                  drop_tld = TRUE) %>%
  # reorder threats: first the fast ones by speed, then Dynamite (fast) and Croc(slow) by speed, then slow ones by speed, then no Threat
  dplyr::mutate(threat = forcats::fct_reorder(threat, threat_speed, mean, .desc = TRUE)) %>%
  dplyr::mutate(threat = forcats::fct_relevel(threat, "No threat", after = Inf)) %>%
  dplyr::mutate(threat = forcats::fct_relevel(threat, "Dynamite", "Crocodile", after = length(which(threats_info$threat_speed>2))))
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
