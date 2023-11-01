#' Create threat name column from episode name
#'
#' Creates "threat" column, parsed from episode name. Expects column called
#' `episode`
#' For finding Unity's internal threat name, see "find_unity_threat_name"
#'
#' @param df A trial results data frame.
#'
#' @return The data frame with the new column added.
#' @export
#'
#' @examples
create_threat_name <- function(df){
  dplyr::mutate(df, threat = stringr::str_match(
    episode,
    stringr::regex(".*_\\d{3} (.+?)(?= -)", dotall = TRUE)
  )[, 2])
}

#' Remove tutorial trials - i.e. episode either containing "Tutorial" or
#' "Practice"
#'
#' @param df A trial results data frame.
#'
#' @return The data frame with the trials removed.
#' @export
#'
#' @examples
remove_tutorials <- function(df){
  dplyr::filter(
    df,
    !stringr::str_detect(episode, "Tutorial"),
    !stringr::str_detect(episode, "Practice")
  )
}

#' Fixes names in older versions of the output. Specifically, deletes the
#' occurance of `"vrthreat_"` in any column names.
#'
#' @param df A data frame.
#'
#' @return Fixed data frame.
#' @export
#'
#' @examples
fix_old_names <- function(df){
  dplyr::rename_with(df, ~stringr::str_replace(., "vrthreat_", ""))
}


#' Apply replacements using replacement data frame.
#'
#' Replaces the `ppid` and `session_num` of data in your trial results. This is
#' used if, for example, you must repeat some specific trials with a participant
#' under a new PPID. This replaces the trials found in the trial results with
#' those under the alternative ppid. They are marked with a new session number
#' to allow them to be distinguished. Any episodes (as defined by the string in
#' the episode column) are replaced with those in the replacement session.
#' Often, trials are repeated because of a failure. If `last_trial_exclude` is
#' set to 1, then the last trial from the previous session of the matched ppid
#' will be excluded. Afterwards, in case the exact same trial was repeated in
#' a row, only the first occurrence will be retained.
#'
#' @param df A trial results data frame.
#' @param replacements_df A data frame with columns
#'   `"ppid_old"`,`"ppid_new"`,`"session_num_new"`, `"last_trial_exclude"`

#'
#' @return
#' @export
#'
#' @examples
apply_replacements <- function(df, replacements_df) {

  new_replacements_df <- replacements_df %>%
    dplyr::mutate(previous_session_num = session_num_new - 1)

  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      session_num = with(
        replacements_df,
        ifelse(ppid %in% ppid_old,
               session_num_new[which(ppid_old == ppid)],
               session_num)
      ),
      ppid = with(replacements_df,
                  ifelse(ppid %in% ppid_old,
                         ppid_new[which(ppid_old == ppid)],
                         ppid))
    ) %>%
    # remove last trial from first session if indicated in replacements_df
    dplyr::ungroup() %>%
    dplyr::left_join(new_replacements_df,
                     by = join_by(ppid  == ppid_new,
                                  session_num == previous_session_num)) %>%
    dplyr::mutate(last_trial_exclude = if_else(is.na(last_trial_exclude),
                                               0, last_trial_exclude)) %>%
    dplyr::group_by(ppid, session_num) %>%
    dplyr::filter(!(last_trial_exclude == 1 &
                      trial_num == max(trial_num))) %>%
    dplyr::select(!tidyselect::any_of(colnames(new_replacements_df))) %>%
    # when two epochs appear in a row across sessions then only keep the first one
    dplyr::mutate(
      first_trial = min(trial_num), # first trial per session
      last_trial = max(trial_num),  # last trial per session
      previous_trial = dplyr::lag(trial_num), # preceding trial
      if_else(is.na(previous_trial), 0, previous_trial)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ppid, episode) %>%
    dplyr::filter(!(
      length(dplyr::cur_group_rows()) > 1 &
          length(unique(session_num)) > 1 &
          trial_num == first_trial &
          previous_trial == last_trial
    )) %>%
  dplyr::select(!c(first_trial, last_trial, previous_trial))

}

#' Reorient movement columns to starting position.
#'
#' Convenient wrapper around `reorient_movement_to_start`. For provided columns, reorients
#' movement dataframes contained in the provided columns.
#' Returns the data frame with new columns added.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing dataframes to reorient
#' (Default: waist tracker data).
#' @param ref_pos_col Reference position column (for example, generated with
#'  `find_start_pos`). Default: no reference, will default to a list of zeros.
#'
#' @return The data frame with a new columns of resampled movements added, with
#' new column name ending with `_reoriented`.
#' @export
#'
#' @examples
reorient_movement_cols_to_start <-
  function(df,
           .cols = tidyselect::ends_with("waist_movement_data_0"),
           ref_pos_col = c()) {
    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dplyr::across(
          {{ .cols }}, ~ reorient_movement_to_start(., ref_pos = {{ ref_pos_col }}),
          .names = "{paste0(.col, '_reoriented')}"
        ))
  }


#' Resample movement data columns
#'
#' Convenient wrapper around `resample_movement`. For provided columns, resamples
#' movement dataframes contained in the columns between times stored in
#' `start_resample_col` and `end_resample_col`. Resampling is done up to a max
#' duration of `max_duration` with a sample rate `sample_rate`. Non-existing
#' time points for any trial are filled with "NA". Position values will be reoriented
#' to reference position if given.
#' Returns the data frame with new columns added.
#'
#' To resample fruit collection and eyetracker data, see
#' `resample_fruit_task_cols` and `resample_eyetracker_cols`.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing dataframes to resample.
#' (Default: all movement data). Each data frame must have a column 'time'.
#' @param start_resample_col Column containing start timestamps.
#' @param end_resample_col Column containing end timestamps.
#' @param max_duration Maximum duration of the resample.
#' @param sample_rate New sample rate (Hz).
#' @param ref_pos_col Column containing reference position (e.g. fruit picking position), default
#' is relative to origin (i.e. just resample, no translation)
#'
#' @return The data frame with a new columns of resampled movements added, with
#' new column name ending with `_resampled`.
#' @export
#'
#' @examples
resample_movement_cols <-
  function(df,
           .cols = tidyselect::ends_with("_movement_data_0") & !(tidyselect::starts_with("valid")),
           start_resample_col,
           end_resample_col,
           max_duration,
           sample_rate = 10.0,
           ref_pos_col = c()) {

    new_time <- create_resampling_index(max_duration, sample_rate)

    df <- df %>%
      ungroup() %>%
      rowwise()

    if (ncol(select(df, {{ ref_pos_col }})) > 0) {
      df <- df %>%
        dplyr::mutate(
          dplyr::across(
            {{ .cols }}, ~ translate_movement_to_ref(., {{ ref_pos_col }})
          ))
    }

    df %>%
      dplyr::mutate(
        dplyr::across(
          {{ .cols }}, ~ resample_movement(.,
                                           new_time,
                                           {{ start_resample_col }},
                                           {{ end_resample_col }}),
          .names = "{paste0(.col, '_resampled')}"
        ))
  }

#' Resample fruit task data columns
#'
#' Convenient wrapper around `resample_fruit_task`. For provided columns, resamples
#' fruit task dataframes contained in the #' columns between times stored in
#' `start_resample_col` and `end_resample_col`. #' Resampling is done up to a max
#' duration of `max_duration` with a sample rate #' `sample_rate`. Non-existing
#' time points for any trial are filled with "NA".
#' Returns the data frame with new columns added.
#'
#' See `resample_movement_cols` for resampling movement or eyetracker data.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing dataframes to resample.
#' (Default: all fruit task data). Each data frame must have a column 'time'
#' and a column 'events'.
#' @param start_resample_col Column containing start timestamps.
#' @param end_resample_col Column containing end timestamps.
#' @param max_duration Maximum duration of the resample.
#' @param sample_rate New sample rate (Hz).
#'
#' @return The data frame with a new columns of resampled movements added, with
#' new column name ending with `_resampled`.
#' @export
#'
#' @examples
resample_fruit_task_cols <-
  function(df,
           .cols = tidyselect::contains("_fruittask0.csv_data"),
           start_resample_col,
           end_resample_col,
           max_duration,
           sample_rate = 10.0) {

    new_time <- create_resampling_index(max_duration, sample_rate)

    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dplyr::across(
          {{ .cols }}, ~ resample_fruit_task(.,
                                         new_time,
                                         {{ start_resample_col }},
                                         {{ end_resample_col }}),
            .names = "{paste0(.col, '_resampled')}"
        )
      )
  }

#' Resample eyetracker data columns
#'
#' Convenient wrapper around `resample_eyetracker`. For provided columns, resamples
#' eyetracker dataframes contained in the columns between times stored in
#' `start_resample_col` and `end_resample_col`. Resampling is done up to a max
#' duration of `max_duration` with a sample rate `sample_rate`. Non-existing
#' time points for any trial are filled with "NA".
#' Returns the data frame with new columns added.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing dataframes to resample.
#' (Default: all eyetracker data). Each data frame must have a column 'time'.
#' @param start_resample_col Column containing start timestamps.
#' @param end_resample_col Column containing end timestamps.
#' @param max_duration Maximum duration of the resample.
#' @param sample_rate New sample rate (Hz).
#'
#' @return The data frame with a new columns of resampled movements added, with
#' new column name ending with `_resampled`.
#' @export
#'
#' @examples
resample_eyetracker_cols <-
  function(df,
           .cols = tidyselect::ends_with("_eye_tracking_data_0"),
           start_resample_col,
           end_resample_col,
           max_duration,
           sample_rate = 10.0) {

    new_time <- create_resampling_index(max_duration, sample_rate)

    df %>%
      dplyr::mutate(
        dplyr::across(
          {{ .cols }}, ~ resample_eyetracker(.,
                                           new_time,
                                           {{ start_resample_col }},
                                           {{ end_resample_col }}),
          .names = "{paste0(.col, '_resampled')}"
        ))
  }

#' Average time series data columns within group
#'
#' Convenient wrapper around `average_timeseries`. For provided columns,
#' averages resampled timeseries dataframes contained. Returns a new data frame
#' with the averaged data and columns with grouping variables. No input checks
#' are performed, i.e. if input data are not resampled at the same time index then
#' no warning is thrown.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing timeseries dataframes to average
#' (Default: all resampled movement data). Each data frame must have a column
#' 'new_time'.
#' @param ... grouping variable(s), arguments separated with commas
#'
#' @return A new data frame with averaged timeseries data.
#' @export
#'
#' @examples
average_timeseries_cols <-
  function(df,
           .cols = tidyselect::ends_with("_movement_data_0_resampled"),
           ...)
  {
    df %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(across({{ .cols }},
      ~ average_timeseries(.),
      .names = "{ paste(.col) }"),
      .groups = "drop"
      )
  }


#' Summarise movement data columns to ensure unique time stamps
#'
#' Convenient wrapper around `summarise_movement`. For provided columns,
#' summarises movement data frames and returns the same data frame with unique
#' time stamp values and a new column `n` to indicate duplicated time stamps.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing movement dataframes
#' (Default: all movement data).
#' @param by Time stamp column, default is `time`
#'
#' @return The same data frame with old movement data columns replaced
#' @export
#'
#' @examples
summarise_movement_cols <-
  function(df,
           .cols = tidyselect::ends_with("_movement_data_0")
           & !(tidyselect::starts_with("valid")),
           by = "time") {
    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(
        {{ .cols }},
        ~ summarise_movement(.x, by)
        ))
  }

#' Check movement data columns
#'
#' Checks validity of movement tracker data by using `check_valid_movement` on all
#' movement data columns contained in the data frame. The reference column is
#' checked against the starting position; all other columns are checked against the
#' reference column.
#'
#' @param df A trial results data frame with movement data columns
#' @param start_pos A trial start position column (expects, per row, a list with elements `x` and `z`) (string)
#' @param ref_tracker A reference tracker column (string, default is "head_movement_data_0")
#'
#' @return The same dataframe, with new columns `valid_` which contains detailed
#'  information for each tracker (list), and a column `valid_trackers` that combines
#'  information from all trackers (Boolean)
#' @export
#'
#' @examples
check_tracker_cols <-
  function(df,
           start_pos,
           ref_tracker = "head_movement_data_0") {
    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(ref_tracker),
          ~ check_valid_movement(
            .,
            ref_pos = .data[[start_pos]],
            max_dist = 1,
            max_speed = 10
          ),
          .names = "valid_{.col}"
        ),
        dplyr::across(
          .cols = tidyselect::ends_with("movement_data_0") &
            !(tidyselect::starts_with("valid")) &
            !(tidyselect::any_of(ref_tracker)) &
            !(tidyselect::starts_with("threat")),
          ~  check_valid_movement(
            .,
            ref_df = .data[[ref_tracker]],
            max_dist = 2,
            max_speed = 10
          ),
          .names = "valid_{.col}"
        ),
        dplyr::across(
          .cols = tidyselect::starts_with("valid_"),
          ~ purrr::pluck(., "valid"),
          .names = "summary_{.col}"
        ),
        valid_trackers = all(unlist(across(tidyselect::starts_with("summary_valid_"))))
      ) %>%
      dplyr::select(!(tidyselect::starts_with("summary_valid_")))
  }




