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

#' Find all unique labels across trials 
#' 
#' Finds all unique labels within nested tibbles across a trial_results tibble 
#'
#' @param df trial_results tribble
#' @param outer_col String name of the column containing the action sequence tibbles
#' @param inner_col String name of the column in the action sequence tibble that contains the actions
#'
#' @returns
#' @export
#'
#' @examples
find_all_labels <- function(df, outer_col, inner_col) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::pull(outer_col) %>%
    dplyr::bind_rows() %>%
    dplyr::pull(inner_col) %>%
    unique()
}


#' Get distribution of labels or similar categorical data across a trial results dataframe.
#'
#' Takes a trial results dataframe, extracts durations of each unique label within nested tibbles
#' and returns the distribution of durations per label and proportion of total time. 
#' 
#' If the trial results dataframe is grouped then it returns a tibble with the
#' distributions nested in `distributions` column per group. 
#' 
#' @param df A trial results df
#' @param outer_col The column containing nested tibbles. (string)
#' @param inner_col The column within a nested tibble
#'
#' @returns
#' @export
#'
#' @examples
get_label_distribution <- function(df, outer_col, inner_col) {
  #Save grouping data
  group_names <- dplyr::group_vars(df)

  #Compute Durations
  df <- df %>%
  dplyr::rowwise() %>%
  dplyr::mutate(dist_durations := list(extract_durations(.data[[outer_col]], inner_col)))

  #Regroup data
  df <- dplyr::grouped_df(df, group_names)
  
  #Helper function
  combine_durations <- function(df){
    df %>%
      dplyr::group_by(.data[[inner_col]]) %>%
      dplyr::summarise(duration = sum(duration, na.rm = TRUE)) %>%
      dplyr::mutate(proportion = duration/sum(duration)) %>%
      dplyr::arrange(dplyr::desc(duration)) 
  }
  
  #Final Computation
  distributions <- df %>% 
    dplyr::summarise(
      dist_durations = list(dplyr::bind_rows(dist_durations)),
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(distribution = list(combine_durations(dist_durations))) %>%
    dplyr::select(-dist_durations) %>%
    dplyr::ungroup()

  if(length(group_names) == 0)
    return(distributions$distribution[[1]])
  else
    return(distributions)
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
#' a row in the last trial of the aborted session and the first trial of the new 
#' session, only the first occurrence can be retained if `remove_duplicates` is set.
#'
#' @param df A trial results data frame.
#' @param replacements_df A data frame with columns
#'   `"ppid_old"`,`"ppid_new"`,`"session_num_new"`, `"last_trial_exclude"`
#' @param remove_duplicates Whether or not to remove consecutive duplicate trials 
#'   (default: `TRUE`).     

#'
#' @return
#' @export
#'
#' @examples
apply_replacements <- function(df, replacements_df, remove_duplicates = TRUE) {

  new_replacements_df <- replacements_df %>%
    dplyr::mutate(previous_session_num = session_num_new - 1)

  df <- 
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
    dplyr::select(!tidyselect::any_of(colnames(new_replacements_df))) 
  
  if (remove_duplicates) {
    df <- 
      df %>%
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
  
  df 
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

#' Reorient movement data columns by affine transformation
#'
#' This is a wrapper function for `reorient_movement`. See there for details.
#' Applies the same affine transformation to selected movement data columns
#' within each row of a trial results data frame.
#'
#' If `scenario_col` is provided, the corresponding scenario data are passed to
#' `reorient_movement` for each row. This can be used to apply or undo scenario
#' position and rotation transformations.
#'
#' @param df A trial_results data frame.
#' @param .cols A tidy selection of columns to work on. Default: all movement data.
#' @param R A 3 x 3 rotation matrix or reference to column containing rotation matrices Default: no rotation.
#' @param offset A 3-element translation vector in rotated coordinates
#'   Default: no translation.
#' @param scenario_col A column containing scenario data. Default: `NULL`.
#' @param scenario_direction Either `"apply"` or `"undo"`. Default: `"apply"`.
#'
#' @returns The trial_results tibble with all selected movement columns
#'   reoriented.
#' @export
#'
#' @examples
reorient_movement_cols <- function(df,
                                   .cols = tidyselect::ends_with("_movement_data_0") & !(tidyselect::starts_with("valid")),
                                   R = diag(3),
                                   offset = c(0, 0, 0),
                                   scenario_col = NULL,
                                   scenario_direction = "apply") {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(
      {{ .cols }},
      ~ list(reorient_movement(.x, 
                               R = if(is.character(R)) .data[[R]] else R,
                               offset = if(is.character(offset)) .data[[offset]] else offset,
                               scenario_data = if (!is.null(scenario_col)) .data[[scenario_col]],
                               scenario_direction = scenario_direction))))
}


#' Reorient movement data columns from Qualisys into Unity
#' 
#' This is a wrapper function for `reorient_qualisys2unity`. See there for details.
#'   
#' Provide the offset as a vector in Unity coordinates (FROM Unity Scenario origin TO Qualisys origin).
#' The function can also apply the rotation to local scenario coordinates provided in a scenario data object.
#' This would ensure the returned data is in global coordinates.
#' 
#' It is recommended to include scenario data as all data stored via UXF from unity VR is in global coordinates.
#' 
#' @param df a trial_results data frame
#' @param .cols A tidy selection of columns to work on. Default: `all_of("mocap")`.
#' @param offset A 3-element translation vector in Unity coordinates (from Unity Scenario Origin towards Qualisys Mocap origin)
#' @param scenario_col A column containing scenario data (Default:NULL)
#'
#' @returns The trial_results tibble with all position columns reoriented 
reorient_qualisys2unity_cols <- function(df,
                                         .cols = tidyselect::all_of("mocap"),
                                         offset = c(0,0,0),
                                         scenario_col = NULL) {
  df %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(
      {{ .cols }},
      ~ list(reorient_qualisys2unity(.x, offset = offset,
            if (!is.null(scenario_col)) .data[[scenario_col]]))
    ))
}



#' Translate movement cols as per reference column
#' 
#' Convenient wrapper around [reorient_movement]
#' 
#' Non-existing time points for any trial are filled with "NA". Position values will be reoriented
#' to reference position if given.
#' 
#' @param df A trial results data frame.
#' @param .cols Columns containing dataframes to resample.
#' (Default: all movement data). Each data frame must have a column 'time'.
#' @param ref_pos_col Column containing reference position (e.g. fruit picking position)
#'
#' @return The data frame with movement data offset and new columns with resampling, with
#' new column name ending with `_resampled`.
#' @export
#'
#' @examples
translate_ref_movement_cols <- function(df,
                                        .cols = tidyselect::ends_with("_movement_data_0") & !(tidyselect::starts_with("valid")),
                                        ref_pos_col = c()){
  # Translate to reference position
  if (ncol(dplyr::select(df, {{ ref_pos_col }})) > 0) {
    df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        {{ .cols }},
        ~ list(reorient_movement(.x, offset = lapply(.data[[ref_pos_col]], `-`)))
      )
    ) 
  }

  df
}


#' Resample and relocate movement data columns
#'
#' Convenient wrapper around [resample_movement]
#' 
#' Non-existing time points for any trial are filled with "NA". Position values will be reoriented
#' to reference position if given.
#' 
#' Returns the data frame with new columns added.
#'
#' To resample fruit collection and eyetracker data, see
#' `resample_fruit_task_cols` and `resample_eyetracker_cols`.
#'
#' @param df A trial results data frame.
#' @param .cols Columns containing dataframes to resample.
#' (Default: all movement data). Each data frame must have a column 'time'.
#' @param sample_rate New sample rate (Hz) (default 10).
#' @param ref_pos_col Column containing reference position (e.g. fruit picking position), default
#' is relative to origin (i.e. just resample, no translation)
#'
#' @return The data frame with movement data offset and new columns with resampling, with
#' new column name ending with `_resampled`.
#' @export
#'
#' @examples
resample_movement_cols <-function(df,
                                  .cols = tidyselect::ends_with("_movement_data_0") & !(tidyselect::starts_with("valid")),
                                  sample_rate = NULL) {    
  # Resample if resample rate given
  if(!is.null(sample_rate) && !is.na(sample_rate)){
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        {{ .cols }}, ~ list(resample_movement(.,
                                          sample_rate = sample_rate)),
        .names = "{paste0(.col, '_resampled')}"
      ))
  }
    
  df
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
#' (Default: all movement and eye tracking data).
#' @param by Time stamp column, default is `time`
#'
#' @return The same data frame with old movement data columns replaced
#' @export
#'
#' @examples
summarise_movement_cols <-
  function(df,
           .cols = (tidyselect::ends_with("_movement_data_0") | 
                      tidyselect::ends_with("_eye_tracking_data_0"))
           & !(tidyselect::starts_with("valid")),
           by = "time") {
    df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(
        {{ .cols }},
        ~ summarise_movement(.x, by)
        ))
  }

#' Correct movement timing across columns
#' 
#' Convenient wrapper around `correct_movement_timing`. For provided columns,
#' corrects the `time` column in nested movement data frames by a given offset
#'
#'
#' @param df A trial results data frame
#' @param .cols A tidy selection of columns
#' @param offset_col A column name containing the offset for each row (string)
#'
#' @returns
#' @export
#'
#' @examples
correct_movement_timing_cols <- function(df, .cols, offset_col) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across({{ .cols }}, 
                    ~ list(correct_movement_timing(.x, .data[[offset_col]])),
                    .names = "{.col}_corrected"
      )
    )
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
