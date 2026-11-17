#' Extract direction of the orientation of a reference object relative to a
#' stationary target (in 2D, XZ) between a `min_time` and `max_time`.
#'
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period.
#'
#' @param ref_movement A reference movement data frame, e.g. the head or
#'   the waist movement. Movement file should have `pos_x`, `pos_y`, `pos_z`,
#'   `rot_x`, `rot_y`, `rot_z` columns.
#' @param target_position A list with `pos_x`, `pos_y`, and `pos_z` coordinates of the target
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#' @param convert2cos Whether to use the cosine of the angle (TRUE, default) or
#' the absolute angle (FALSE)
#' @param ... any other arguments to be passed on to [add_orientation2target()]
#' @return An average of the `target_ratio` or absolute target difference over
#'  time between `min_time` and `max_time`. Uses trapezium rule for average over
#'  variable sample rate.
#' @export
#'
#' @examples
#' sample_movement <- example_data$head_movement_data_0[[1]]
#' pos_list <- list(pos_x = 2, pos_y=1, pos_z=2)
#' 
#' extract_orientation(sample_movement, pos_list)
extract_orientation <-
  function(ref_movement,
           target_position,
           min_time = 0,
           max_time = Inf,
           convert2cos = TRUE,
           ...) {
    
    if (validate_skip_on_null(ref_movement, target_position) ||
        !validate_tibble_non_empty(ref_movement, .fun_name = "extract_orientation") ||
        !validate_has_rotation(ref_movement, "extract_orientation") ||
        !validate_has_position(ref_movement, "extract_orientation") ||
        !validate_has_time(ref_movement, .fun_name = "extract_orientation") ||
        !validate_positional_list(target_position, .fun_name = "extract_orientation") ||
        !validate_numeric(min_time, .fun_name = "extract_orientation") ||
        !validate_numeric(max_time, .fun_name = "extract_orientation"))
      return(NA_real_) # make sure not empty
    

    DV <- ifelse(convert2cos, "target_ratio", "target_abs_diff")

    out_df <-
      add_orientation2target(ref_movement, target_position, ...) %>%
      dplyr::filter(time >= min_time, time <= max_time) %>%
      dplyr::ungroup() %>%
      # catch out-of-bound values due to floating point imprecision
      dplyr::mutate(target_ratio = pmin(target_ratio, 1),
                    target_ratio = pmax(target_ratio, -1),
                    target_abs_diff = acos(target_ratio)) %>%
      dplyr::summarise(time_average = timeseries_mean(.data[[DV]], time))

    if (nrow(out_df) == 0) {
      out_df[1, 1] = NA_real_ # make sure always one row
    }

    out_df %>%
      dplyr::pull(time_average)
  }
#' Extract direction of the orientation of a reference object relative to a
#' target movement (in 2D, XZ) between a `min_time` and `max_time`.
#'
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period.
#'
#' @param ref_movement A reference movement data frame, e.g. the head or
#'   the waist movement. Movement file should have `pos_x`, `pos_y`, `pos_z`,
#'   `rot_x`, `rot_y`, `rot_z` columns.
#' @param target_movement Data frame of reference movement (e.g threat movement)
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#' @param convert2cos Whether to use the cosine of the angle (TRUE, default) or
#' the absolute angle (FALSE)
#' @param ... any other arguments to be passed on to [add_orientation2target2]
#' @return An average of the `target_ratio` or absolute target difference over
#'  time between `min_time` and `max_time`. Uses trapezium rule for average over
#'  variable sample rate.
#' @export
#'
#' @examples
#' sample_movement <- example_data$head_movement_data_0[[1]]
#' threat_movement <- example_data$threat_movement_data_0[[1]]
#' 
#' extract_orientation2(sample_movement, threat_movement)
extract_orientation2 <-
  function(ref_movement,
           target_movement,
           min_time = 0,
           max_time = Inf,
           convert2cos = TRUE,
           ...) {


    if (validate_skip_on_null(ref_movement, target_movement) || 
        !validate_tibble_non_empty(ref_movement, .fun_name = "extract_orientation2") ||
        !validate_has_position(ref_movement, "extract_orientation2") ||
        !validate_has_rotation(ref_movement, "extract_orientation2") ||
        !validate_has_time(ref_movement, .fun_name = "extract_orientation2") ||  
        !validate_has_position(target_movement, "extract_orientation2") ||
        !validate_has_rotation(target_movement, "extract_orientation2") ||
        !validate_has_time(target_movement, .fun_name = "extract_orientation2") ||  
        !validate_overlap(ref_movement, target_movement, .fun_name = "extract_orientation2") ||  
        !validate_numeric(min_time, .fun_name = "extract_orientation2") ||
        !validate_numeric(max_time, .fun_name = "extract_orientation2"))
      return(NA_real_) # make sure not empty

    DV <- ifelse(convert2cos, "target_ratio", "target_abs_diff")

    out_df <-
      add_orientation2target2(ref_movement, target_movement, ...) %>%
      dplyr::filter(time >= min_time, time <= max_time) %>%
      dplyr::ungroup() 
    
    if (nrow(out_df) == 0) return(NA_real_)
    
      out_df %>%
      # catch out-of-bound values due to floating point imprecision
      dplyr::mutate(target_ratio = pmin(target_ratio, 1),
                    target_ratio = pmax(target_ratio, -1),
                    target_abs_diff = acos(target_ratio)) %>%
      dplyr::summarise(time_average = timeseries_mean(.data[[DV]], time, na_handling = "discard")) %>%
      dplyr::pull(time_average)
  }

#' Extract ratio of target fixation between a `min_time` and `max_time`.
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period, or use the entire duration of
#' the provided data frame.
#'
#' @param df Eye tracker data frame. Expected columns are `time` and `focus_object_raw`.
#' @param target Unity's internal target name (e.g. as returned by [find_unity_threat_name]
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#'
#' @return Ratio of target fixations (0-1), between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_fixation <-
  function(df,
           target,
           min_time = min(df$time, na.rm = TRUE),
           max_time = max(df$time, na.rm = TRUE)) {

    if (validate_skip_on_null(df) ||
        !validate_tibble_non_empty(df, min_length = 2, .fun_name = "extract_fixation") ||
        !validate_has_time(df, .fun_name = "extract_fixation") ||
        !validate_time_interval(df, min_time = min_time, max_time = max_time) ||
        !validate_numeric(min_time, .fun_name = "extract_fixation") ||
        !validate_numeric(max_time, .fun_name = "extract_fixation"))
      return(NA_real_) # make sure not empty

    episode_duration <- max_time - min_time

    df <- df %>%
      dplyr::filter(time >= min_time, time <= max_time)
    
    if (!validate_tibble_non_empty(df, min_length = 2,.fun_name = "extract_fixation")) return(NA_real_) 
    
    fixation_duration <- df %>%
      extract_durations("focus_object_raw")  %>%
      dplyr::filter(!is.na(focus_object_raw)) %>%
      dplyr::filter(focus_object_raw==target) %>%
      dplyr::pull(duration)

    if(length(fixation_duration) == 0 || is.na(fixation_duration))
      fixation_duration <- 0

    fixation_duration/episode_duration
  }

#' Extract fruit picking between a `min_time` and `max_time`.
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period. In this case, you can also
#' retrieve the collection rate by setting `rate` to 1.
#'
#' NOTE: the fruit collection data frames contain no information on when
#' the trial ends; only on the last event of the task. Hence it is not possible
#' to extract the fruit collection rate without providing start and end time stamps.
#'
#' @param df A fruit collection data frame. Expected columns are `time` and `event`.
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#' @param method  whether to extract sum (default) or rate
#'
#' @return Sum of fruits or average of fruits picked per second, between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_fruit_task <-
  function(df,
           min_time = 0,
           max_time = Inf,
           method = "sum") {


    if (validate_skip_on_null(df) ||
        !validate_tibble_non_empty(df, .fun_name = "extract_fruit_task") ||
        !validate_has_time(df, .fun_name = "extract_fruit_task") ||
        !validate_numeric(min_time, .fun_name = "extract_fruit_task") ||
        !validate_numeric(max_time, .fun_name = "extract_fruit_task") ||
        !validate_option(method, "sum", "rate", .fun_name = "extract_fruit_task"))
      return(NA_real_) # make sure not empty

    episode_duration <- ifelse(method == "rate", max_time - min_time, 1)

    df %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      # find collection events
      dplyr::filter(event == "collect") %>%
      # count rows and divide by duration
      nrow() / episode_duration
  }


#' Extract speed (peak or average speed) between a `min_time` and `max_time`.
#'
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period. In both cases, you can either
#' retrieve peak or average (default) three-dimensional speed.
#'
#' NOTE: to avoid an impact of tracker glitches, data are resampled at default
#' rate of 10 Hz and median-smoothed over 3 data points (300 ms).
#'
#' @param df A movement data frame. Expected columns are `pos_x`, `pos_y`, `pos_z`
#' @param min_time minimum time in s (default: trial start)
#' @param max_time maximum time in s (default: trial end)
#' @param method  whether to extract mean (default) or max
#' @param samplingrate resampling rate
#' @param dimension "3D" (default) or "2D" (only horizontal speed)
#'
#' @return Peak or average 3-dimensional speed, between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_speed <-
  function(df,
           min_time = min(df$time, na.rm = TRUE),
           max_time = max(df$time, na.rm = TRUE),
           method = "mean",
           samplingrate = 10,
           dimension = "3D") {

    if (validate_skip_on_null(df) ||
        !validate_tibble_non_empty(df, min_length =  2, .fun_name = "extract_speed")||
        !validate_has_position(df, "extract_speed") ||
        !validate_has_time(df, min_length = 2, .fun_name = "extract_speed") ||
        !validate_numeric(min_time, .fun_name = "extract_speed") ||
        !validate_numeric(max_time, .fun_name = "extract_speed") ||
        !validate_option(method, "mean", "max", .fun_name = "extract_speed") ||
        min_time > (max_time - 1 / samplingrate))
      return(NA_real_)

    df %>%
      resample_filter_pos(sample_rate = samplingrate, span = 3) %>%
      dplyr::mutate(pos_x = new_pos_x,
                    pos_y = new_pos_y,
                    pos_z = new_pos_z) %>%
      # compute speed
      add_speed(dimension) %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      # and summarise
      {if (method == "max") {
        max(dplyr::pull(., speed), na.rm = TRUE)
      } else {
        mean(dplyr::pull(., speed), na.rm = TRUE)
      }}
  }

#' Extract angular speed (peak or average speed) for a direction vector
#' between a `min_time` and `max_time`.
#'
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period. In both cases, you can either
#' retrieve peak or average (default) angular speed.
#'
#' NOTE: to avoid an impact of tracker glitches, data are resampled at default
#' rate of 10 Hz (movement) or 100 Hz (gaze) and median-smoothed over 3 data points
#' (300 or 30 ms).
#'
#' @param df A movement or eyetracker data frame. Expected columns are either
#' `rot_x`, `rot_y`, `rot_z`, or `gaze_direction_x`, `gaze_direction_y`,
#' `gaze_direction_z`
#' @param min_time minimum time in s (default: trial start)
#' @param max_time maximum time in s (default: trial end)
#' @param method   whether to extract mean (default) or max
#' @param samplingrate resampling rate
#' @param direction Direction vector for which to compute the angular speed
#'
#' @return Peak or average 3-dimensional speed, between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_speed_angular <-
  function(df,
           min_time = min(df$time, na.rm = TRUE),
           max_time = max(df$time, na.rm = TRUE),
           method = "mean",
           samplingrate = 10,
           direction = c(0, 0, 1)) {

    if (validate_skip_on_null(df) ||
        !validate_tibble_non_empty(df, min_length = 2, check_na_columns = TRUE, .fun_name = "extract_speed_angular") ||
        !validate_has_time(df, .fun_name = "extract_speed_angular") ||
        !validate_has_gaze_or_rotation(df,"extract_speed_angular") ||
        !validate_numeric(min_time, .fun_name = "extract_speed_angular")  ||
        !validate_numeric(max_time, .fun_name = "extract_speed_angular") ||
        !validate_option(method, "mean", "max", .fun_name = "extract_speed_angular") ||
        min_time > (max_time - 1 / samplingrate))
      return(NA_real_)

    prepare_gaze_data(df, samplingrate) %>%
      add_angular_diff(direction = direction) %>%
      dplyr::mutate(angular_speed = samplingrate * angular_diff) %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      # and summarise
      {
        if (method == "max") {
          max(pull(., angular_speed), na.rm = TRUE)
        } else {
          mean(pull(., angular_speed), na.rm = TRUE)
        }
      }
  }


#' Extract net displacement between a `min_time` and `max_time`.
#' 
#' This function extracts the resulting displacement from a movement tibble, 
#' either in the horizontal plane or in 3D
#' 
#' NOTE: to avoid an impact of tracker glitches, data are resampled at default
#' rate of 10 Hz and median-smoothed over 3 data points (300 ms).
#'
#' @param df A movement data frame. Expected columns are `pos_x`, `pos_y`, `pos_z`
#' @param min_time minimum time in s (default: trial start)
#' @param max_time maximum time in s (default: trial end)
#' @param dimension "2D" (default, in horizontal plane only) or "3D" 
#'
#' @returns
#' @export
#'
#' @examples
extract_displacement <- function(df,
                                 min_time = min(df$time, na.rm = TRUE),
                                 max_time = max(df$time, na.rm = TRUE),
                                 dimension = "2D") {
  
  if (validate_skip_on_null(df) ||
      !validate_tibble_non_empty(df, min_length =  2, .fun_name = "extract_displacement")||
      !validate_has_position(df, "extract_displacement") ||
      !validate_has_time(df, min_length = 2, .fun_name = "extract_displacement") ||
      !validate_numeric(min_time, .fun_name = "extract_displacement") ||
      !validate_numeric(max_time, .fun_name = "extract_displacement") ||
      min_time > max_time)
    return(NA_real_)
  
  # initialise
  displacement <- NA_real_
  samplingrate <- 10
 
  # resample over entire data frame to avoid edge effects
  df <- 
    df %>%
    resample_filter_pos(sample_rate = samplingrate, span = 3) %>%
    dplyr::mutate(pos_x = new_pos_x,
                  pos_y = new_pos_y,
                  pos_z = new_pos_z) %>%
    # remove values outside time range
    dplyr::filter(min_time <= time, time <= max_time)
  
  # ensure resulting df has at least two rows
  if (nrow(df) == 0) return(NA_real_)
  if (nrow(df) == 1) return(0) # no displacement
  
  df <- 
    df %>%
    # get first and last row
    dplyr::slice(c(1, n())) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), diff))
  
  if (dimension == "2D") {
    displacement <- norm(c(df$pos_x, df$pos_z), "2")
  }
  if (dimension == "3D") {
    displacement <- norm(c(df$pos_x, df$pos_y, df$pos_z), "2")
  }
  
  return(displacement)
}

  
  
#' Extract gaze elevation between a `min_time` and `max_time`.
#'
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period.
#'
#' NOTE: to avoid an impact of tracker glitches, data are resampled at default
#' rate of 10 Hz (movement) or 100 Hz (gaze) and median-smoothed over 3 data points
#' (300 or 30 ms).
#'
#' @param df A movement or eyetracker data frame. Expected columns are either
#' `rot_x`, `rot_y`, `rot_z`, or `gaze_direction_x`, `gaze_direction_y`,
#' `gaze_direction_z`
#' @param min_time minimum time in s (default: trial start)
#' @param max_time maximum time in s (default: trial end)
#' @param samplingrate resampling rate
#' @param direction Gaze reference vector (default: forward)
#'
#' @return Average gaze elevation in degrees, between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_gaze_elevation <-
  function(df,
           min_time = min(df$time, na.rm = TRUE),
           max_time = max(df$time, na.rm = TRUE),
           samplingrate = NULL,
           direction = c(0, 0, 1)) {

    if (is.null(samplingrate)) {
      samplingrate <-
        ifelse(("gaze_direction_x" %in% colnames(df)), 100, 10)
    }

    if (validate_skip_on_null(df) ||
        !validate_tibble_non_empty(df, check_na_columns = TRUE, .fun_name = "extract_gaze_elevation") ||
        !validate_has_gaze_or_rotation(df, "extract_gaze_elevation") ||
        !validate_has_time(df, .fun_name = "extract_gaze_elevation") ||
        !validate_numeric(min_time, .fun_name = "extract_gaze_elevation")  ||
        !validate_numeric(max_time, .fun_name = "extract_gaze_elevation") ||
        min_time > (max_time - 1 / samplingrate))
      return(NA_real_)

    prepare_gaze_data(df, samplingrate) %>%
      add_gaze_elevation(direction) %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      pull(gaze_elevation) %>%
      mean(na.rm = TRUE)

  }

#' Extract pupil diameter statistics between a `min_time` and `max_time`
#' 
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period.
#' 
#' Data contains pupil diameters of the left and right eyes. So data to be used specified in
#' the `combination` parameter. Default is "best" and this utilizes the pupil measurements of the eye 
#' with fewer NA. Setting `combination` to "combined" causes it to take the average. 
#' "left" or "right" can be specified as well
#'  
#' In "combined" mode, if the proportion of NAs found in the left and right eyes differ by na_proportion_tolerance 
#' (Default is 0.1 i.e 10%) then a warning is generated and `combination` defaults to `best`
#'
#' @param df A movement or eyetracker data frame. Expected columns are either
#' `eye_openness_left`, `eye_openness_right`, `pupil_diameter_left` and `pupil_diameter_right`
#' @param min_time minimum time in s (default: trial start)
#' @param max_time maximum time in s (default: trial end)
#' @param method One of the strings "mean" "max" or "min". Specifies statistic (default is mean)
#' @param combination One of the strings "best", "combined", "left" or "right". (default is best)
#' @param openness_required Openness of eye required to include measurement. (default is 0.55)
#' @param na_proportion_tolerance Difference in NA proportions between left and right eye tolerated in combination = `combined`. 
#'
#' @returns A statistic (mean, min, max) from pupil diameter as specified in method. numeric
#' 
#' @export
#' @examples
extract_pupil <- 
  function(df,
           min_time = min(df$time),
           max_time = max(df$time),
           method = "mean",
           combination = "best",
           openness_required = 0.55,
           na_proportion_tolerance = 0.1){
    
  if(validate_skip_on_null(df, method, combination, openness_required) ||
     !validate_tibble_non_empty(df, .fun_name = "extract_pupil") ||
     !validate_has_time(df, .fun_name = "extract_pupil") ||
     !validate_option(method, "mean", "max", "min", .fun_name = "extract_pupil") ||
     !validate_option(combination, "best", "combined", "left", "right", .fun_name = "extract_pupil") ||
     !validate_numeric(openness_required, .fun_name = "extract_pupil") ||
     !validate_numeric(na_proportion_tolerance, .fun_name = "extract_pupil")){
    return(NA_real_) #make sure not empty
  }

  #filter out data and ensure -1 are NA. Also ensure minimum openness.
  pupil_data <- df %>%
    dplyr::filter(time >= min_time, time <= max_time) %>%
    dplyr::select(pupil_diameter_left, pupil_diameter_right, eye_openness_left, eye_openness_right) %>%
    dplyr::mutate(pupil_diameter_left = ifelse(eye_openness_left > openness_required, pupil_diameter_left, NA_real_)) %>%
    dplyr::mutate(pupil_diameter_right = ifelse(eye_openness_right > openness_required, pupil_diameter_right, NA_real_)) %>%
    dplyr::mutate(pupil_diameter_left = ifelse(pupil_diameter_left >= 0, pupil_diameter_left, NA_real_)) %>%
    dplyr::mutate(pupil_diameter_right = ifelse(pupil_diameter_right >= 0, pupil_diameter_right, NA_real_))
  
  
  #Default to "best" if "combined" inappropriate   
  if (combination %in% c("combined", "best")){
    NAs_left <- sum(is.na(pupil_data$pupil_diameter_left))
    NAs_right <- sum(is.na(pupil_data$pupil_diameter_right))
  }
  
  if(combination == "combined" && abs(NAs_left - NAs_right) > nrow(pupil_data)*na_proportion_tolerance){
    warning("Proportion of NA between left and right eye varies significantly. Defaulting to `best` data selection.")
    combination = "best"
  }
  
  #Compute relevant column (as eye_diameter)
  #combined
  if (combination == "combined") {
    pupil_data <- pupil_data %>%
      dplyr::filter(!is.na(pupil_data$pupil_diameter_left) | !is.na(pupil_data$pupil_diameter_right)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(pupil_diameter = mean(c(pupil_diameter_left, pupil_diameter_right), na.rm = TRUE)) %>%
      dplyr::ungroup() 
  #best
  } else if(combination == "best"){
    #choose eye with fewer NA
    side_selected <- ifelse(NAs_left > NAs_right, "right", "left")
    
    #save as pupil_diameter
    if(side_selected == "left"){
      pupil_data <- pupil_data %>%
        dplyr::mutate(pupil_diameter = pupil_diameter_left)
    } else {
      pupil_data <- pupil_data %>%
        dplyr::mutate(pupil_diameter = pupil_diameter_right)
    }
  #left
  } else if (combination == "left") {
    pupil_data <- pupil_data %>%
      dplyr::mutate(pupil_diameter = pupil_diameter_left)
  #right
  } else if (combination == "right") {
    pupil_data <- pupil_data %>%
      dplyr::mutate(pupil_diameter = pupil_diameter_right)
  }

  #output relevant summary
  switch(method,
    mean = pupil_data %>% dplyr::pull(pupil_diameter) %>% mean(., na.rm=TRUE),
    min = pupil_data %>% dplyr::pull(pupil_diameter) %>% min(., na.rm=TRUE),
    max = pupil_data %>% dplyr::pull(pupil_diameter) %>% max(., na.rm=TRUE))
  }

#'  Extract (min, max or mean) 2D distance between movement trajectory and
#'  reference position.
#'
#'#' Searches a movement trajectory dataframe and summarises distance
#' (ignoring y) to a reference position as average or minimum.
#' 
#' The "mse" option (Mean Squared Error) calculates the time weighted average of squared disance (Squared error) from 
#' the reference position.
#'
#' @param ref_movement A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param ref_position Reference position, stored as a list
#'   containing `"pos_x"`, `"pos_z"` items.
#' @param min_time Minimum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param method  whether to extract "min" (default), "max", "mean", "mse", or proportion of 
#'   time the distance is "less" or "greater" than a threshold
#' @param threshold numerical threshold if method is "less" or "greater", ignored otherwise   
#' @return A numeric value of the minimum 2D distance of the ref_movement
#'   trajectory to the supplied safe position.
#' @export
#'
#' @examples
extract_movement_dist <-
  function(ref_movement,
           ref_position,
           min_time = min(ref_movement$time, na.rm = TRUE),
           max_time = max(ref_movement$time, na.rm = TRUE),
           method = "min", 
           threshold = 0) {
    
    if (validate_skip_on_null(ref_movement, ref_position) ||
        !validate_tibble_non_empty(ref_movement, .fun_name = "extract_movement_dist")||
        !validate_positional_list(ref_position, drop_y = TRUE ,.fun_name = "extract_movement_dist") ||
        !validate_has_time(ref_movement, .fun_name = "extract_movement_dist") ||
        !validate_numeric(min_time, .fun_name = "extract_movement_dist") ||
        !validate_numeric(max_time, .fun_name = "extract_movement_dist") ||
        !validate_option(method, "min", "max", "mean", "mse", "less", "greater", .fun_name = "extract_movement_dist"))
      return(NA_real_) # make sure not empty

    
    episode_duration <- max_time - min_time

    ref_movement <-
      ref_movement %>%
      # compute dt
      dplyr::mutate(dt = c(0, diff(.data[["time"]]))) %>%
      # remove time points outside defined interval
      dplyr::filter(min_time <= time, time <= max_time)

    if (nrow(ref_movement) == 0)
      return(NA_real_)

    # calculate moment-by-moment distance
    ref_movement %>%
      add_dist(ref_position = ref_position) %>%

      # and summarise
      {
        switch(
          method,
          min = dplyr::pull(., distance) %>%
            min(na.rm = TRUE),
          max = dplyr::pull(., distance) %>%
            max(na.rm = TRUE),
          mean = dplyr::mutate(., weighted_distance = distance * dt) %>%
            dplyr::pull(weighted_distance) %>%
            sum(na.rm = T) / episode_duration,
          mse = dplyr::mutate(., weighted_distance = distance^2 * dt) %>%
            dplyr::pull(weighted_distance) %>%
            sum(na.rm = T) / episode_duration, 
          less = dplyr::mutate(., 
                               cond = distance < threshold,
                               prop = cond * dt) %>%
            dplyr::pull(prop) %>%
            sum(na.rm = T) / episode_duration,
          greater = dplyr::mutate(., 
                               cond = distance > threshold,
                               prop = cond * dt) %>%
            dplyr::pull(prop) %>%
            sum(na.rm = T) / episode_duration
        )
      }
  }


#' Extract (min, max or) 2D distance between two movement trajectories.
#'
#' Searches two dataframes of movement trajectories and summarises distance
#' (ignoring y) between the two trajectories as average or minimum.
#'
#' The "mse" option (Mean Squared Error) calculates the time weighted average of squared disance (Squared error) from 
#' the reference position.
#' 
#' NOTE: because sampling times can differ between data frames, data are
#' resampled at default rate of 10 Hz, and to avoid an impact of tracker
#' glitches they are median-smoothed over 3 data points (300 ms), using the function
#' [combine_movement]
#'
#' @param df1 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param df2 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param min_time Minimum time within the resampled movement (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the resampled movement (taken from `"time"`
#'   column) to search.
#' @param method  whether to extract "min" (default), "max", "mean", "mse", or proportion of 
#'   time the distance is "less" or "greater" than a threshold
#' @param threshold numerical threshold if method is "less" or "greater", ignored otherwise   
#' @param samplingrate resampling rate
#'
#' @return A numeric value of the minimum 2D distance of the two movement trajectories.
#' @export
#'
#' @examples
extract_movement2_dist <-
  function(df1,
           df2,
           min_time = min(c(df1$time, df2$time), na.rm = TRUE),
           max_time = max(c(df1$time, df2$time), na.rm = TRUE),
           method = "min", 
           samplingrate = 10,
           threshold = 0) {

    if (validate_skip_on_null(df1) ||
        validate_skip_on_null(df2) ||
        !validate_tibble_non_empty(df1, .fun_name = "extract_movement2_dist") ||
        !validate_tibble_non_empty(df2, .fun_name = "extract_movement2_dist") ||
        !validate_has_position(df1, .fun_name = "extract_movement2_dist") ||
        !validate_has_position(df2, .fun_name = "extract_movement2_dist") ||
        !validate_has_time(df1, .fun_name = "extract_movement2_dist") ||
        !validate_has_time(df2, .fun_name = "extract_movement2_dist") ||
        !validate_overlap(df1, df2, .fun_name = "extract_movement2_dist") ||
        !validate_numeric(min_time, .fun_name = "extract_movement2_dist") ||
        !validate_numeric(max_time, .fun_name = "extract_movement2_dist") ||
        !validate_option(method, "min", "max", "mean", "mse", "less", "greater", .fun_name = "extract_movement2_dist"))
      return(NA_real_)

    df <- df1 %>%
      add_dist2(df2,
       samplingrate = samplingrate) %>%
      dplyr::filter(min_time <= time, time <= max_time)
    
    if (nrow(df) == 0)
      return(NA_real_)

    episode_duration <- max(df$time) - min(df$time)
    
    # and summarise
    df %>%
      dplyr::mutate(dt = c(0, diff(.data[["time"]]))) %>%
      {
        # the evaluation of the expression "method" in switch looks awkward
        # (one might expect "min" instead of min)
        # but it is correct, see help(switch)
        # different from extract_movement_dist(), no weighting is needed because
        # df was resampled with equal spacing
        switch(
          method,
          min = dplyr::pull(., distance) %>%
            min(., na.rm = TRUE),
          max = dplyr::pull(., distance) %>%
            max(., na.rm = TRUE),
          mean = dplyr::pull(., distance) %>%
            mean(., na.rm = TRUE),
          mse = (dplyr::pull(., distance))^2 %>%
            mean(., na.rm = TRUE),
          less = dplyr::mutate(., 
                               cond = distance < threshold,
                               prop = cond * dt) %>%
              dplyr::pull(prop) %>%
              sum(na.rm = TRUE)/episode_duration,
          greater = dplyr::mutate(., 
                               cond = distance > threshold,
                               prop = cond * dt) %>%
              dplyr::pull(prop) %>%
              sum(na.rm = TRUE)/episode_duration
        )
      }
  }


#' Extract duplicated time stamps.
#'
#' Extracts the number of duplicated time stamps in a summarised movement
#' trajectory between a `min_time` and `max_time`. Use [summarise_movement()].
#' Duplicated timestamps indicate an aborted trial - not tracker error.  
#' 
#' Returns the number of duplicated time stamps, total number of duplicates,
#' or maximum number of duplicates.
#'
#' @param df A summarised movement data frame (must contain a column `n`)
#' @param min_time Minimum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param method  whether to extract maximum number of duplicates (`max`, default),
#' total number of duplicates (`total`), or the number of duplicated time stamps
#' (`number`)
#' @return Numeric, depending on `method`
#' @export
#'
#' @examples
extract_timestamp_duplicates <-
  function(df,
           min_time = min(df$time, na.rm = TRUE),
           max_time = max(df$time, na.rm = TRUE),
           method = "max") {

    if(validate_skip_on_null(df) ||
       !validate_tibble_non_empty(df, .fun_name = "extract_timestamp_duplicates") ||
       !validate_has_time(df, .fun_name = "extract_timestamp_duplicates") ||
       !validate_numeric(min_time, .fun_name = "extract_timestamp_duplicates") ||
       !validate_numeric(max_time, .fun_name = "extract_timestamp_duplicates")||
       !validate_option(method, "max", "total", "number", .fun_name = "extract_timestamp_duplicates"))
      return(NA_real_)

    df <-
      df %>%
      dplyr::filter(min_time <= time, time <= max_time)

    if (nrow(df) == 0)
      return(NA_real_)

    df <- df %>%
      dplyr::filter(n > 1)

    if (nrow(df) == 0)
      return(0)

      {
        switch(
          method,
          max = dplyr::pull(df, n) %>%
            max(),
          total = dplyr::pull(df, n) %>%
            sum(),
          number = nrow(df)
        )
      }
  }


#' Extract recording duration of a movement tibble
#'
#' Extracting the recording duration of a movement tibble can be useful to check
#' the synchronisation of different trackers and devices.
#'
#' @param df A movement tibble (must contain a column `time`)
#'
#' @returns Scalar duration, i.e. difference between last and first time stamp
#' @export
#'
#' @examples
extract_recording_duration <- function(df) {
  if(validate_skip_on_null(df) ||
     !validate_tibble_non_empty(df, .fun_name = "extract_recording_duration") ||
     !validate_has_time(df, .fun_name = "extract_recording_duration"))
    return(NA_real_)

  df %>%
    dplyr::pull(time) %>%
    range() %>%
    diff()

}

#' Extract 2D Path Length from Movement Data
#'
#' Calculates the total 2D path length from x- and z-coordinate movement data,
#' optionally within a specified time window. Assumes the data contains at least
#' two rows with columns `pos_x`, `pos_z`, and `time`.
#'
#' @param movement_data A data frame containing `pos_x`, `pos_z`, and `time` columns.
#' @param min_time Optional minimum time for filtering the movement data.
#' @param max_time Optional maximum time for filtering the movement data.
#'
#' @return A numeric value representing the total path length, or `NA_real_` if
#'   the input is invalid or insufficient.
#'
#' @examples
#' extract_path_length(movement_data)
#' extract_path_length(movement_data, min_time = 10, max_time = 50)
extract_path_length <- function(movement_data, min_time = NULL, max_time = NULL) {
  if (validate_skip_on_null(movement_data) ||
      !validate_tibble_non_empty(movement_data, min_length = 2, .fun_name = "extract_path_length") ||
      !validate_has_position(movement_data) ||
      !validate_has_time(movement_data))
    return(NA_real_)
  
  filtered_data <- movement_data
  
  if (!is.null(min_time)) {
    filtered_data <- dplyr::filter(filtered_data, time >= min_time)
  }
  if (!is.null(max_time)) {
    filtered_data <- dplyr::filter(filtered_data, time <= max_time)
  }
  
  if (nrow(filtered_data) < 2) {
    cat("Insufficient rows after filtering: must have at least two rows.\n Returning NA.\n")
    return(NA_real_)
  }
  
  filtered_data %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(next_pos_x = dplyr::lead(pos_x),
                  next_pos_z = dplyr::lead(pos_z)) %>%
    dplyr::filter(!is.na(next_pos_x) & !is.na(next_pos_z)) %>%
    dplyr::mutate(distance = sqrt((next_pos_x - pos_x)^2 + (next_pos_z - pos_z)^2)) %>%
    dplyr::pull(distance) %>%
    sum(na.rm = TRUE)
}


#' Extract a timemask from a dataframe for use in extract_with_timemask
#'
#' @param df A dataframe containing `time` and `label_col` columns.
#' @param ... <data-masking> Expressions that return a logical value, and are defined in terms of the variables in df. 
#' If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @returns a list with a tibble for intervals and the total ON duration.
#'
#' @seealso [extract_with_timemask] for using the timemask.
#' 
#' @export
#' @examples
#' 
#' # Data-masking syntax similar to dplyr::filter
#' sample <- example_data$head_movement_data_0[[1]]
#' extract_timemask(sample, pos_x^2 + pos_y^2 + pos_z^2 > 15)
#' 
#' # Programmatic use with Data-masking looks like
#' column_name <- "pos_x"
#' value <- mean(sample[[column_name]])
#' extract_timemask(sample, .data[[column_name]] > value)
#' 
extract_timemask <- function(df, ...){
  if(!validate_has_time(df, .fun_name = "extract_timemask"))
    return(NA_real_)

  # Compute tidyverse style condition setting 
  expressions <- rlang::enquos(...)
  data_mask <- rlang::as_data_mask(df)
  boolean_signal <- TRUE
  for(expression in expressions){
    boolean_signal <- boolean_signal & rlang::eval_tidy(expression, data_mask)
  }

  # Mask NA as false
  boolean_signal[is.na(boolean_signal)] <- FALSE

  # Compute extract_timemask
  run_lengths <- rle(boolean_signal) #run lengths of TRUE and FALSEs
  first_run_type <- run_lengths$values[1] #check if first run is FALSE
  if(first_run_type){ #drop first run if its a FALSE
    cumul_lengths <- cumsum(run_lengths$lengths)
    offset <- 0
  } else {
    offset <- run_lengths$lengths[1]
    cumul_lengths <- cumsum(run_lengths$lengths[-1]) #drop length of the first run
  }

  cumul_even_pos <- seq(0, length(cumul_lengths), 2)
  run_start_indexes <- c(1, 1 + cumul_lengths[cumul_even_pos]) + offset
  run_end_indexes <- cumul_lengths[-cumul_even_pos] + offset

  #cover edge case where -cumul_even_pos == 0 and run_end_indexes becomes numeric(0) as a result
  if(isTRUE(all.equal(cumul_even_pos, 0))){
    run_end_indexes <- cumul_lengths[1] + offset
  }

  #trim start_indexes in case last run is a FALSE one
  if(length(run_start_indexes) > length(run_end_indexes)){
    run_start_indexes <- run_start_indexes[-length(run_start_indexes)]
  }

  #Assume an interval runs from index i_1 to i_2, with time stamps t_1, t_2.
  #Also suppose that t_0 is the time stamp at (i_1 - 1), and t_3 is the time stamp at (i_2 + 1).
  #Then define the run interval as
  #   [(t_0 + t_1)/2, (t_2 + t_3)/ 2]
  #If t_0 or t_3 is out of bounds then let t_0 = t_1 or t_3 = t2 respectively
  start_time <- (df$time[run_start_indexes] + df$time[pmax(1, run_start_indexes - 1)])/2
  end_time <- (df$time[run_end_indexes] + df$time[pmin(nrow(df), run_end_indexes + 1)])/2

  intervals <- tibble::tibble(
    start_time = start_time,
    end_time =  end_time
  )

  total_duration <- sum(intervals$end_time - intervals$start_time)

  #Cleaner output in case no intervals matched
  if(is.na(total_duration)){
    intervals <- intervals[0, ]
    total_duration = 0
  }

  l <- list(total_on_duration=total_duration, intervals=intervals)
  class(l) <- "timemask"

  l
}

#' Wrap extract_* functions with a mask on time.
#' 
#' Wrapper that takes any existing extract_* function,
#' applies it to each contiguous ON-interval in the mask, and
#' combines the interval-wise results using a length-weighted mean.
#' 
#' The returned function captures all dataframes with a time column
#' applies the mask and then computes the statistic.
#' 
#' The `method` parameter can alternatively specify how the statistics are combined.
#' `mean` would take the time weighted average (default), `sum` sums up each statistic, `evaluate` simply returns the intervals tibble of the mask with a column of statistics evaluated.
#' `table` concatenates tables if the result of each call to extract_functon returned a tibble (like in [extract_durations]).
#'
#' @param extract_function The function to be called.
#' @param mask A timemask that specifies the ON intervals
#' @param method A string that specifies how the statistics are combined between calls. Options are `"mean"` (default), `"sum"`. `"evaluate"` and `"table"``.
#'
#' @returns A function that takes in the same arguments as `extract_function`
#' 
#' @seealso [extract_timemask()] for creating time masks
#'
#' @export
#' @examples
#' player <- example_data$head_movement_data_0[[7]]
#' threat <- example_data$threat_movement_data_0[[7]]
#' 
#' #Compute timemask where threat is moving at speed > 0.5 m/s
#' threat <- add_speed(threat)
#' mask <- extract_timemask(threat, speed > 0.5)
#' 
#' #Extract player speed in the same time. Warnings for erroneous intervals
#' extract_with_timemask(extract_speed, mask)(player)
extract_with_timemask <- function(extract_function, mask, method = "mean"){
  #save function name if possible
  func_expression <- rlang::enexpr(extract_function)
  if(rlang::is_symbol(func_expression)){
    func_name <- rlang::as_string(func_expression)
  } else {
    func_name <- ""
  }

  #Slightly detailed warning.
  if(!is.function(extract_function) && is.character(extract_function)){
    stop("`extract_function` parameter to extract_with_timemask not a function (strings not allowed).")
  } else if(!is.function(extract_function)){
    stop("`extract_function` parameter to extract_with_timemask not a function.")
  }

  #local function that captures tibbles to mask with.
  check_time_col <- function(df){
    if(is.data.frame(df) &&
       "time" %in% names(df) &&
       nrow(df) >= 1){
      return(TRUE)
    }
    FALSE
  }

  #Switch to min_time max_time interface of extract_* function if possible
  check_time_params <- FALSE
  if(is.function(extract_function)){
    check_min_time <- "min_time" %in% formalArgs(extract_function)
    check_max_time <- "max_time" %in% formalArgs(extract_function)
    check_time_params <- check_min_time && check_max_time
  }

  #Ensure function and mask is stored from environment
  force(extract_function)
  force(mask)
  force(method)
  # wrapped function that is returned
  function(...){
    if(!validate_timemask(mask, .fun_name = "extract_with_timemask")){
      if(method == "table")
        return(NULL)
      return(NA_real_)
    }

    all_args <- list(...)

    #save min_time and max_time parameters if end user needed them with the wrapped function
    min_time <- -Inf
    max_time <- Inf
    if("min_time" %in% names(all_args)){
      min_time <- all_args$min_time
    }
    if("max_time" %in% names(all_args)){
      max_time <- all_args$max_time
    }

    #compute across each interval
    total_time <- 0
    result <- 0
    for(i in 1:nrow(mask$intervals)){
      if(check_time_params){
        local_args <- all_args
        local_args$min_time <- max(min_time, mask$intervals$start_time[i])
        local_args$max_time <- min(max_time, mask$intervals$end_time[i])
        computed_statistic <- do.call(extract_function, local_args) #utilize min_time max_time interface

        end_time <- local_args$max_time
        start_time <- local_args$min_time
      } else {
        start_time <- mask$intervals$start_time[i]
        end_time <- mask$intervals$end_time[i]
        filtered_args <- purrr::map_if(all_args, check_time_col, ~ dplyr::filter(.x, time > start_time, time < end_time))
        computed_statistic <- do.call(extract_function, filtered_args)
      }
      
      if(any(is.na(computed_statistic))){
        if(func_name != ""){
          warning(stringr::str_glue("NA statistic computed for interval [{start_time}, {end_time}] by {func_name}"))
        } else {
          warning(stringr::str_glue("NA statistic computed for interval [{start_time}, {end_time}]"))
        }
        if(method != "evaluate")
          next; 
      }
      
      #compute total time
      total_time <- total_time + end_time - start_time
      
      #combination method table, result will be a tibble
      if(method == "table"){
        computed_statistic$mask_index = i
        if(i == 1){
          result <- computed_statistic
        } else {
          result <- dplyr::bind_rows(result, computed_statistic)
        }
      }

      if(method == "evaluate"){
        if(i == 1)
          result <- list()
        if(is.data.frame(computed_statistic))
          computed_statistic <- list(computed_statistic)
        result[length(result) + 1] <- computed_statistic
      }

      if(method == "sum"){
        result <- result + computed_statistic
      }

      if(method == "mean"){
        result <- result + (end_time - start_time)*computed_statistic 
      }
    }

    if(total_time == 0){
      if(method == "table")
        return(NULL)
      return(NA_real_)
    }
    
    if(method == "evaluate"){
      if(!is.data.frame(result[1]))
        result <- unlist(result)
      result <- dplyr::mutate(mask$intervals, !!func_name := result)
    }

    if(method == "mean"){
      result <- result/total_time
    }

    result
  }
}


#' Extract number of time bins in a movement tibble
#' 
#' this function simply returns the number of rows in a movement tibble between
#' `min_time` and `max_time`
#'
#' @param df A movement tibble with column `time` 
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#' 
#' @returns An integer number
#' @export
#'
#' @examples
extract_time_bins <- function(df,
                              min_time = 0,
                              max_time = Inf) {
  if (!is.data.frame(df) ||
      is.na(min_time) ||
      is.na(max_time))
    return(NA_real_) # make sure not empty
  
  if (nrow(df) == 0) return(0)
  
  df %>%
    dplyr::filter(time >= min_time, time <= max_time) %>%
    nrow()
}

#' Extract durations of discrete values in a movement tibble
#' 
#' Extracts the overall duration during which an `inner_col` in a movement or 
#' eye tracker tibble took its unique values. This generalises the notion of a 
#' frequency table to  account for potentially uneven sampling and can be handy 
#' for example to extract fixation durations from an eye tracker tibble. This 
#' function works differently from other `extract_...` function in that it 
#' returns a tibble, not a scalar value (and `NULL` rather than `NA_real_` in 
#' case of invalid input)
#'
#' Each row in which a value is observed adds to the duration of that value 
#' half the time interval to the previous timestamp and half the time interval 
#' to the next timestamp.
#'
#' @param df Any tibble with columns `time` and `inner_col`
#' @param inner_col Any column in `df` (character)
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#'
#' @returns The table of durations for each value in the `inner_col` in descending order
#' @export
#'
#' @examples
extract_durations   <- function(df, 
                                inner_col,
                                min_time = 0,
                                max_time = Inf) {  
  if (validate_skip_on_null(df) ||
      !validate_tibble_non_empty(df, min_length = 2, .fun_name = "extract_durations") ||
      !validate_has_time(df, .fun_name = "extract_durations") ||
      !validate_numeric(min_time, .fun_name = "extract_durations") ||
      !validate_numeric(max_time, .fun_name = "extract_durations") ||
      !(inner_col %in% colnames(df))
  )
    return(NULL) # make sure not empty

  # Compute Frame Durations
  df <- df %>%
    dplyr::ungroup() %>% # necessary to prevent all NA forming a bug
    dplyr::mutate(last_time = dplyr::lag(time),
                  last_time = dplyr::if_else(is.na(last_time), min(time), last_time),
                  next_time = dplyr::lead(time),
                  next_time = dplyr::if_else(is.na(next_time), max(time), next_time),
                  frame_duration = ((time+next_time)/2-(time+last_time)/2))

  # Filter to time interval of interest
  df <-  df %>%
    dplyr::filter(time >= min_time, time <= max_time)
  if (nrow(df) == 0) return(NULL)
    
  # Return summarised Durations
  df %>%
    dplyr::group_by(.data[[inner_col]]) %>%
    dplyr::summarise(duration = sum(frame_duration)) %>%
    dplyr::arrange(dplyr::desc(duration))
}

#' Extracts the most frequent label in a time series tibble.
#' 
#' [extract_durations] is used to return the label that was active for the longest duration.
#' Consider using with the 'evaluate' option in [extract_with_timemask].   
#'
#' @param df Any tibble with columns `time` and `label_col`
#' @param label_col Any column in `df` (character)
#' @param min_time minimum time in s (default -Inf)
#' @param max_time maximum time in s (default Inf)
#'
#' @returns The most frequent label string (or object) in `label_col` 
#' @export
#'
#' @examples
#' sample <- example_data$head_movement_data_0[[1]]
#' mask <- extract_timemask(example_data$head_movement_data_0[[1]], pos_x > 1.4)
#' 
#' action_tibble <- example_data$action[[1]]
#' extract_with_timemask(extract_frequent_label, mask, method = "evaluate")(action_tibble, "Coarse")
extract_frequent_label <- function(df, label_col, min_time = -Inf, max_time = Inf) {
  if (!validate_numeric(min_time, .fun_name = "extract_durations") ||
      !validate_numeric(max_time, .fun_name = "extract_durations"))
    return(NA_character_) # input handling
      
  df <- dplyr::filter(df, time >= min_time, time <= max_time)

  if (validate_skip_on_null(df) ||
      !validate_tibble_non_empty(df, min_length = 2, .fun_name = "extract_durations") ||
      !validate_has_time(df, .fun_name = "extract_durations") ||
      !(label_col %in% colnames(df)))
    return(NA_character_) # make sure not empty
  
  
  (extract_durations(df, label_col) %>% dplyr::pull(label_col))[[1]] 
}

#' Extract mean direction between a `min_time` and `max_time` as unit vector.
#'
#' This function works differently from other `extract_...` functions in that it 
#' returns a direction list with components `dir_x`, `dir_y`, `dir_z`, not a 
#' scalar value, and `NULL` if the input is invalid.
#' 
#' @param df Movement data frame. Expects columns `pos_x`, `pos_y`, `pos_z`, `time`.
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#' 
#' @returns
#' @export
#'
#' @examples
extract_direction <-
  function(df,
           min_time = min(df$time, na.rm = TRUE),
           max_time = max(df$time, na.rm = TRUE)) {
    
    if (validate_skip_on_null(df) ||
        !validate_tibble_non_empty(df, min_length =  2, .fun_name = "extract_direction")||
        !validate_has_position(df, "extract_direction") ||
        !validate_has_time(df, min_length = 2, .fun_name = "extract_direction") ||
        !validate_numeric(min_time, .fun_name = "extract_direction") ||
        !validate_numeric(max_time, .fun_name = "extract_direction"))
      return(NULL)
    
   
      # compute direction
      df <- 
        df %>% 
        add_direction() %>%
        # remove values outside time range
        dplyr::filter(min_time <= time, time <= max_time) 
      
      # and summarise
      out <- list(dir_x = mean(pull(df, "dir_x"), na.rm = TRUE),
           dir_y = mean(pull(df, "dir_y"), na.rm = TRUE),
           dir_z = mean(pull(df, "dir_z"), na.rm = TRUE))
      
      # and normalise
      mag <- sqrt(sum((c(out$dir_x, out$dir_y, out$dir_z))^2))
      
    if(!is.na(mag) && mag != 0){
      out <- lapply(out, `/`, mag)
      } else {
      out <- list(dir_x = 0, dir_y = 0, dir_z = 0)    
      }

    return(out)
  }


#' Extract value of inner column at specified time
#' 
#' Extracts the the value of an `inner_col` in a movement or eye tracker tibble
#' at a specified time. Returns the original variable type of the `inner_col`. 
#'
#' @param df Any tibble with columns `time` and `inner_col`
#' @param inner_col Any column in `df` (character)
#' @param time A time stamp on the same scale as the `time` column
#' @param tolerance Generate warnings if closest timestamp further than this tolerance (default 0.1s)
#'
#' @returns
#' @export
#'
#' @examples
extract_at_time   <- function(df, 
                                inner_col,
                                time,
                                tolerance = .1) {
  
  if (validate_skip_on_null(df) ||
      !validate_tibble_non_empty(df, min_length = 2, .fun_name = "extract_at_time") ||
      !validate_has_time(df, .fun_name = "extract_at_time") ||
      !(inner_col %in% colnames(df))
  )
    return(NA) # make sure not empty
  
  indx <-find_argmin(abs(df$time - time))
  max_diff <- abs(df$time[indx] - time)
  if (max_diff > tolerance) warning("Time stamp mismatch ", max_diff, "s of more than ", tolerance,"s seconds.")
  
  df[[inner_col]][indx]
  
}

#' Extract at specific distance from a reference position, using any extract function
#'
#' @param movement_df Movement df used to determine the distance
#' @param extract_df df to extract from
#' @param ref_position Reference position (position list or movement df)
#' @param ref_distance Maximum distance in the same units as the movement df and reference position)
#' @param extract_function Extract function to use
#' @param method How to summarise the results across occurrences: `"mean"`, `"sum"`, `"table"`
#' @param ... Additional arguments passed to the extract function
#'
#' @returns
#' @export
#'
#' @examples
extract_at_distance <- function(movement_df, extract_df, ref_position, ref_distance, extract_function, method = "mean", ...) {

      if (validate_skip_on_null(movement_df) ||
      !validate_tibble_non_empty(movement_df, min_length = 1, .fun_name = "extract_at_distance") ||
      !validate_has_time(movement_df, .fun_name = "extract_at_distance") ||
      !validate_has_time(extract_df, .fun_name = "extract_at_distance") ||
      !validate_has_position(movement_df, "extract_at_distance") ||
      !validate_has_position(ref_position, "extract_at_distance") ||
      (is_tibble(ref_position) && !validate_tibble_non_empty(ref_position, min_length = 1, .fun_name = "extract_at_distance"))
      )
    
    return(NULL) # make sure not empty
  
  if (is_tibble(ref_position)) {
  movement_df <- 
    movement_df %>%
    add_dist2(df2 = ref_position)
  } else {
    movement_df <- 
      movement_df %>%
      add_dist(ref_position = ref_position)
  }
  
  # for dist2, there is no guarantee that the two dfs have overlapping time points,
  # so check again if empty
  
  if (!validate_tibble_non_empty(movement_df, min_length = 1, .fun_name = "extract_at_distance")) {
    return(NULL)
  }
  
  timemask <- movement_df %>%
    extract_timemask(distance < ref_distance) 
  
  extract_with_timemask(extract_function, timemask, method)(extract_df, ...)
  
  
}
  

#' Extract value sequence of inner column between a `min_time` and `max_time`.
#' 
#' This function works differently from other `extract_...` functions in that it 
#' returns a vector of the same type as the `inner_col`, not a scalar value.
#'
#' @param df Any tibble with columns `time` and `inner_col`
#' @param inner_col Any column in `df` (character)
#' @param min_time minimum time in s (default trial start)
#' @param max_time maximum time in s (default trial end)
#'
#' @returns
#' @export
#'
#' @examples
extract_sequence   <- function(df, 
                              inner_col,
                              min_time = 0,
                              max_time = Inf) {
  
  if (validate_skip_on_null(df) ||
      !validate_tibble_non_empty(df, min_length = 2, .fun_name = "extract_sequence") ||
      !validate_has_time(df, .fun_name = "extract_sequence") ||
      !validate_numeric(min_time, .fun_name = "extract_sequence") ||
      !validate_numeric(max_time, .fun_name = "extract_sequence") ||
      !(inner_col %in% colnames(df))
  )
    return(NA) # make sure not empty
  
  df <-  df %>%
    dplyr::filter(time >= min_time, time <= max_time)
  
  if (nrow(df) == 0) return(NA)
  
  df[[inner_col]]
  
}