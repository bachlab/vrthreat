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
#' @param ... any other arguments to be passed on to `add_orientation2target`
#' @return An average of the `target_ratio` or absolute target difference over
#'  time between `min_time` and `max_time`. Uses trapezium rule for average over
#'  variable sample rate.
#' @export
#'
#' @examples
extract_orientation <-
  function(ref_movement,
           target_position,
           min_time = 0,
           max_time = Inf,
           convert2cos = TRUE,
           ...) {

    if (!is.data.frame(ref_movement) ||
        nrow(ref_movement) == 0 ||
        is.null(target_position) ||
        is.na(min_time) ||
        is.na(max_time))
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
#' @param ... any other arguments to be passed on to `add_orientation2target2`
#' @return An average of the `target_ratio` or absolute target difference over
#'  time between `min_time` and `max_time`. Uses trapezium rule for average over
#'  variable sample rate.
#' @export
#'
#' @examples
extract_orientation2 <-
  function(ref_movement,
           target_movement,
           min_time = 0,
           max_time = Inf,
           convert2cos = TRUE,
           ...) {


    if (!is.data.frame(ref_movement) ||
        nrow(ref_movement) * nrow(target_movement) == 0  ||
        is.na(min_time) ||
        is.na(max_time))
      return(NA_real_) # make sure not empty

    DV <- ifelse(convert2cos, "target_ratio", "target_abs_diff")

    out_df <-
      add_orientation2target2(ref_movement, target_movement, ...) %>%
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

#' Extract ratio of target fixation between a `min_time` and `max_time`.
#' You can supply timestamps to `min_time` and `max_time` (e.g timestamp of
#' begin escape) to average over only a period, or use the entire duration of
#' the provided data frame.
#'
#' @param df Eye tracker data frame. Expected columns are `time` and `focus_object_raw`.
#' @param target Unity's internal target name (e.g. as returned by `find_unity_threat_name`)
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
           min_time = min(df$time),
           max_time = max(df$time)) {

    if (!is.data.frame(df) ||
        nrow(df) == 0  ||
        is.na(min_time) ||
        is.na(max_time))
      return(NA_real_) # make sure not empty

    episode_duration <- max_time - min_time


    df %>%
      # compute dt
      dplyr::mutate(dt = c(0, diff(.data[["time"]]))) %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      # find fixation events
      add_fixation2target(target) %>%
      # and summarise
      dplyr::mutate(weighted_fixation = fixation * dt) %>%
      dplyr::pull(weighted_fixation) %>%
      sum(na.rm = T) / episode_duration
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


    if (!is.data.frame(df) ||
        nrow(df) == 0  ||
        is.na(min_time) ||
        is.na(max_time))
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
#'
#' @return Peak or average 3-dimensional speed, between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_speed <-
  function(df,
           min_time = min(df$time),
           max_time = max(df$time),
           method = "mean",
           samplingrate = 10) {

    if (!is.data.frame(df) ||
        is.na(min_time) ||
        is.na(max_time) ||
        min_time > (max_time - 1 / samplingrate) ||
        nrow(df) == 0)
      return(NA_real_)

    # resample over entire data frame to avoid edge effects
    new_time <- create_resampling_index(max(df$time) - min(df$time), samplingrate)

    df %>%
      resample_filter_pos(new_time, span = 3) %>%
      dplyr::mutate(pos_x = new_pos_x,
                    pos_y = new_pos_y,
                    pos_z = new_pos_z) %>%
      # compute speed
      add_speed() %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      # and summarise
      {if (method == "max") {
        max(pull(., speed), na.rm = TRUE)
      } else {
        mean(pull(., speed), na.rm = TRUE)
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
           min_time = min(df$time),
           max_time = max(df$time),
           method = "mean",
           samplingrate = 10,
           direction = c(0, 0, 1)) {

    if (!is.data.frame(df) ||
        is.na(min_time)  ||
        is.na(max_time) ||
        min_time > (max_time - 1 / samplingrate) ||
        nrow(df) == 0  ||
        all(is.na(df[, 2])))
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
#' @return Average gaze elevation in radian, between `min_time` and
#'   `max_time`.
#' @export
#'
#' @examples
extract_gaze_elevation <-
  function(df,
           min_time = min(df$time),
           max_time = max(df$time),
           samplingrate = NULL,
           direction = c(0, 0, 1)) {

    if (is.null(samplingrate)) {
      samplingrate <-
        ifelse(("gaze_direction_x" %in% colnames(df)), 100, 10)
    }

    if (!is.data.frame(df) ||
        is.na(min_time)  ||
        is.na(max_time) ||
        min_time > (max_time - 1 / samplingrate) ||
        nrow(df) == 0 ||
        all(is.na(df[, 2])))
      return(NA_real_)

    prepare_gaze_data(df, samplingrate) %>%
      add_gaze_elevation(direction) %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      pull(gaze_elevation) %>%
      mean(na.rm = TRUE)

  }

#'  Extract (min, max or mean) 2D distance between movement trajectory and
#'  reference position.
#'
#'#' Searches a movement trajectory dataframe and summarises distance
#' (ignoring y) to a reference position as average or minimum.
#'
#' @param ref_movement A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param ref_position Reference position, stored as a list
#'   containing `"pos_x"`, `"pos_z"` items.
#' @param min_time Minimum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param method  whether to extract min (default), max or mean
#' @return A numeric value of the minimum 2D distance of the ref_movement
#'   trajectory to the supplied safe position.
#' @export
#'
#' @examples
extract_movement_dist <-
  function(ref_movement,
           ref_position,
           min_time = min(ref_movement$time),
           max_time = max(ref_movement$time),
           method = "min") {

    if (!is.data.frame(ref_movement) ||
        nrow(ref_movement) == 0 |
        is.null(ref_position)  ||
        is.na(min_time) ||
        is.na(max_time))
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
      dplyr::mutate(distance = calculate_2d_dist(pos_x,
                                                  pos_z,
                                                  ref_position$pos_x,
                                                  ref_position$pos_z)) %>%

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
            sum(na.rm = T) / episode_duration
        )
      }
  }


#' Extract (min, max or) 2D distance between two movement trajectories.
#'
#' Searches two dataframes of movement trajectories and summarises distance
#' (ignoring y) between the two trajectories as average or minimum.
#'
#' NOTE: because sampling times can differ between data frames, data are
#' resampled at default rate of 10 Hz, and to avoid an impact of tracker
#' glitches they are median-smoothed over 3 data points (300 ms).
#'
#' @param df1 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param df2 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param min_time Minimum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the ref_movement (taken from `"time"`
#'   column) to search.
#' @param method  whether to extract min (default), max or mean
#' @param samplingrate resampling rate
#'
#' @return A numeric value of the minimum 2D distance of the two movement trajectories.
#' @export
#'
#' @examples
extract_movement2_dist <-
  function(df1,
           df2,
           min_time = min(c(df1$time, df2$time)),
           max_time = max(c(df1$time, df2$time)),
           method = "min",
           samplingrate = 10) {

    if (!is.data.frame(df1) ||
        !is.data.frame(df2) ||
        is.na(min_time) ||
        is.na(max_time) ||
        min_time > (max_time - 1 / samplingrate) ||
        nrow(df1) * nrow(df2) == 0)
    return(NA_real_)



    # create joint resampling index
    start_time <- max(df1$time[1], df2$time[1])

    joint_resampling_index <- function(df,
                                       start_time,
                                       sr = samplingrate) {
      new_time <-
        create_resampling_index(max(df$time) - start_time,
                                samplingrate) - (min(df$time) - start_time)
      new_time[new_time > 0]
    }

    # preprocess and combine both data frames
    resample_filter_pos(df1,
                        joint_resampling_index(df1, start_time),
                        span = 3) %>%
      dplyr::inner_join(
        resample_filter_pos(df2,
                            joint_resampling_index(df2, start_time),
                            span = 3),
        by = "time",
        suffix = c(".1", ".2")
      ) %>%
      # remove values outside time range
      dplyr::filter(min_time <= time, time <= max_time) %>%
      # calculate moment-by-moment distance
      dplyr::mutate(distance = calculate_2d_dist(new_pos_x.1,
                                                  new_pos_z.1,
                                                  new_pos_x.2,
                                                  new_pos_z.2)) %>%
      # and summarise
      dplyr::pull(., distance) %>%
      {
        switch(
          method,
          min = min(., na.rm = TRUE),
          max = max(., na.rm = TRUE),
          mean = mean(., na.rm = TRUE)
        )
      }
  }


#' Extract duplicated time stamps.
#'
#' Extracts the number of duplicated time stamps in a summarised movement
#' trajectory between a `min_time` and `max_time`.
#' Returns the number of duplicated time stamps, total number of duplicates,
#' or maximum number of duplicates.
#'
#' @param df A summarised movement data frame (must contain a column `n`
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
           min_time = min(df$time),
           max_time = max(df$time),
           method = "max") {

    if(!is.data.frame(df) ||
       is.na(min_time) ||
       is.na(max_time))
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
