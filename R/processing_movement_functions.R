#' Get time of first fruit collection
#'
#' Gets the time stamp of the first collected fruit from a fruit collection data frame
#'
#' @param df Fruit collection data frame, expected columns: `time`, `event`
#'
#' @return a time stamp
#' @export
#'
#' @examples
get_first_fruit_collection <- function(df) {
  if (!is.data.frame(df) ||
      nrow(df) == 0)
    return(NA_real_)

  df %>%
    filter(event == "collect") %>%
    {
      if (nrow(.) == 0)
        NA_real_
      else {
        slice(., 1) %>%
          pull(time)
      }
    }
}


#' Guess/interpolate position at a point in time
#'
#' @param df a data frame
#' @param ref_time a reference time
#'
#' @return a list with elements `pos_x`, `pos_y`, `pos_z`
#' @export
#'
#' @examples
guess_pos_at_time <- function(df, ref_time) {
  if (!is.data.frame(df)   ||
      nrow(df) == 0 ||
      ref_time < min(df$time) ||
      ref_time > max(df$time))
    return(NA_real_)

  new_time <- sort(unique(c(df$time, ref_time)))

  df %>%
    resample_movement(new_time) %>%
    pluck(1) %>%
    dplyr::filter(dplyr::near(time, ref_time)) %>%
    # in case two time stamps within machine precision
    slice(1) %>%
    with(list(pos_x = pos_x, pos_y = pos_y, pos_z = pos_z))
}

#' Guess First Move From Position
#'
#' Guess the time at which the player initially moved away from a fixed position
#'
#' @param pos A list containing `x` `y` `z` of fixed position.
#' @param ref_movement A data frame with reference movement, e.g. the head or
#'   the waist movement. Movement file should have `pos_x`, `pos_y`, `pos_z`
#'   columns.
#' @param max_dist Maximum distance in meters before player is
#'   classified as away from the position
#' @min_time Minimum time to look for move (default: start of data frame)
#' @max_time Maximum time to look for move (default: end of data frame)
#'
#' @return A time stamp (Unity time) representing the time when the participant
#'   initially moved from the position.
#' @export
#'
#' @examples
guess_move_from_pos <-
  function(pos,
           ref_movement,
           max_dist = 0.2,
           min_time = min(ref_movement$time),
           max_time = max(ref_movement$time)) {
    if (nrow(ref_movement) == 0)
      return(NA_real_)
    if (is.null(pos))
      return(NA_real_)

    temp_tbl <- ref_movement %>%
      filter(time > min_time & time < max_time) %>%
      dplyr::rowwise() %>%
      # this could be made faster with rowSums, but would need to catch the case that
      # df has only one row
      dplyr::mutate(dist = norm(c(pos_x - pos$pos_x, 0, pos_z - pos$pos_z), type = "2"),
                    near = dist < max_dist)

    # get "runs" of when we were near or not
    runs <- with(temp_tbl, rle(near))
    near_runs_idx <- with(runs, which(values == TRUE))

    # if never inside, return NA
    if (length(near_runs_idx) == 0)
      return(NA_real_)


    # we use the first run of when they were near the fruit
    # gives us the index of the when they first left the fruit
    move_idx <-
      with(runs, sum(lengths[seq(1, near_runs_idx[1])])) + 1
    # if we never escaped
    if (move_idx > nrow(temp_tbl))
      return(NA)

    with(temp_tbl, time[move_idx])
  }


#' Guess start of escape
#'
#' This function determines the time point when the player was first away from the
#' reference position, and then determines the last time that its velocity away
#' from the reference position was smaller than a threshold. Positions are initially
#' median-filtered, and so is the computed velocity. All calculations are performed
#' without resampling.
#'
#' Developer note: if escape is imposed on an ongoing movement away from bush, it
#' might not be detected by this algorithm based on a fixed velocity criterion.
#' However, it can actually be ambiguous to decide whether the escape was planned
#' before or after the min_time. The algorithm could potentially be improved by
#' using an acceleration criterion (e.g. zero crossing) but the tracker data are
#' a bit noisy and this would require some more elaborate signal processing/filtering.
#'
#' @param df  movement data frame
#' @param ref_pos  reference position, a list containing `pos_x` and `pos_z`
#' @param esc_dist minimum distance from reference position to determine escape
#' @param span span of the median filter
#' @param min_speed minimum speed to determine start of escape
#' @param min_time minimum time to start searching for escape
#' @param max_time maximum time to search for escape
#' @param indx currently disabled (only needed for debugging)
#'
#' @return
#' @export
#'
#' @examples
guess_escape_begin_time <- function(df,
                                    ref_pos,
                                    esc_dist = 0.75,
                                    span = 5,
                                    min_speed = .1,
                                    min_time = min(df$time),
                                    max_time = max(df$time),
                                    indx = 1) {

  # get first time player is away from fruit bush; return NA if this never happens
  if (!is.data.frame(df) || is.na(min_time)) return(NA_real_)
  max_time <- guess_move_from_pos(ref_pos,
                                  filter(df, time > min_time & time < max_time),
                                  max_dist = esc_dist)
  if (is.na(max_time)) return(NA_real_)

  # calculcate and filter velocity
  temp_df <-
    df %>%
    dplyr::mutate(
      dplyr::across(tidyselect::contains(c("pos")),
             ~ stats::runmed(.x, span, endrule = "constant"),
             .names = "{paste0('new_', .col)}"
    ),
    distance = calculate_2d_dist(new_pos_x,
                                  new_pos_z,
                                  ref_pos$pos_x,
                                  ref_pos$pos_z),
    dx = c(0, diff(.data[["distance"]])),
    dt = c(0, diff(.data[["time"]])),
    velocity = dx / dt,
    filt_velocity = stats::runmed(.data[["velocity"]], span, endrule = "constant")) %>%
    dplyr::filter(min_time <= time, time <= max_time)

  if (nrow(temp_df) < 1) return(NA_real_)

  filt_velocity <- pull(temp_df, filt_velocity)

  # if escape is already slowing down at esc_dist then use last index where escape
  # speed was faster

  if (utils::tail(filt_velocity, 1) < min_speed) {
    max_indx <-
      utils::tail(which(filt_velocity > min_speed), 1)
    if (length(max_indx) == 0)
      return(NA_real_)
    filt_velocity <- filt_velocity[1:max_indx]
  }

  # now find escape start
  begin_esc_indx <- utils::tail(which(filt_velocity < min_speed), 1)
  if (length(begin_esc_indx) == 0)
    return(NA_real_)

  # debugging and development tools
  # temp_df %>%
  #   ggplot(aes(x = time, y = distance)) +
  #   geom_path()
  #
  # ggsave(file.path("out", "study1", "figs", "begin_escape", paste0("figure_dist", indx, ".png")))
  #
  # temp_df %>%
  #   ggplot(aes(x = time, y = filt_velocity)) +
  #   geom_path()
  #
  # ggsave(file.path("out", "study1", "figs", "begin_escape", paste0("figure_vel", indx, ".png")))


  temp_df %>%
    dplyr::slice(begin_esc_indx) %>%
    pull(time)
}


#' Guess end of escape time
#'
#' Guess the time at which escape movement ends, depending on trial outcome:
#' Survived, killed: end of recording minus buffer and fade time
#' Escaped to safe house: crossing of the safe house threshold
#' Assumes safe house size 1 m x 1 m x 2 m (height)
#' Does not check whether people came through the door (only distance to safe
#' house centre is considered)
#' If escaped but no safe house position logged, or entry to safe house unclear
#' (edge cases): end of recording minus 0.5 s (as a rough guess)
#'
#' @param safe_pos Safe position (output from find_safe_position)
#' @param max_time Maximum time to look for end of escape. Should usually be the
#'                 time point at which the visual display for the participant ended
#'                 (i.e., not the end of movement tracking or end time of the trial)
#'                 This usually the end time, minus a buffer time (often 1.5 s)
#'                 and the fade time (often 0.05 s)
#' @param end_state End state column entry
#' @param ref_movement Reference movement (e.g. waist movement)
guess_escape_end_time <-
  function(safe_pos,
           max_time = max(ref_movement$time),
           end_state,
           ref_movement) {

    if (!(end_state %in% c("Survived", "ConfrontedThreat", "Safe"))) return(NA_real_)
    if (end_state %in% c("Survived", "ConfrontedThreat")) return(max_time)
    if (is.null(safe_pos)) return(max_time - 0.5)

    ref_movement %>%
      dplyr::filter(time < max_time) %>%
      dplyr::filter(calculate_2d_dist(pos_x, pos_z, safe_pos$pos_x, safe_pos$pos_z) < .6) %>%
      dplyr::slice(1) %>%
      {if (nrow(.) == 1) dplyr::pull(., time) else max_time - 0.5}
  }

#' Guess escape abortion position and time
#'
#' Guess whether escape was aborted and extract position (distance from safe place)
#' and absolute time. Looks for trials with outcome "survived" in which the
#' minimum distance from the safe place was achieved at least 0.5 s before trial
#' end (as derived from `guess_escape_end_time()`)
#'
#' @param ref_movement Movement data frame
#' @param ref_position Reference position of safe place
#' @param begin_escape_time Start of escape
#' @param end_escape_time End of escape
#' @param end_state End state
#'
#' @return
#' @export
#'
#' @examples
guess_escape_abortion <-
  function(ref_movement,
           ref_position,
           begin_escape_time,
           end_escape_time,
           end_state) {

    if (!is.data.frame(df) ||
        !(end_state == "Survived") ||
        is.na(begin_escape_time) ||
        is.na(end_escape_time) ||
        is.null(ref_position))
      return(list(tibble(distance = NA_real_, time = NA_real_)))

    ref_movement <-
      ref_movement %>%
      # compute dt
      dplyr::mutate(dt = c(0, diff(.data[["time"]]))) %>%
      # remove time points outside defined interval
      dplyr::filter(begin_escape_time <= time, time <= end_escape_time) %>%
      dplyr::mutate(distance = calculate_2d_dist(pos_x,
                                                  pos_z,
                                                  ref_position$pos_x,
                                                  ref_position$pos_z))

    if (nrow(ref_movement) == 0)
      return(list(tibble(distance = NA_real_, time = NA_real_)))

    min_dist <-
      ref_movement %>%
      dplyr::pull(distance) %>%
      min()

    min_time <-
    ref_movement %>%
      dplyr::filter(distance == min_dist) %>%
      dplyr::pull(time)

    if (end_escape_time - min_time < 0.5) {
      list(tibble(distance = NA_real_, time = NA_real_))
    } else {
      list(tibble(distance = min_dist, time = min_time))
    }
  }


#' Prepare gaze data for further processing
#'
#' This is a convenience function to resample and filter head movement or
#' eye tracker data frames for further gaze processing.
#'
#' To be used with `extract_...` functions that extract movement features between
#' time points. To avoid edge effects, the filtering is done on the entire data
#' frame.
#'
#' NOTE: to avoid an impact of tracker glitches, data are resampled at default
#' rate of 10 Hz (movement) or 100 Hz (gaze) and median-smoothed over 3 data points
#' (300 or 30 ms).
#'
#' @param df A movement or eyetracker data frame. Expected columns are either
#' `rot_x`, `rot_y`, `rot_z`, or `gaze_direction_x`, `gaze_direction_y`,
#' `gaze_direction_z`
#' @param samplingrate resampling rate
#'
#' @return Resampled data frame with filtered columns.
#' @export
#'
#' @examples
prepare_gaze_data <- function(df,
                              samplingrate = NULL) {
  if (is.null(samplingrate)) {
    samplingrate <-
      ifelse(("gaze_direction_x" %in% colnames(df)), 100, 10)
  }

  # resample over entire data frame to avoid edge effects
  new_time <-
    create_resampling_index(max(df$time) - min(df$time), samplingrate)

  if ("gaze_direction_x" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(!c("focus_object_raw", "focus_threat")) %>%
      dplyr::select(where(~ (!all(is.na(.x))))) %>%
      resample_filter_pos(new_time, span = 3) %>%
      dplyr::select(!tidyselect::starts_with("gaze_")) %>%
      dplyr::rename(
        gaze_direction_x = new_gaze_direction_x,
        gaze_direction_y = new_gaze_direction_y,
        gaze_direction_z = new_gaze_direction_z
      )
  }

  if ("rot_x" %in% colnames(df)) {
    df <-
      df %>%
      resample_filter_pos(new_time, span = 3) %>%
      dplyr::select(!tidyselect::starts_with("rot_")) %>%
      dplyr::rename(rot_x = new_rot_x,
                    rot_y = new_rot_y,
                    rot_z = new_rot_z)
  }
  return(df)
}


#' Resample and median-filter position and rotation columns in a movement data frame
#'
#' This is a convenience function to resample and filter an entire data frame.
#' To be used with `extract_...` functions that extract movement features between
#' time points. To avoid edge effects, the filtering is done on the entire data
#' frame, rather than just on an interval of interest.
#'
#' @param df Movement data frame
#' @param new_time Resampling index
#' @param span Median filter span (default 3)
#'
#' @return Resampled data frame with filtered columns add as `new_pos_...` or `new_rot`
#'

resample_filter_pos <- function(df,
                                new_time,
                                span = 3) {

  df %>%
    # resample to constant rate over entire trial (to avoid edge filter effects)
    resample_movement(new_time, from = min(df$time), to = max(df$time))  %>%
    # extract from list
    purrr::pluck(1) %>%
    # running median smoother
    dplyr::mutate(dplyr::across(
      tidyselect::contains(c("pos", "rot", "gaze", "pupil")),
      ~ stats::runmed(.x, span, endrule = "constant"),
      .names = "{paste0('new_', .col)}"
    ))
}

#' Reorient movement data frame such that initial values match start position
#' and orientation
#'
#' This is a convenience function to make imprecisely attached trackers
#' comparable between scenarios. For example waist trackers may be in a slightly
#' different place on the torso on different trials and subjects. This functions
#' subtracts the median of the first 10 rows and adds the starting position. The
#' median is used to avoid impact of tracker imprecision in individual frames.
#'
#' @param df    A movement data frame.
#' @param .cols Columns to reorient. Default: all rotations and in-plane position
#' @param ref_pos Reference position (named list, names must correspond to columns
#' in df. See also `find_start_pos` for a way of generating such a list)
#'
#' @return      A re-oriented data frame with the same column names
#' @export
#'
#' @examples
reorient_movement_to_start <-
  function(df,
           .cols = c("rot_x", "rot_y", "rot_z", "pos_x", "pos_z"),
           ref_pos = list(
             "pos_x" = 0,
             "pos_y" = 0,
             "pos_z" = 0,
             "rot_x" = 0,
             "rot_y" = 0,
             "rot_z" = 0
           )) {
    df %>%
      dplyr::mutate(dplyr::across({{ .cols }},
      ~ (.) - median((.)[1:10], na.rm = TRUE) + ref_pos[[cur_column()]]),
      .keep = "unused") %>%
      # and return a list
      list()
  }


#' Translate positions in movement data frame to reference position.
#'
#' This is a convenience function to make escape responses comparable between
#' scenarios with different orientation (e.g. different fruit picking place). It
#' does not (yet) allow reorienting the rotations.
#' (NOTE: if required, such rotation re-orientation would require a function to
#' convert a rotation matrix to Euler angles)
#'
#' @param df       A movement data frame.
#' @param ref_pos  A named list of x,y,z reference position (default: 0, 0, 0)
#'
#' @return         A translated movement data frame with the same column names
#' @export
#'
#' @examples
translate_movement_to_ref <-
  function(df,
           ref_pos = list(pos_x=0, pos_y=0, pos_z=0)) {
    df %>%
      dplyr::mutate(
        pos_x = pos_x - ref_pos$pos_x,
        pos_y = pos_y - ref_pos$pos_y,
        pos_z = pos_z - ref_pos$pos_z,
        .keep = "unused"
      ) %>%
      # and return a list
      list()
  }

#' Resample a movement data frame to new time index.
#'
#' Takes a movement data frame, removes value outside range (from, to), and
#' resamples to new index, where 0 in the new index corresponds to "from".
#' For use with 'summarise'.
#'
#' @param df       A movement data frame. Expected column: 'time'
#' @param new_time New time index, with respect to "from"
#' @param from     A numerical value of the starting time for resampling (default 0)
#' @param to       A numerical value of the end time for resampling (default inf, i.e. end of the data frame)
#'
#'
#' @return A resampled movement data frame
#' @export
#'
#' @examples
resample_movement <-
  function(df,
           new_time,
           from = 0,
           to = Inf) {
    if (is.na(from) | is.na(to)) {
      return(df %>%
               dplyr::slice(0) %>%
               dplyr::mutate(new_time = NA) %>%
               list())
    } else {
      df %>%
        # ungroup to make summarise work later (in case any groups exist)
        dplyr::ungroup() %>%
        # remove values outside time range
        dplyr::filter((time > from) & (time < to)) %>%
        # linearly interpolate
        dplyr::summarise(dplyr::across(.cols = everything(),
                                       function(y)
                                         {if (length(y) > 0) return(
                                                suppressWarnings(stats::approx(time - from, y, new_time)$y)) else return(
                                                rep(NA, times = length(new_time)))}))  %>%
        # add new time
        dplyr::mutate(new_time = new_time)  %>%
        # and return a list
        list()
      }
  }


#' Resample a fruit collection frame to new time index.
#'
#' Takes a fruit collection data frame, removes value outside range (from, to), and resamples to new index, where 0 in the new index corresponds to "from". For use with 'summarise'.
#'
#' @param df       A fruit collection data frame. Expected columns: 'time', 'event'
#' @param new_time New time index, with respect to "from"
#' @param from     A numerical value of the starting time for resampling (default 0)
#' @param to       A numerical value of the end time for resampling (default inf, i.e. end of the data frame)
#'
#'
#' @return A resampled movement data frame
#' @export
#'
#' @examples
resample_fruit_task <-
  function(df,
           new_time,
           from = 0,
           to = Inf) {
    if (is.na(from) | is.na(to)) {
      return(df %>%
               dplyr::slice(0) %>%
               dplyr::transmute(fruits_collected = NA, new_time = NA) %>%
               list())
    } else {
      bins <- from + c(0, new_time)
      df %>%
        # ungroup to make summarise work later (in case any groups exist)
        dplyr::ungroup() %>%
        # remove values outside time range
        dplyr::filter((time > from) & (time < to)) %>%
        # find collection events
        dplyr::filter(event == "collect") %>%
        # and sort into time bins provided
        dplyr::summarise(fruits_collected = graphics::hist(time, breaks = bins, plot = FALSE)$counts /
                           diff(bins)) %>%
        # add new time
        dplyr::mutate(new_time = new_time)  %>%
        # and return a list
        list()
    }
  }

#' Resample an eyetracker data frame to new time index.
#'
#' Takes an eyetracker data frame, identifies eligible columns, removes value
#' outside range (from, to), and resamples to new index, where 0 in the new
#' index corresponds to "from".
#' For use with 'summarise'.
#' Works on all generated columns that start with `fixation`, and on all raw
#' columns known to contain numerical data
#'
#' @param df       An eye tracker data frame. Expected column: 'time'
#' @param new_time New time index, with respect to "from"
#' @param from     A numerical value of the starting time for resampling (default 0)
#' @param to       A numerical value of the end time for resampling (default inf, i.e. end of the data frame)
#'
#'
#' @return A resampled eyetracker data frame
#' @export
#'
#' @examples
resample_eyetracker <-
  function(df,
           new_time,
           from = 0,
           to = Inf) {
    if (is.na(from) | is.na(to)) {
      return(df %>%
               dplyr::slice(0) %>%
               dplyr::mutate(new_time = NA) %>%
               list())
    } else {
      df %>%
        # ungroup to make summarise work later (in case any groups exist)
        dplyr::ungroup() %>%
        # remove values outside time range
        dplyr::filter((time > from) & (time < to)) %>%
        # linearly interpolate
        dplyr::summarise(dplyr::across(.cols = starts_with(
          c(
            "fixation",
            "gaze",
            "pupil",
            "eyes",
            "focus_point",
            "focus_distance"
          )
        ),
        function(y)
        {
          if (length(y) > 0)
            return(suppressWarnings(stats::approx(time - from, y, new_time)$y))
          else
            return(rep(NA, times = length(new_time)))
        }))  %>%
        # add new time
        dplyr::mutate(new_time = new_time)  %>%
        # and return a list
        list()
    }
  }


#' Average a list of resampled time series (movement, fruit collection or
#' eyetracking) data frames
#'
#' Takes a list of resampled time series data frames and averages them into one
#' new data frame, by default removing a possible column 'time' if it exists. For
#' use with 'summarise'.
#'
#' Expects a column 'new_time' as a reference.
#' No input checks are done - if the input data frames do not have the same size
#' or do not have the same 'new_time' index, no warning is thrown.
#'
#' @param df       A list of time series data frames. Expected column: 'new_time'
#' @param .cols     A tidy selection of columns to be included in the output
#' (default: everything other than 'time')
#'
#'
#' @return a data frame encapsulated in a list
#' @export
#'
#' @examples
#'
average_timeseries <-
  function(df,
           .cols = !tidyselect::starts_with("time")) {
    df %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(new_time) %>%
      dplyr::summarise(dplyr::across({{ .cols }}, mean, na.rm = TRUE))  %>%
      list()
  }

#' Summarise movement data frame for each unique time stamp
#'
#' Ensures that each time stamp is unique by averaging all rows within duplicated time stamp
#' values (for rotation columns: circular mean). Also adds a column `n` which
#' is the count of rows for this time stamp.
#' This is useful because there can be several observations for a time stamp in a
#' movement data frame. One reason is that the epoch can be "frozen" when the
#' menu button is pressed, generating new data but freezing the timer.
#'
#' @param df Movement data frame
#' @param by = "time": time stamp key
#'
#' @return
#' @export
#'
#' @examples
summarise_movement <- function(df, by){
  df %>%
    dplyr::group_by(.data[[by]]) %>%
    dplyr::summarise(dplyr::across(.cols = !(tidyselect::contains("rot_")), ~ mean(.x, na.rm = T)),
                     dplyr::across(.cols =  tidyselect::contains("rot_"), ~ CircStats::deg(CircStats::circ.mean(CircStats::rad(.x[!is.na(.x)])))),
                     n = n()) %>%
    list()
}


#' Check whether movement data frame contains valid tracker information
#'
#' Checks the momentary speed, distance from reference position and/or distance
#' from reference movement data frame. To compute speed, and distance across two data frames,
#' the positions in the movement data frame(s) are first resampled to 10 Hz and
#' then median-filtered over 3 data points, to avoid an impact of very short
#' tracker glitches. Distance with respect to fixed position is checked for the
#' first 200  ms of the tracking and operates on raw position values.
#'
#'
#' @param df A movement data frame
#' @param ref_df A reference movement data frame (default: NULL)
#' @param ref_pos A reference initial position (list containing `pos_x` and
#'               `pos_z` items) (default: NULL)
#' @param max_dist A maximum distance in metre (default: 2)
#' @param max_speed A maximum speed in m/s (default: 10)
#'
#' @return A list of validity checks. If df is empty then the summary will be valid
#' @export
#'
#' @examples
check_valid_movement <- function(df,
                                 ref_df = NULL,
                                 ref_pos = NULL,
                                 max_dist = 2,
                                 max_speed = 10) {
  if (is.null(ref_df) || nrow(df) == 0) {
    tracker_dist <- NA_real_
    tracker_dist_valid <- NULL
  } else {
    tracker_dist <- extract_movement2_dist(df, ref_df, method = "max")
    tracker_dist_valid <- tracker_dist < max_dist
  }

  if (is.null(ref_pos)  || nrow(df) == 0 || anyNA(ref_pos)) {
    tracker_pos <- NA_real_
    tracker_pos_valid <- NULL
  } else {
    tracker_pos <- extract_movement_dist(
      df,
      ref_pos,
      min_time = df$time[1],
      max_time = df$time[1] + 0.2,
      method = "max"
    )
    tracker_pos_valid <- tracker_pos < max_dist
  }

  if (nrow(df) == 0) {
    tracker_speed <- NA_real_
    tracker_speed_valid <- NULL
  } else {
    tracker_speed <- extract_speed(df, method = "max")
    tracker_speed_valid <- (tracker_speed < max_speed)
  }

  valid <-
    all(c(tracker_dist_valid, tracker_pos_valid, tracker_speed_valid))

  list(
    list(
      valid = valid,
      tracker_dist = tracker_dist,
      tracker_dist_valid = tracker_dist_valid,
      tracker_pos = tracker_pos,
      tracker_pos_valid = tracker_pos_valid,
      tracker_speed = tracker_speed,
      tracker_speed_valid = tracker_speed_valid
    )
  )
}

