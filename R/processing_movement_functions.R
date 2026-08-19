#' Get time of first fruit collection
#'
#' Gets the time stamp of the first collected fruit from a fruit collection data frame
#' (This function is a legacy wrapper for get_fruit_time)
#'
#' @param df Fruit collection data frame, expected columns: `time`, `event`
#'
#' @return a time stamp
#' @export
#'
#' @examples
get_first_fruit_collection <- function(df) {
  get_fruit_time(df, n = 1, event_type = "collect")
}

#' Get fruit appearance or collect time for the n-th fruit
#' This function returns the exact time stamp of this fruit.
#'
#' @param df A fruit collection data frame. Expected columns are `time` and `event`.
#' @param n A specific fruit (integer) or "last" (default: 1)
#' @param event_type "appear" or "collect" (default)
#'
#' @return Time stamp
#' @export
#'
#' @examples
get_fruit_time <-
  function(df,
           n = 1,
           event_type = "collect") {
    if (!is.data.frame(df) ||
        nrow(df) == 0  ||
        (!is.numeric(n) &
         (!is.character(n) || n != "last")) ||
        n <= 0)
      return(NA_real_) # make sure not empty


    df <-
      df %>%
      # find collection events
      dplyr::filter(event == event_type)

    if (n == "last")
      n <- nrow(df)

    if (nrow(df) == 0 ||
        nrow(df) < n)
      return(NA_real_)

    df %>%
      slice(n) %>%
      pull(time)
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
      is.na(ref_time) ||
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
#' Guess the time at which the player initially moved away from a fixed position.
#' This is evaluated within the horizontal plane only (2D).
#' If ref_pos is given, then the function evaluates the orthogonal distance along
#' the line pos-ref_pos
#' (This function is a legacy wrapper for guess_move_at_pos())
#'
#' @param pos A list containing `pos_x` `pos_y` `pos_z` of fixed position.
#' @param ref_pos Optional: a second list of the same type
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
           ref_pos = NULL,
           ref_movement,
           max_dist = 0.2,
           min_time = min(ref_movement$time),
           max_time = max(ref_movement$time)) {

    guess_move_at_pos(
      pos = pos,
      ref_pos = ref_pos,
      ref_movement = ref_movement,
      max_dist = max_dist,
      min_time = min_time,
      max_time = max_time,
      method = "from"
    )
  }


#' Guess First Move From or To Position
#'
#' Guess the time at which the player initially moved away from, or to, a fixed position.
#' This is evaluated within the horizontal plane only (2D).
#' If ref_pos is given, then the function evaluates the orthogonal distance along
#' the line pos-ref_pos
#'
#' @param pos A list containing `pos_x` `pos_y` `pos_z` of fixed position.
#' @param ref_pos Optional: a second list of the same type
#' @param ref_movement A data frame with reference movement, e.g. the head or
#'   the waist movement. Movement file should have `pos_x`, `pos_y`, `pos_z`
#'   columns.
#' @param max_dist Maximum distance in meters before player is
#'   classified as away from the position
#' @min_time Minimum time to look for move (default: start of data frame)
#' @max_time Maximum time to look for move (default: end of data frame)
#' @method "`from`" (default) or "`to`"
#'
#' @return A time stamp (Unity time) representing the time when the participant
#'   initially moved from or to the position.
#' @export
#'
#' @examples
guess_move_at_pos <-
  function(pos,
           ref_pos = NULL,
           ref_movement,
           max_dist = 0.2,
           min_time = min(ref_movement$time),
           max_time = max(ref_movement$time),
           method = "from") {

    ref_movement <-
      ref_movement %>%
      dplyr::filter(time > min_time & time < max_time)

    if (nrow(ref_movement) == 0 | is.null(pos))
      return(NA_real_)


    p1 <- c(pos$pos_x, pos$pos_z)
    if (!is.null(ref_pos)) {
      p2 <- c(ref_pos$pos_x, ref_pos$pos_z)
    } else {
      p2 <- ref_pos
    }

    evaluate_dist <- function(p1, p2, p3) {
      if (is.null(p2)) {
        return(sqrt(sum((p1 - p3)^2)))
      } else {
        return(orthdist(p1, p2, p3))
      }
    }

    temp_tbl <- ref_movement %>%
      dplyr::rowwise() %>%
      # this could be made faster with rowSums, but would need to catch the case that
      # df has only one row
      dplyr::mutate(
        dist = evaluate_dist(p1, p2, c(pos_x, pos_z)),
        # for method "from", near means close to ref_pos
        # for method "to", near means away from ref_pos, i.e. close to initial pos
        near = dplyr::if_else(method == "from",
                       dist < max_dist,
                       dist > max_dist)
      )

    # get "runs" of when we were near or not
    runs <- with(temp_tbl, rle(near))
    near_runs_idx <- with(runs, which(values == TRUE))

    # if never inside, return NA
    if (length(near_runs_idx) == 0)
      return(NA_real_)


    # we use the first run of when they were near the initial position
    # gives us the index of the when they first left the initial position
    move_idx <-
      with(runs, sum(lengths[seq(1, near_runs_idx[1])])) + 1
    # if we never escaped
    if (move_idx > nrow(temp_tbl))
      return(NA_real_)

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
  max_time <- guess_move_from_pos(pos = ref_pos,
                                  ref_movement = filter(df, time > min_time & time < max_time),
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
#' If not going to safe house: end of recording minus buffer and fade time
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

    if (!(stringr::str_to_lower(end_state) %in% c("survived", "confrontedthreat", "safe", "killedthreat"))) return(NA_real_)
    if (!(stringr::str_to_lower(end_state) == "safe")) return(max_time)
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

    if (!is.data.frame(ref_movement) ||
        !(stringr::str_to_lower(end_state) == "survived") ||
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



#' Guess time at a distance between two movement data frames
#'
#' Guess (by interpolation) the time that two movement data frames were, for the
#' first time, closer to (method "min") or further away than (method "max") a
#' distance threshold
#'
#' @param df1 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param df2 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param min_time Minimum time within the resampled movement (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the resampled movement (taken from `"time"`
#'   column) to search.
#' @param method  whether to extract the first time distance is smaller ("min", default)
#'   or larger than ("max") the threshold
#' @param samplingrate resampling rate
#'
#'
#' @return A scalar time
#' @export
#'
#' @examples
guess_time_at_dist2 <-
  function(df1,
           df2,
           dist,
           min_time = min(c(df1$time, df2$time)),
           max_time = max(c(df1$time, df2$time)),
           method = "min",
           samplingrate = 10) {

    df <-
      combine_movement(
        df1 = df1,
        df2 = df2,
        min_time = min_time,
        max_time = max_time,
        samplingrate = samplingrate
      )

    if (is.null(df) || nrow(df) == 0)
      return(NA_real_)


    time <-
      df %>%
      # calculate moment-by-moment distance
      dplyr::mutate(distance = calculate_2d_dist(new_pos_x.1,
                                                 new_pos_z.1,
                                                 new_pos_x.2,
                                                 new_pos_z.2)) %>%
      {
        if (method == "max") {
          dplyr::filter(., distance > dist)
        } else {
          dplyr::filter(., distance < dist)
        }
      } %>%
      dplyr::slice_head() %>%
      dplyr::pull(time)

    if (length(time) == 0) return(NA_real_)

    return(time)

}

#' Estimate actual threat speed from movement data
#'
#' This function takes the threat movement up to player (2.5 m distance) or fruit
#' position (.5 m distance), whatever is earlier, and computes the threat speed
#' over this interval
#'
#' @param threat_df A movement tibble
#' @param player_df A movement tibble
#' @param scenario_data A scenario data structure
#' @param threat_appear_time Threat appearance time
#'
#' @return A list with threat_speed and move_time (the duration of the movement
#'         on which threat_speed is based on)
#' @export
#'
#' @examples
guess_threat_speed <- function(threat_df,
                               player_df,
                               scenario_data,
                               threat_appear_time) {
  # We assume that the jump animation is
  # not triggered until the threat is within 2.5 m distance from the player,
  # so this is a lower bound on the jump time
  threat_near_player <- guess_time_at_dist2(
    threat_df,
    player_df,
    dist = 2.5,
    min_time = threat_appear_time,
    method = "min",
    samplingrate = 200
  )

  threat_near_fruit <- guess_move_at_pos(
    pos = find_fruit_position(scenario_data),
    ref_movement = threat_df,
    max_dist = .5,
    min_time = threat_appear_time,
    method = "to"
  )

  # this is the maximum time we are taking into account for threat movement
  threat_max_movement <-
    find_min(c(threat_near_player, threat_near_fruit))

  threat_speed <- extract_speed(
    threat_df,
    threat_appear_time,
    threat_max_movement,
    method = "mean",
    samplingrate = 200
  )

  move_time <- (threat_max_movement - threat_appear_time)

  return(list(threat_speed = threat_speed, move_time = move_time))
}

#' Guess first time an object is in foveal view
#'
#' @param eye_df An eyetracker data frame
#' @param ref_pos A data frame with reference position as list or movement
#' tibble with columns `pos_x`, `pos_y`, `pos_z`
#' @param object_diameter Diameter of the object in m, default: 0.
#' @param foveal_field The foveal field in degrees, default: 1°.
#'
#' @return A time stamp (real)
#' @export
#'
#' @examples
guess_first_view <- function(eye_df,
                             ref_pos,
                             object_diameter = 0,
                             foveal_angle = 1) {
  out_time <-
    eye_df %>%
    add_in_view(ref_pos,
                object_diameter = object_diameter,
                foveal_angle = foveal_angle) %>%
    ungroup() %>%
    filter(in_view == TRUE) %>%
    slice(1) %>%
    pull(time)

  if (length(out_time) == 0) {
    out_time <- NA_real_
  }
  return(out_time)
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
      
      old_time <- df$time
      
        df %>%
          # ungroup to make summarise work later (in case any groups exist)
          dplyr::ungroup() %>%
          # linearly interpolate
         dplyr::summarise(dplyr::across(.cols = everything(),
                                       function(y)
                                         {if (length(y) > 0) return(
                                                suppressWarnings(stats::approx(old_time - from, y, new_time)$y)) else return(
                                                rep(NA, times = length(new_time)))}))  %>%
        # add new time
        dplyr::mutate(new_time = new_time)  %>%
        dplyr::filter(time >= from & time <= to) %>%
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
#' Ensures that each time stamp is unique by removing all rows with NA time 
#' stamps and averaging all rows within duplicated time stamp
#' values (for rotation columns: circular mean). Also adds a column `n` which,
#' is the count of rows for this time stamp, and a column `n_na` (constant
#' across rows), which is the count of NA time stamp rows.
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
  n_na <- sum(is.na(df$time))
  
  df %>%
    dplyr::filter(!is.na(time)) %>%
    dplyr::group_by(.data[[by]]) %>%
    dplyr::summarise(dplyr::across(.cols = !(tidyselect::contains("rot_")), ~ mean(.x, na.rm = T)),
                     dplyr::across(.cols =  tidyselect::contains("rot_"), ~ CircStats::deg(CircStats::circ.mean(CircStats::rad(.x[!is.na(.x)])))),
                     n = n(),
                     n_na = n_na) %>%
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

#' Combine and resample two movement data frames
#'
#' This is a convenience function used in extract_ and get_ functions that deal
#' with two movement data frames.
#'
#' NOTE: because sampling times can differ between data frames, data are
#' resampled at default rate of 10 Hz, and to avoid an impact of tracker
#' glitches they are median-smoothed over 3 data points (300 ms).
#'
#' @param df1 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param df2 A dataframe of movement (must contain standard trajectory
#'   columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).
#' @param min_time Minimum time within the resampled movements (taken from `"time"`
#'   column) to search.
#' @param max_time Maximum time within the resampled movements (taken from `"time"`
#'   column) to search.
#' @param samplingrate resampling rate
#'
#' @return a combined data frame with original position columns suffixed by .1 and .2
#' @export
#'
#' @examples
combine_movement <- function(df1,
                             df2,
                             min_time = min(c(df1$time, df2$time)),
                             max_time = max(c(df1$time, df2$time)),
                             samplingrate = 10) {
  if (!is.data.frame(df1) ||
      !is.data.frame(df2) ||
      is.na(min_time) ||
      is.na(max_time) ||
      min_time > (max_time - 1 / samplingrate) ||
      nrow(df1) * nrow(df2) == 0)
    return(NULL)

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
    dplyr::filter(min_time <= time, time <= max_time)
}


#' Find the temporal latency between two movement trajectories
#' 
#' This function is useful to align for example tracker and mocap data when 
#' they are not precisely synchronised. The two movement trajectories must be in the
#' same coordinate system, but they can have a spatial translation, different 
#' scaling, and different sampling rate. 
#' 
#' The function uses brute force by a simple grid-search at the specified sample 
#' rate and over the specified interval.
#'
#' @param df1 First movement data frame, must have a column `time`
#' @param df2 Second movement data frame, must have a column `time`
#' @param col1 (String) vector of column names for xyz position of the first data frame
#' @param col2 (String) vector of column names for xyz position of the second data frame
#' @param scale1 Scaling of the first movement trajectory (e.g. 1 for m or 10e-3 for mm)
#' @param scale2 Scaling of the second movement trajectory (e.g. 1 for m or 10e-3 for mm)
#' @param sample_rate Sampling rate used for the comparison
#' @param interval Two-element vector of the interval for the optimisation
#'
#' @returns A list with elements and `lag` (estimated lag in s) and `dist` 
#' (residual spatial distance after mean translation subtracted from both data frames) 
#' @export
#'
#' @examples
find_movement_latencies <- function(df1, df2, 
                                    col1 = c("pos_x", "pos_y", "pos_z"), 
                                    col2 = c("pos_x", "pos_y", "pos_z"),
                                    scale1 = 1,
                                    scale2 = 1,
                                    sample_rate = 10, 
                                    interval = c(-1, 1), 
                                    start_time1 = 0) {
  
  if (is.null(df1) | is.null(df2)) return(list(dist = NA_real_, lag = NA_real_))
  
  pp_df <- function(df, col_set, R, scaling, sample_rate) {
    
    # transform to within-trial time
    df <- df %>%
      mutate(time = time - min(time))
    
    # create resampling index
    new_time <- create_resampling_index(max(df$time) - min(df$time), sample_rate = sample_rate)
    
    # resample
    df %>%
      # select desired columns
      mutate(pos_x = scaling * .data[[col_set[1]]],
             pos_y = scaling * .data[[col_set[2]]],
             pos_z = scaling * .data[[col_set[3]]],
             # translate to move around origin
             pos_x = pos_x - mean(pos_x),
             pos_y = pos_y - mean(pos_y),
             pos_z = pos_z - mean(pos_z),
      ) %>%
      # filter, resample and filter
      select(time, pos_x, pos_y, pos_z) %>%
      dplyr::mutate(dplyr::across(
        tidyselect::contains(c("pos")),
        ~ stats::runmed(.x, k = 3, endrule = "constant"))) %>%
      resample_filter_pos(new_time, span = 3) %>%
      mutate(pos_x = new_pos_x,
             pos_y = new_pos_y,
             pos_z = new_pos_z) %>%
      select(time, pos_x, pos_y, pos_z) 
  }
  
  df1 <- pp_df(df1, col1, R1, scale1, sample_rate)
  df2 <- pp_df(df2, col2, R2, scale2, sample_rate)
  
  min_time <- max(c(min(df1$time), min(df2$time)))
  max_time <- min(c(max(df1$time), max(df2$time)))
  
  df1 <- df1 %>%
    filter(time >= min_time, time <= max_time)
  df2 <- df2 %>%
    filter(time >= min_time, time <= max_time)
  
  # compute lagged mean distance. This is a brute-force approach, it could also
  # be done more efficiently using component-wise fft to maximise cross-correlation,
  # but the implementation is more error-prone and so not useful if this code is 
  # run only once per study
  interval_grid <- seq(from = interval[1], to = interval[2], by = 1/sample_rate)
  
  df_diff <- function(lag, df1, df2) {
    df2 %>%
      mutate(time = time + lag) %>%
      inner_join(df1, by = "time") %>%
      ungroup() %>%
      mutate(diff_x = pos_x.x - pos_x.y,
             diff_y = pos_y.x - pos_y.y,
             diff_z = pos_z.x - pos_z.y,
             dist   = sqrt(diff_x^2 + diff_y^2 + diff_z^2)) %>%
      summarise(mean_dist = mean(dist)) %>%
      pull(mean_dist)
  }      
  
  lagged_diff <- map_dbl(interval_grid, df_diff, df1, df2)  
  return(list(dist = min(lagged_diff), lag = interval_grid[which.min(lagged_diff)]))
  
}

#' Correct movement timing
#' 
#' This function corrects the time column of a given movement tibble by a fixed 
#' offset. This can be useful e.g. when joining different motion capture data 
#' types recorded with different clocks.
#'
#' @param df A movement tibble with column `time`
#' @param offset A scalar offset
#'
#' @returns The same movement tibble with corrected column `time`
#' @export
#'
#' @examples
correct_movement_timing <- function(df, offset) {
  
  if (is.null(df) || is.na(offset)) return(df)
  
  df %>% 
    dplyr::mutate(time = time + offset)
}
