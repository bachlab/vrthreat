# Add rotation matrices to movement data frame
#'
#' Helper function that takes a movement data frame and adds
#' a new column `R` with the rotation matrices.
#'
#' @param df Movement data frame. Expected columns: `rot_x`, `rot_y`, `rot_z`
#'
#' @return Same df with rotation matrices added
#' @export
#'
#' @examples
add_rot_move <- function(df) {
  df %>%
    dplyr::rowwise() %>%
    # create rotation matrices
    dplyr::mutate(R = list(eulunity2rot(rot_x, rot_y, rot_z)))
}

# Add rotation matrices to eye tracker data frame
#'
#' Helper function that takes an eye tracker data frame and adds
#' a new column `R` with the rotation matrices.
#'
#' @param df Movement data frame. Expected columns: `gaze_direction_x`,
#' `gaze_direction_y`, `gaze_direction_z`
#' @param direction Reference gaze direction. Default: forward.
#'
#' @return Same df with rotation matrices added
#' @export
#'
#' @examples
add_rot_eye <- function(df,
                        direction = c(0, 0, 1)) {
  df %>%
    dplyr::rowwise() %>%
    # create rotation matrices
    dplyr::mutate(R = list(vec2rot(
      direction,
      c(gaze_direction_x, gaze_direction_y, gaze_direction_z)
    )))
}

#' Add rotation matrix to movement or eye tracker data frame
#'
#' Takes a movement or eye tracker data frame, and adds rotation matrices from orientation data.
#' For gaze data, rotation is computed wrt a reference vector, usually forward gaze.
#'
#'
#' @param df Movement or eye tracker or combined/resampled data frame. Expects either columns `rot_x`,
#'  `rot_y`, `rot_z`, or columns `gaze_direction_x`, `gaze_direction_y`, `gaze_direction_z`
#' @param direction Reference gaze direction. A single vector. Default: forward
#'
#' @return Same df with rotation matrices added
#' @export
#'
#' @examples
#'
add_rot <- function(df,
                    direction = c(0, 0, 1)) {

  # compute rotation matrix for movement data frame
  if ("rot_x" %in% colnames(df) &&
      !("gaze_direction_x" %in% colnames(df))) {
    df <-
      df %>%
      add_rot_move()
  }

  # compute rotation matrix for eyetracker data frame
  if ("gaze_direction_x" %in% colnames(df) &&
      !("rot_x" %in% colnames(df))) {
    df <-
      df %>%
      add_rot_eye(direction)
  }

  return(df)

}

#' Add direction of the orientation of a reference object relative to a
#' stationary target (in 2D, XZ).
#'
#' Returns the same data frame with added cos(angle) and
#' angle over time.
#'
#' @param ref_movement A data frame with reference movement, e.g. the head or
#'   the waist movement. Movement file should have `pos_x`, `pos_y`, `pos_z`,
#'   `rot_x`, `rot_y`, `rot_z` columns.
#' @param target_position A list containing `pos_x` `pos_y` `pos_z` target position
#' @param relative_to_first = FALSE
#'
#' @return `rev_movement` with new columns `target_diff` (raw angle relative
#' to target) and `target_ratio` (cosine of angle)
#' @export
#'
#' @examples
add_orientation2target <-
  function(ref_movement,
           target_position,
           relative_to_first = FALSE) {

    # sometimes we take the mean of the first 10 samples and use the rotation
    # relative to this value
    relative_to_first_vec <- rep(relative_to_first, nrow(ref_movement))

    temp_df <- ref_movement %>%
      dplyr::mutate(
        rot_x = ifelse(relative_to_first_vec, rot_x - stats::median(rot_x[1:10], na.rm = FALSE), rot_x),
        rot_y = ifelse(relative_to_first_vec, rot_y - stats::median(rot_y[1:10], na.rm = FALSE), rot_y),
        rot_z = ifelse(relative_to_first_vec, rot_z - stats::median(rot_z[1:10], na.rm = FALSE), rot_z)
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(fwd = list(eulunity2rot(rot_x, rot_y, rot_z) %*% c(0.0, 0.0, 1.0)), # get rotation matrix and multiply by forward vector
                    rot_y = calculate_2d_angle_deg(fwd[1, 1], fwd[3, 1])) %>% # process rot_y to avoid discontinuities
      dplyr::ungroup() %>%
      dplyr::mutate(
        target_ang = calculate_2d_angle_deg(target_position$pos_x, # angle towards target
                                            target_position$pos_z,
                                            pos_x,
                                            pos_z),
        target_diff = angle_diff_deg(target_ang, rot_y), # angle relative to reference (e.g. head)
        target_ratio = pracma::cosd(target_diff)
      )

    if (nrow(temp_df) == 0) {
      temp_df <- tibble(
        time = NA_real_,
        target_diff = NA_real_,
        target_ratio = NA_real_
      )
    }

    temp_df %>%
      dplyr::select(time, target_diff, target_ratio) %>%
      dplyr::right_join(ref_movement, by = "time")
  }

#' Add direction of the orientation of a reference object relative to a
#' target movement (in 2D, XZ).
#'
#' Returns the reference data frame with cos(angle) and angle over time added.
#'
#' @param ref_movement A data frame with reference movement, e.g. the head or
#'   the waist movement. Movement file should have `pos_x`, `pos_y`, `pos_z`,
#'   `rot_x`, `rot_y`, `rot_z` columns.
#' @param target_movement Movement of point of interest (e.g threat movement)
#' @param join_by = "time" Column by which to join the two data frames (character)
#' @param relative_to_first = FALSE
#'
#' @return `ref_movement` with columns `time`, `target_diff` (raw angle relative
#' to target) and `target_ratio` (cosine of angle) added
#' @export
#'
#' @examples
add_orientation2target2 <-
  function(ref_movement,
           target_movement,
           join_by = "time",
           relative_to_first = FALSE) {

    empty_out <- ref_movement %>%
      dplyr::mutate(target_diff = NA_real_,
             target_ratio = NA_real_)

    if (nrow(target_movement) == 0)
      return(empty_out)

    df <-
      dplyr::left_join(
        ref_movement,
        target_movement,
        by = join_by,
        suffix = c("_ref", "_target")
      )

    # sometimes we take the mean of the first 10 samples and use the rotation
    # relative to this value
    relative_to_first_vec <- rep(relative_to_first, nrow(df))

    temp_df <- df %>%
      dplyr::mutate(
        rot_x_ref = ifelse(relative_to_first_vec, rot_x_ref - stats::median(rot_x_ref[1:10], na.rm = FALSE), rot_x_ref),
        rot_y_ref = ifelse(relative_to_first_vec, rot_y_ref - stats::median(rot_y_ref[1:10], na.rm = FALSE), rot_y_ref),
        rot_z_ref = ifelse(relative_to_first_vec, rot_z_ref - stats::median(rot_z_ref[1:10], na.rm = FALSE), rot_z_ref)
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(fwd = list(eulunity2rot(rot_x_ref, rot_y_ref, rot_z_ref) %*% c(0.0, 0.0, 1.0)), # get rotation matrix and multiply by forward vector
                    rot_y_ref = calculate_2d_angle_deg(fwd[1, 1], fwd[3, 1])) %>% # process rot_y to avoid discontinuities
      dplyr::ungroup() %>%
      dplyr::mutate(
        target_ang = calculate_2d_angle_deg(pos_x_target, # angle towards target
                                            pos_z_target,
                                            pos_x_ref,
                                            pos_z_ref),
        target_diff = angle_diff_deg(target_ang, rot_y_ref), # angle relative to reference (e.g. head)
        target_ratio = pracma::cosd(target_diff)
      )

    if (nrow(temp_df) == 0)
      return(empty_out)

    temp_df %>%
      dplyr::select(all_of(c(join_by, "target_diff", "target_ratio"))) %>%
      dplyr::right_join(ref_movement, by = join_by)
  }


#' Add target fixations to eyetracker data
#'
#' Returns the same dataframe with new column indicating 0 (no fixation) and 1
#' (fixation) over time.
#'
#' @param df Eyetracker data frame. Expected column is `focus_object_raw`.
#' @param target Unity's internal target name (e.g. as returned by
#' `find_unity_threat_name`), or vector of target names
#' @param targetname Optional suffix for the new column name (default: `fixation`
#' without suffix)
#'
#' @return Eyetracker data frame with new column `fixation` or
#' `fixation_targetname` added
#' @export
#'
#' @examples
add_fixation2target <- function(df, target, targetname = NULL) {

  if (is.null(targetname)) {
    fix_col_name <- "fixation"
  } else {
    fix_col_name <- stringr::str_c("fixation_", targetname)
  }

  df %>%
    dplyr::mutate("{fix_col_name}" := as.numeric(focus_object_raw %in% target))
}

#' Add angular difference wrt reference direction to movement or eye tracker data frame
#'
#' Takes a movement or eye tracker data frame, derives rotation matrices from orientation data,
#' applies them to a reference direction vector, and calculates the row-by-row
#' angular difference for the vector rotation.
#'
#' @param df Movement or eye tracker or combined/resampled data frame. Expects either columns `rot_x`,
#'  `rot_y`, `rot_z`, or columns `gaze_direction_x`, `gaze_direction_y`, `gaze_direction_z`
#' @param direction Reference gaze vector direction (default: forward)
#'
#' @return Input data frame with column `angular_diff` added.
#' @export
#'
#' @examples
add_angular_diff <- function(df,
                             direction = c(0, 0, 1)) {

  df %>%
    add_rot(direction) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(R_next = dplyr::lag(R)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(angular_diff =
                    abs(rot2centralangle(R, R_next, direction))) %>%
    # remove rotation matrices
    dplyr::select(!c(R, R_next))

}

#' Add movement speed to movement data frame
#'
#' Takes a movement data frame and calculates the row-by-row speed
#'
#' @param df Movement data frame. Expects columns `rot_x`, `rot_y`, `rot_z`,
#'           `time`.
#'
#' @return Input data frame with column `speed` added.
#' @export
#'
#' @examples
add_speed <- function(df) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains("pos") | tidyselect::any_of("time"),
        ~ c(NA, diff(.)),
        .names = "{paste0(.col, '_diff')}"
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(speed = 1 / time_diff * norm(c(pos_x_diff,
                                                 pos_y_diff,
                                                 pos_z_diff), type = "2"))
}

#' Add gaze elevation to (head) movement or eye tracker data frame
#'
#' Takes a movement or eye tracker data frame, derives rotation matrices from orientation data,
#' applies them to a reference direction vector, and calculates the row-by-row
#' angular difference for the vector rotation.
#'
#' @param df Movement or eye tracker or combined/resampled data frame. Expects either columns `rot_x`,
#'  `rot_y`, `rot_z`, or columns `gaze_direction_x`, `gaze_direction_y`, `gaze_direction_z`
#' @param direction Reference gaze vector direction (default: forward)
#'
#' @return Original data frame with column `gaze_elevation` in degrees added.
#' @export
#'
#' @examples
add_gaze_elevation <- function(df,
                               direction = c(0, 0, 1)) {
  # Note: cart2sph in pracma returns (theta, phi, r) where theta is the angle with
  # the x axis and phi the angle with the xy plane (i.e. elevation).

  df %>%
    add_rot(direction) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(gaze_vector_cart_unity = list(c(R %*% direction)),
                  gaze_vector_cart_world = list(gaze_vector_cart_unity[c(1, 3, 2)] * c(-1, 1, 1)),
                  gaze_vector_sph = list(cart2sph(gaze_vector_cart_world)),
                  gaze_elevation = gaze_vector_sph[2]/pi*180)  %>%
    dplyr::select(!c(R, gaze_vector_cart_unity, gaze_vector_cart_world, gaze_vector_sph))
}
