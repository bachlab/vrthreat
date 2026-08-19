
# ------------------------------------------------------------------------------
# --- maths functions ----------------------------------------------------------
# ------------------------------------------------------------------------------

# -- angle_diff_deg() ----------------------------------------------------------
test_that("Angle difference is calculated correctly",{
  expect_equal(
    angle_diff_deg(
      c(10, 360, 380, -120, -360, -720),
      c(-10, 380, 360, -170, 370, -180)
    ),
    c(20, -20, 20, 50, -10, -180)
  )
})


# -- calculate_2d_angle_deg() --------------------------------------------------
test_that("calculate_2d_angle_deg gives correct output", {
  expect_equal(
    c(
      calculate_2d_angle_deg(1, 1, 0, 0),
      calculate_2d_angle_deg(1, 0, 0, 0),
      calculate_2d_angle_deg(0, -1, 0, 0),
      calculate_2d_angle_deg(-1, 0, 0, 0),
      calculate_2d_angle_deg(0, 1, 0, 0),
      calculate_2d_angle_deg(0, 0, 0, 0)
    ),
    c(45, 90, 180, -90, 0, 0)
  )
})


# -- calculate_2d_dist() ------------------------------------------------------
test_that("calculate_2d_dist gives correct output", {
  expect_equal(c(calculate_2d_dist(1, 1, 2, 1),
                 calculate_2d_dist(2, 0)),
               c(1, 2))
})


# -- timeseries_mean() ---------------------------------------------------------
test_that("Mean of timeseries is calculated correctly", {
  expect_equal(
    timeseries_mean(1:5, 1:5),
    3
  )
  expect_equal(
    timeseries_mean(1:5, c(1, 2, 2.5, 4, 5)),
    3.125
  )
})
test_that("Mean of timeseries works with 1 element vector", {
  expect_equal(
    timeseries_mean(25.5, 1.0),
    25.5
  )
})



# -- eulunity2rot --------------------------------------------------------------
test_that("eulunity2rot clockwise rotation by pi/2 about x-axis converts y into z",
          {
            expect_equal(as.vector(eulunity2rot(90, 0, 0) %*% c(0, 1, 0)), c(0, 0, 1))
          })


test_that("eulunity2rot clockwise rotation by pi/2 about y-axis converts z into x",
          {
            expect_equal(as.vector(eulunity2rot(0, 90, 0) %*% c(0, 0, 1)), c(1, 0, 0))

          })

test_that("eulunity2rot clockwise rotation by pi/2 about z-axis converts x into y",
          {
            expect_equal(as.vector(eulunity2rot(0, 0, 90) %*% c(1, 0, 0)), c(0, 1, 0))
          })

test_that("eulunity2rot gives correct outputs",
          {
            ## test compositions of basic rotations
            # a simple composition
            expect_equal(as.vector(eulunity2rot(90, 0, 90) %*% c(1, 0, 0)), c(0, 0, 1))
            # a composition with two rotations that leave the rotated vector unchanged only when
            # rotated in correct order
            expect_equal(as.vector(eulunity2rot(90, 133, 39) %*% c(0, 0, 1)), c(0, -1, 0))
          })

# -- vec2rot() -----------------------------------------------------------------

test_that("vec2rot gives correct output for norm vectors",
          {
            test_vec2rot <- function() {
              x <- runif(3)
              x <- x / norm(x, type = "2")
              y <- runif(3)
              y <- y / norm(y, type = "2")
              R <- vec2rot(x, y)
              yhat <- R %*% x
              sum((yhat - y) ^ 2)
            }

            expect_equal(
              c(
                test_vec2rot(),
                test_vec2rot(),
                test_vec2rot(),
                test_vec2rot(),
                test_vec2rot()
              ),
              rep(0, 5)
            )
          })

# -- angle_between -------------------------------------------------------------
test_that("angle_between_vec gives correct outputs",
          {
            expect_equal(angle_between(c(1, 0, 0), c(0, 1, 0)), 90)
            expect_equal(angle_between(c(1, 0, 0), c(1, 1, 0)), 45)
          })

# -- rot2centralangle ----------------------------------------------------------
test_that("rot2centralangle gives correct outputs", {
  # exchange x and y axis rotates x axis by 90 deg
  expect_equal(rot2centralangle(diag(3),
                                matrix(
                                  c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = T
                                ),
                                c(1, 0, 0)),
               90)
  # exchange x and y axis rotates z axis by 0 deg
  expect_equal(rot2centralangle(diag(3),
                                matrix(
                                  c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = T
                                ),
                                c(0, 0, 1)),
               0)
  # output does not depend on magnitude of direction vector
  expect_equal(rot2centralangle(diag(3),
                                matrix(
                                  c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = T
                                ),
                                c(1, 0, 0)),
               rot2centralangle(diag(3),
                                matrix(
                                  c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = T
                                ),
                                c(3, 0, 0))
  )

  # Rotation by 180 degrees around the z axis
  expect_equal(rot2centralangle(diag(3),
                                matrix(c(-1, 0, 0, 0, -1, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
                                c(1, 0, 0)),
               180)

  # Output does not depend on magnitude of direction vector
  expect_equal(rot2centralangle(diag(3),
                                matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
                                c(1, 0, 0)),
               rot2centralangle(diag(3),
                                matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
                                c(3, 0, 0)))

  # Negative rotation angle
  expect_equal(rot2centralangle(diag(3),
                                matrix(c(0, -1, 0, -1, 0, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
                                c(1, 0, 0)),
               90)

})

# -- proj_vec2vec --------------------------------------------------------------
test_that("proj_vec2vec gives correct outputs", {
  test_proj_vec2vec <- function(v1, v2, v3) {
    norm(proj_vec2vec(v1, v2) - v3, type = "2")
  }
  expect_equal(c(
    test_proj_vec2vec(c(1, 1, 1), c(1, 0, 0), c(1, 0, 0)),
    test_proj_vec2vec(c(2, 2, 2), c(0, 1, 0), c(0, 2, 0)),
    test_proj_vec2vec(c(2, 2, 2), c(0, 2, 0), c(0, 2, 0)),
    test_proj_vec2vec(c(1, 0, 0), c(1, 1, 0), c(.5, .5, 0)),
    test_proj_vec2vec(c(9, 9), c(1, 0), c(9, 0))
  ),
  c(0, 0, 0, 0, 0))
})

# -- proj_vec2plane ------------------------------------------------------------
test_that("proj_vec2plane gives correct outputs", {
  test_proj_vec2plane <- function(v1, v2, v3, v4) {
    norm(proj_vec2plane(v1, v2, v3) - v4, type = "2")
  }
  expect_equal(c(
    test_proj_vec2plane(c(1, 1, 1), c(1, 0, 0), c(0, 1, 0), c(1, 1, 0)),
    test_proj_vec2plane(c(2, 2, 2), c(1, 0, 0), c(0, 1, 0), c(2, 2, 0)),
    test_proj_vec2plane(c(3, 1, 1), c(0, 1, 0), c(0, 0, 1), c(0, 1, 1))
  ),
  c(0, 0, 0))
})

# -- orthdist ------------------------------------------------------------------
test_that("orthdist gives correct outputs", {
  expect_equal(c(orthdist(c(1, 0), c(0, 0), c(0.5, 100)),
                 orthdist(c(1, 0), c(0, 0), c(0.3, 100)),
                 orthdist(c(1, 0, 0), c(0, 0, 0), c(0.5, 100, 100))),
               c(0.5, 0.7, 0.5))
})

# -- cart2sph ------------------------------------------------------------------
test_that("cart2sph gives the same output as pracma", {
  v <- runif(3)
  expect_equal(norm(cart2sph(v) - pracma::cart2sph(v), type = "2"), 0)
})

# -- create_resampling_index ---------------------------------------------------
test_that("create_resampling_index gives correct output", {
  expect_equal(sum(
    create_resampling_index(1, 10) - seq(from = .1, to = 1, by = .1)
  ), 0)
  expect_equal(sum(
    create_resampling_index(2, 10) - seq(from = .1, to = 2, by = .1)
  ), 0)
})

# -- find_min ------------------------------------------------------------------
test_that("find_min works correctly", {
  expect_equal(find_min(c(1, 2, 3)), 1)
  expect_equal(find_min(c(NA, NA, NA)), NA_real_)
  expect_equal(find_min(numeric(0)), NA_real_)
  expect_equal(find_min(c(NA, 5, 3)), 3)
  expect_equal(find_min(c(-1, -5, -3)), -5)
})

# -- find_argmin ---------------------------------------------------------------
test_that("find_argmin works correctly", {
  expect_equal(find_argmin(c(3, 1, 2)), 2)
  expect_equal(find_argmin(c(NA, NA, NA)), NA_real_)
  expect_equal(find_argmin(numeric(0)), NA_real_)
  expect_equal(find_argmin(c(NA, 5, 3)), 3)
  expect_equal(find_argmin(c(-3, -1, -5)), 3)
})

# ------------------------------------------------------------------------------
# --- processing_movement functions --------------------------------------------
# ------------------------------------------------------------------------------

df <- function(pos, rot, time = 0) {
  tibble::tibble(
    time = time,
    pos_x = pos[1],
    pos_y = pos[2],
    pos_z = pos[3],
    rot_x = rot[1],
    rot_y = rot[2],
    rot_z = rot[3]
  )
}

# -- guess_move_at_post --------------------------------------------------------


test_that("guess_move_at_pos works correctly",
          {
            move_from_origin <- dplyr::bind_rows(df(
              time = seq(from = 0, to = 1, by = .1),
              pos = c(0, 0, 0),
              rot = c(0, 0, 0)
            ),
            df(
              time = seq(from = 1.1, to = 2, by = .1),
              pos = c(0, 0, 10),
              rot = c(0, 0, 0)
            ))
            move_to_origin   <- dplyr::bind_rows(df(
              time = seq(from = 0, to = 1, by = .1),
              pos = c(0, 0, 10),
              rot = c(0, 0, 0)
            ),
            df(
              time = seq(from = 1.1, to = 2, by = .1),
              pos = c(0, 0, 0),
              rot = c(0, 0, 0)
            ))

            move_from_origin_lat <- move_from_origin
            move_to_origin_lat   <- move_to_origin

            move_from_origin_lat$pos_x <- 10
            move_to_origin_lat$pos_x   <- 10

            pos_origin <- list(pos_x = 0,
                               pos_y = 0,
                               pos_z = 0)
            pos_forward <- list(pos_x = 0,
                                pos_y = 0,
                                pos_z = 10)

            # moving from origin
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = NULL,
                ref_movement = move_from_origin,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "from"
              ),
              1.1
            )
            # moving from origin when not origin should return NA
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = NULL,
                ref_movement = move_to_origin,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "from"
              ),
              NA_real_
            )
            # moving to origin
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = NULL,
                ref_movement = move_to_origin,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "to"
              ),
              1.1
            )
            # moving to origin when at origin should return NA
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = NULL,
                ref_movement = move_from_origin,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "to"
              ),
              NA_real_
            )
            # laterally moving from origin
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = pos_forward,
                ref_movement = move_from_origin_lat,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "from"
              ),
              1.1
            )
            # laterally moving from origin when not at origin should return NA
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = pos_forward,
                ref_movement = move_to_origin_lat,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "from"
              ),
              NA_real_
            )
            # laterally  moving to origin
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = pos_forward,
                ref_movement = move_to_origin_lat,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "to"
              ),
              1.1
            )
            # laterally moving to origin when at origin should return NA
            expect_equal(
              guess_move_at_pos(
                pos = pos_origin,
                ref_pos = pos_forward,
                ref_movement = move_from_origin_lat,
                max_dist = 0.5,
                min_time = 0,
                max_time = 100,
                method = "to"
              ),
              NA_real_
            )
          })


# ------------------------------------------------------------------------------
# --- add_movement functions ---------------------------------------------------
# ------------------------------------------------------------------------------


extract_var <- function(df, varname) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::select(tidyselect::all_of(varname)) %>%
    dplyr::pull()
}


# -- add_rot_move ----------------------------------------------------

test_that("add_rot_move correctly adds rotation matrices", {
  test_df <- tibble::tibble(
    rot_x = c(0, 90, 180),
    rot_y = c(0, 45, 90),
    rot_z = c(0, 90, 180)
  )

  # Apply the function to add rotation matrices
  result_df <- add_rot_move(test_df)

  # Check if the function added the column 'R'
  expect_true("R" %in% names(result_df), info = "Rotation matrix column 'R' not added")

  # Calculate expected rotation matrices using eulunity2rot
  expected_matrices <- list(
    eulunity2rot(0, 0, 0),
    eulunity2rot(90, 45, 90),
    eulunity2rot(180, 90, 180)
  )

  # Check the content of the 'R' column against expected values
  purrr::map2(result_df$R, expected_matrices, ~{
    expect_equal(.x, .y)
  })
})


# -- add_vector2target ----------------------------------------------------
test_that("add_vector2target adds correct values to a stationary target", {
  # Ref_movement is from eye-tracker
  eye_df <- tibble::tibble(
    time = 1:3,
    gaze_origin_x = c(0, 0, 0),
    gaze_origin_y = c(0, 0, 0),
    gaze_origin_z = c(0, 0, 0)
  )

  # Ref_movement is from head/foot/waist trackers
  tracker_df <- tibble::tibble(
    time = 1:3,
    pos_x = c(0, 0, 0),
    pos_y = c(0, 0, 0),
    pos_z = c(0, 0, 0)
  )

  # Define target_position
  target_position <- list(pos_x = -1, pos_y = 0, pos_z = 1)

  # Apply add_vector2target function to the eye data
  new_eye_df <- add_vector2target(eye_df, target_position)

  # Apply add_vector2target function to the non-eye ref movement data
  new_mov_df <- add_vector2target(tracker_df, target_position)

  # Expected vector values
  expected_vector2target_x <- -1
  expected_vector2target_y <- 0
  expected_vector2target_z <- 1

  # Testing whether values match [eye data]
  expect_equal(new_eye_df$vector2target_x[1], expected_vector2target_x)
  expect_equal(new_eye_df$vector2target_y[1], expected_vector2target_y)
  expect_equal(new_eye_df$vector2target_z[1], expected_vector2target_z)

  # Testing whether values match [ref movement data]
  expect_equal(new_mov_df$vector2target_x[1], expected_vector2target_x)
  expect_equal(new_mov_df$vector2target_y[1], expected_vector2target_y)
  expect_equal(new_mov_df$vector2target_z[1], expected_vector2target_z)
})


# -- add_vector2target2 ----------------------------------------------------

test_that("Vector differences to dynamic targets are calculated correctly", {
  set.seed(123) # For reproducibility

  # Simulate ref data (non-eye data such as head movement)
  ref_movement <- tibble::tibble(
    time = seq(1, 10, by = 1),
    pos_x = runif(10, -0.5, 0.5),
    pos_y = runif(10, -0.5, 0.5),
    pos_z = runif(10, -0.5, 0.5)
  )

  # Simulate eye data
  eye_movement <- tibble::tibble(
    time = seq(1, 10, by = 1),
    gaze_origin_x = runif(10, -0.5, 0.5),
    gaze_origin_y = runif(10, -0.5, 0.5),
    gaze_origin_z = runif(10, -0.5, 0.5)
  )

  # Simulate target movement data (dynamic target)
  target_movement <- tibble::tibble(
    time = seq(1, 10, by = 1),
    pos_x = seq(0.1, 1, length.out = 10),
    pos_y = seq(-0.1, -1, length.out = 10),
    pos_z = seq(0.2, 2, length.out = 10)
  )

  # Using function `add_vector2target2` to calculate vector differences
  output_ref_df <- add_vector2target2(ref_movement, target_movement, "time")
  output_eye_df <- add_vector2target2(eye_movement, target_movement, "time")

  # Examine whether values match
  for (i in 1:nrow(output_ref_df)) {
    expected_vector_diff_x_ref <- target_movement$pos_x[i] - ref_movement$pos_x[i]
    expected_vector_diff_y_ref <- target_movement$pos_y[i] - ref_movement$pos_y[i]
    expected_vector_diff_z_ref <- target_movement$pos_z[i] - ref_movement$pos_z[i]

    expect_equal(output_ref_df$vector2target_x[i], expected_vector_diff_x_ref)
    expect_equal(output_ref_df$vector2target_y[i], expected_vector_diff_y_ref)
    expect_equal(output_ref_df$vector2target_z[i], expected_vector_diff_z_ref)
  }

  for (i in 1:nrow(output_eye_df)) {

    expected_vector_diff_x_eye <- target_movement$pos_x[i] - eye_movement$gaze_origin_x[i]
    expected_vector_diff_y_eye <- target_movement$pos_y[i] - eye_movement$gaze_origin_y[i]
    expected_vector_diff_z_eye <- target_movement$pos_z[i] - eye_movement$gaze_origin_z[i]

    expect_equal(output_eye_df$vector2target_x[i], expected_vector_diff_x_eye)
    expect_equal(output_eye_df$vector2target_y[i], expected_vector_diff_y_eye)
    expect_equal(output_eye_df$vector2target_z[i], expected_vector_diff_z_eye)
  }
})

# -- add_in_view ----------------------------------------------------

test_that("add_in_view identifies targets within and outside the visual field", {
  # create eyetracker df: looking straight ahead, then looking 90° left
  eye_df <- tibble::tibble(
    time = 1:2,
    gaze_direction_x = c(0, 1),
    gaze_direction_y = c(1, 0),
    gaze_direction_z = c(0, 0),
    gaze_origin_x = c(0, 0),
    gaze_origin_y = c(0, 0),
    gaze_origin_z = c(0, 0)
  )

  # target is straight ahead
  ref_pos <- list(pos_x = 0, pos_y = 1, pos_z = 0)

  new_eye_df <- add_in_view(
    eye_df,
    ref_pos,
    # wide foveal angle to halve the left forward quadrant
    foveal_angle = 90,
    direction = c(0, 0, 1)
    )

  expect_equal(new_eye_df$in_view, c(TRUE, FALSE))
})


# -- add_orientation2target ----------------------------------------------------
test_that("add_orientation2target adds correct values", {
  newdf <- df(pos = c(0, 0, 0), rot = c(0, 0, 0)) %>%
    add_orientation2target(list(
      pos_x = -1,
      pos_y = 0,
      pos_z = 1
    ))
  expect_equal(extract_var(newdf, "target_diff"), -45)
  expect_equal(extract_var(newdf, "target_ratio"), pracma::cosd(-45))
})

# -- add_orientation2target2 ---------------------------------------------------
test_that("add_orientation2target2 adds correct values", {
  newdf <- df(pos = c(0, 0, 0), rot = c(0, 0, 0)) %>%
    add_orientation2target(df(pos = c(-1, 0, 1), rot = c(0, 0, 0)))
  expect_equal(extract_var(newdf, "target_diff"), -45)
  expect_equal(extract_var(newdf, "target_ratio"), pracma::cosd(-45))
})


# -- add_angular_diff ----------------------------------------------------------
test_that("add_angular_diff adds correct values", {
  # Test with basic rotation data
  df <- tibble::tibble(pos_x = c(0, 0),
                           pos_y = c(0, 0),
                           pos_z = c(0, 0),
                           rot_x = c(0, 34),
                           rot_y = c(0, 0),
                           rot_z = c(0, 0),
                           time = c(.1, .2))

  newdf  <- df %>%
    add_angular_diff()
  expect_equal(dplyr::pull(newdf, "angular_diff")[2], 34)

   # Test edge cases: no rotation
  df <- tibble::tibble(pos_x = c(0, 0),
                       pos_y = c(0, 0),
                       pos_z = c(0, 0),
                       rot_x = c(0, 0),
                       rot_y = c(0, 0),
                       rot_z = c(0, 0),
                       time = c(.1, .2))

  newdf  <- df %>%
    add_angular_diff()
  expect_equal(dplyr::pull(newdf, "angular_diff")[2], 0)
})

# -- add_speed -----------------------------------------------------------------
test_that("add_speed adds correct values", {
  newdf1 <- df(pos = c(0, 0, 0), rot = c(0, 0, 0), time = .1)
  newdf2 <- df(pos = c(1, 1, 1), rot = c(0, 0, 0), time = .2)
  newdf  <- dplyr::bind_rows(newdf1, newdf2) %>%
    add_speed()
  expect_equal(extract_var(newdf, "speed"), sqrt(3)/.1)
})

# -- add_gaze_elevation  -------------------------------------------------------

test_that("add_gaze_elevation adds correct values on x axis", {
  # x rotation (clockwise) by 45 degrees corresponds to 45 degrees downward
  expect_equal(
    df(pos = c(0, 0, 0), rot = c(45, 0, 0)) %>%
      add_gaze_elevation() %>%
      extract_var("gaze_elevation"),
    -45
  )

})

test_that("add_gaze_elevation adds no values to y axis", {

  # y rotation has no impact on gaze elevation
  expect_equal(
    df(pos = c(0, 0, 0), rot = c(0, 45, 0)) %>%
      add_gaze_elevation() %>%
      extract_var("gaze_elevation"),
    0
  )

})

test_that("add_gaze_elevation adds no values to z axis", {

  # z rotation has no impact on gaze elevation
  expect_equal(
    df(pos = c(0, 0, 0), rot = c(0, 0, 45)) %>%
      add_gaze_elevation() %>%
      extract_var("gaze_elevation"),
    0
  )

})

# test for create_resampling_index

# ------------------------------------------------------------------------------
# --- extract functions -------------------------------------------------------
# ------------------------------------------------------------------------------

# -- extract_duration ----------------------------------------------------------

test_that("extract_duration correctly extracts duration", {

  newdf <- df(pos = c(0, 0, 0), rot = c(0, 0, 0), time = seq(from = 0, to = 10, by = 0.1))
  # z rotation has no impact on gaze elevation
  expect_equal(
    newdf %>%
      extract_recording_duration(),
    10
  )

})

# ------------------------------------------------------------------------------
# --- scenario functions -------------------------------------------------------
# ------------------------------------------------------------------------------

test_that("find_hierarchy_object finds Bush and children", {
  test_hierarchy <- jsonlite::read_json("test_files/test_hierarchy.json")
  result <-
    find_hierarchy_object(test_hierarchy, "Bush", include_children = TRUE)

  expect_equal(length(result), 4)
})

test_that("find_hierarchy_object finds Bush_LOD0", {
  test_hierarchy <- jsonlite::read_json("test_files/test_hierarchy.json")

  result <-
    find_hierarchy_object(test_hierarchy, "Bush_LOD0", include_children = TRUE)

  expect_false(is.null(result))

  expect_false(is.null(result[["position"]]))
  expect_equal(result[["position"]][["x"]], 2.1698818207)

  expect_false(is.null(result[["euler_angles"]]))
  expect_equal(result[["euler_angles"]][["y"]], 90.0000000000)
})

test_that("find_hierarchy_object_any finds first matched name when the first matches the first", {
  test_hierarchy <- jsonlite::read_json("test_files/test_hierarchy.json")

  result <-
    find_hierarchy_object_any(test_hierarchy, c("Bush_LOD0", "Bush_LOD1"))

  expect_equal(result[["name"]], "Bush_LOD0")
})

test_that("find_hierarchy_object_any finds first matched name when the first matches the second", {
  test_hierarchy <- jsonlite::read_json("test_files/test_hierarchy.json")

  result <-
    find_hierarchy_object_any(test_hierarchy, c("blah blah", "Bush_LOD0", "Bush_LOD1"))

  expect_equal(result[["name"]], "Bush_LOD0")
})

test_that("find_hierarchy_object chains correctly", {
  test_hierarchy <- jsonlite::read_json("test_files/test_hierarchy.json")

  result <- test_hierarchy %>%
    find_hierarchy_object_any(fruit_gameobject_names, include_children = TRUE) %>%
    find_hierarchy_object("WalkHere")

  expect_equal(result[["name"]], "WalkHere")
})

# ------------------------------------------------------------------------------
# --- processing trial functions -----------------------------------------------
# ------------------------------------------------------------------------------

test_that("identify_na_columns identifies correct columns", {
  df <- data.frame(
    a = c(1, 2, NA),
    b = c(NA, 2, 3),
    c = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  # Test default behavior
  expect_equal(identify_na_columns(df), c("a", "b"))

  # Test with return_counts = TRUE
  expect_equal(identify_na_columns(df, return_counts = TRUE), c(a = 1, b = 1))

  # Test with no NAs
  df_no_na <- data.frame(a = 1:3, b = 4:6)
  expect_equal(identify_na_columns(df_no_na), character(0))

  # Test with empty data frame
  df_empty <- data.frame()
  expect_equal(identify_na_columns(df_empty), character(0))

  # Test with non-data frame input
  expect_error(identify_na_columns(1:10), "Input 'data' must be a data frame or tibble.")
})

# -- flatten_scenario_data ----------------------------------------------------

test_that("depth works and paths are readable", {
  scene <- list(
    list(name="A", position=list(x=0,y=0,z=0), euler_angles=list(x=0,y=0,z=0),
         children=list(list(name="A1", position=list(x=0,y=0,z=0), euler_angles=list(x=0,y=0,z=0), children=list()))),
    list(name="B", position=list(x=0,y=0,z=0), euler_angles=list(x=0,y=0,z=0), children=list())
  )
  out <- flatten_scenario_data(scene, include_depth = TRUE)
  expect_equal(nrow(out), 3)
  expect_true(all(c("root[[1]]", "root[[1]]$children[[1]]", "root[[2]]") %in% out$path))
  expect_equal(out$depth[out$path == "root[[1]]"], 0)
  expect_equal(out$depth[out$path == "root[[1]]$children[[1]]"], 1)
})

test_that("single node produces 1 row with expected cols", {
  node <- list(
    name = "Root",
    position = list(x = 1, y = 2, z = 3),
    euler_angles = list(x = 10, y = 20, z = 30),
    children = list()
  )
  out <- flatten_scenario_data(node)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_named(out, c("path","name","pos_x","pos_y","pos_z","eul_x","eul_y","eul_z","depth"))
  expect_equal(out$path, "root")
  expect_equal(out$pos_x, 1)
  expect_equal(out$eul_z, 30)
})

test_that("container list flattens; paths and depth are correct", {
  scene <- list(
    list(
      name="A",
      position=list(x=0,y=0,z=0),
      euler_angles=list(x=0,y=0,z=0),
      children=list(
        list(name="A1", position=list(x=1,y=0,z=0), euler_angles=list(x=0,y=0,z=0), children=list())
      )
    ),
    list(name="B", position=list(x=0,y=1,z=0), euler_angles=list(x=0,y=0,z=0), children=list())
  )
  out <- flatten_scenario_data(scene, include_depth = TRUE)
  expect_equal(nrow(out), 3)
  expect_true(all(c("root[[1]]", "root[[1]]$children[[1]]", "root[[2]]") %in% out$path))
  expect_equal(out$depth[out$path == "root[[1]]"], 0)
  expect_equal(out$depth[out$path == "root[[1]]$children[[1]]"], 1)
})

test_that("missing sublists become NA", {
  node <- list(name = "NoAngles", position = list(x=1,y=2,z=3))
  out <- flatten_scenario_data(node)
  expect_true(is.na(out$eul_x))
  expect_equal(out$pos_y, 2)
})
