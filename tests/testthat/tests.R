
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


test_that("guess_move_at_pos works correctly", {
  move_from_origin <- dplyr::bind_rows(
    df(
      time = seq(from = 0, to = 1, by = .1),
      pos = c(0, 0, 0),
      rot = c(0, 0, 0)
    ),
    df(
      time = seq(from = 1.1, to = 2, by = .1),
      pos = c(0, 0, 10),
      rot = c(0, 0, 0)
    )
  )
  move_to_origin <- dplyr::bind_rows(
    df(
      time = seq(from = 0, to = 1, by = .1),
      pos = c(0, 0, 10),
      rot = c(0, 0, 0)
    ),
    df(
      time = seq(from = 1.1, to = 2, by = .1),
      pos = c(0, 0, 0),
      rot = c(0, 0, 0)
    )
  )

  move_from_origin_lat <- move_from_origin
  move_to_origin_lat <- move_to_origin

  move_from_origin_lat$pos_x <- 10
  move_to_origin_lat$pos_x <- 10

  pos_origin <- list(
    pos_x = 0,
    pos_y = 0,
    pos_z = 0
  )
  pos_forward <- list(
    pos_x = 0,
    pos_y = 0,
    pos_z = 10
  )

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



# -- reorient_movement ----------------------------------------------------

test_that("reorient_movement correctly applies 90-degree rotation", {
  # mock movement
  df <- tibble(
    pos_x = 1,
    pos_y = 0,
    pos_z = 0
  )
  
  #Rotation is anti-clockwise viewed from positive y in unity left handed system
  R <- matrix(c( 0, 0, -1, 
                 0, 1, 0,
                 1, 0, 0),
              ncol = 3,
              byrow = TRUE)

  # apply world unity
  result <- reorient_movement(
    df = df,
    R
  )

  # expected: +X becomes +Z
  expect_equal(result$pos_x, 0)
  expect_equal(result$pos_y, 0)
  expect_equal(result$pos_z, 1)
})

test_that("reorient_movement leaves coordinates unchanged with default transformation", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  result <- reorient_movement(df = df)

  expect_equal(result$pos_x, 1)
  expect_equal(result$pos_y, 2)
  expect_equal(result$pos_z, 3)
})


test_that("reorient_movement correctly rotates all three axes", {
  df <- tibble(
    pos_x = c(1, 0, 0),
    pos_y = c(0, 1, 0),
    pos_z = c(0, 0, 1)
  )

  R <- matrix(
    c(0, 0, -1,
      0, 1,  0,
      1, 0,  0),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  # +X -> +Z
  expect_equal(
    c(result$pos_x[1], result$pos_y[1], result$pos_z[1]),
    c(0, 0, 1)
  )

  # +Y -> +Y
  expect_equal(
    c(result$pos_x[2], result$pos_y[2], result$pos_z[2]),
    c(0, 1, 0)
  )

  # +Z -> -X
  expect_equal(
    c(result$pos_x[3], result$pos_y[3], result$pos_z[3]),
    c(-1, 0, 0)
  )
})


test_that("reorient_movement correctly applies translation", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  offset <- c(10, 20, 30)

  result <- reorient_movement(
    df = df,
    offset = offset
  )

  expect_equal(result$pos_x, 11)
  expect_equal(result$pos_y, 22)
  expect_equal(result$pos_z, 33)
})


test_that("reorient_movement applies translation after rotation", {
  df <- tibble(
    pos_x = 1,
    pos_y = 0,
    pos_z = 0
  )

  R <- matrix(
    c(0, 0, -1,
      0, 1,  0,
      1, 0,  0),
    ncol = 3,
    byrow = TRUE
  )

  offset <- c(10, 20, 30)

  result <- reorient_movement(
    df = df,
    R = R,
    offset = offset
  )

  # First: +X -> +Z = (0, 0, 1)
  # Then: add offset = (10, 20, 31)
  expect_equal(result$pos_x, 10)
  expect_equal(result$pos_y, 20)
  expect_equal(result$pos_z, 31)
})


test_that("reorient_movement handles negative coordinates", {
  df <- tibble(
    pos_x = -1,
    pos_y = -2,
    pos_z = -3
  )

  result <- reorient_movement(
    df = df,
    offset = c(10, 20, 30)
  )

  expect_equal(result$pos_x, 9)
  expect_equal(result$pos_y, 18)
  expect_equal(result$pos_z, 27)
})


test_that("reorient_movement handles zero coordinates", {
  df <- tibble(
    pos_x = 0,
    pos_y = 0,
    pos_z = 0
  )

  result <- reorient_movement(
    df = df,
    offset = c(10, 20, 30)
  )

  expect_equal(result$pos_x, 10)
  expect_equal(result$pos_y, 20)
  expect_equal(result$pos_z, 30)
})


test_that("reorient_movement handles multiple movement rows", {
  df <- tibble(
    pos_x = c(1, 2, 3),
    pos_y = c(4, 5, 6),
    pos_z = c(7, 8, 9)
  )

  result <- reorient_movement(
    df = df,
    offset = c(10, 20, 30)
  )

  expect_equal(result$pos_x, c(11, 12, 13))
  expect_equal(result$pos_y, c(24, 25, 26))
  expect_equal(result$pos_z, c(37, 38, 39))
})


test_that("reorient_movement preserves non-position columns", {
  df <- tibble(
    timestamp = c(1, 2, 3),
    pos_x = c(1, 2, 3),
    pos_y = c(4, 5, 6),
    pos_z = c(7, 8, 9),
    velocity = c(10, 11, 12)
  )

  result <- reorient_movement(
    df = df,
    offset = c(10, 20, 30)
  )

  expect_equal(result$timestamp, df$timestamp)
  expect_equal(result$velocity, df$velocity)

  expect_equal(result$pos_x, c(11, 12, 13))
  expect_equal(result$pos_y, c(24, 25, 26))
  expect_equal(result$pos_z, c(37, 38, 39))
})


test_that("reorient_movement preserves row count", {
  df <- tibble(
    pos_x = 1:10,
    pos_y = 11:20,
    pos_z = 21:30
  )

  result <- reorient_movement(df = df)

  expect_equal(nrow(result), nrow(df))
})


test_that("reorient_movement preserves column names", {
  df <- tibble(
    time = 1,
    pos_x = 2,
    pos_y = 3,
    pos_z = 4,
    other = 5
  )

  result <- reorient_movement(df = df)

  expect_equal(names(result), names(df))
})

test_that("reorient_movement accepts uppercase coordinate columns", {
  df <- tibble(
    pos_X = 1,
    pos_Y = 2,
    pos_Z = 3
  )

  result <- reorient_movement(
    df = df,
    offset = c(10, 20, 30)
  )

  expect_equal(result$pos_X, 11)
  expect_equal(result$pos_Y, 22)
  expect_equal(result$pos_Z, 33)
})


test_that("reorient_movement correctly applies a 180-degree rotation around y", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  R <- matrix(
    c(-1,  0,  0,
       0,  1,  0,
       0,  0, -1),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  expect_equal(result$pos_x, -1)
  expect_equal(result$pos_y,  2)
  expect_equal(result$pos_z, -3)
})


test_that("reorient_movement correctly applies a 90-degree rotation around x", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  R <- matrix(
    c(1, 0,  0,
      0, 0, -1,
      0, 1,  0),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  # (1, 2, 3) -> (1, -3, 2)
  expect_equal(result$pos_x,  1)
  expect_equal(result$pos_y, -3)
  expect_equal(result$pos_z,  2)
})


test_that("reorient_movement correctly applies a 90-degree rotation around z", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  R <- matrix(
    c(0, -1, 0,
      1,  0, 0,
      0,  0, 1),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  # (1, 2, 3) -> (-2, 1, 3)
  expect_equal(result$pos_x, -2)
  expect_equal(result$pos_y,  1)
  expect_equal(result$pos_z,  3)
})


test_that("reorient_movement identity rotation behaves like no rotation", {
  df <- tibble(
    pos_x = c(1, -2, 3),
    pos_y = c(4,  5, -6),
    pos_z = c(7, -8, 9)
  )

  R <- diag(3)

  result <- reorient_movement(
    df = df,
    R = R
  )

  expect_equal(result$pos_x, df$pos_x)
  expect_equal(result$pos_y, df$pos_y)
  expect_equal(result$pos_z, df$pos_z)
})


test_that("reorient_movement zero offset behaves like rotation only", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  R <- matrix(
    c(-1,  0,  0,
       0,  1,  0,
       0,  0, -1),
    ncol = 3,
    byrow = TRUE
  )

  result_without_offset <- reorient_movement(
    df = df,
    R = R
  )

  result_with_zero_offset <- reorient_movement(
    df = df,
    R = R,
    offset = c(0, 0, 0)
  )

  expect_equal(
    result_with_zero_offset,
    result_without_offset
  )
})


test_that("reorient_movement does not modify the input data frame", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  df_original <- df

  result <- reorient_movement(
    df = df,
    offset = c(10, 20, 30)
  )

  expect_equal(df, df_original)

  # Ensure the result was actually transformed
  expect_equal(result$pos_x, 11)
  expect_equal(result$pos_y, 22)
  expect_equal(result$pos_z, 33)
})


test_that("reorient_movement applies rotation consistently to arbitrary points", {
  df <- tibble(
    pos_x = c(1, 2, -3, 0),
    pos_y = c(2, -1, 4, 0),
    pos_z = c(3, 5, -2, 0)
  )

  R <- matrix(
    c(0, 0, -1,
      0, 1,  0,
      1, 0,  0),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  expected <- tibble(
    pos_x = c(-3, -5,  2, 0),
    pos_y = c( 2, -1,  4, 0),
    pos_z = c( 1,  2, -3, 0)
  )

  expect_equal(
    result[c("pos_x", "pos_y", "pos_z")],
    expected
  )
})


test_that("reorient_movement validates rotation matrix dimensions", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  expect_warning(
    reorient_movement(
      df = df,
      R = diag(2)
    )
  )

  expect_warning(
    reorient_movement(
      df = df,
      R = matrix(1, nrow = 2, ncol = 3)
    )
  )
})


test_that("reorient_movement validates offset length", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  expect_error(
    reorient_movement(
      df = df,
      offset = c(1, 2)
    )
  )

  expect_error(
    reorient_movement(
      df = df,
      offset = c(1, 2, 3, 4)
    )
  )
})


test_that("reorient_movement applies rotation before translation", {
  df <- tibble(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  # 180 degree rotation around y
  R <- matrix(
    c(-1, 0,  0,
       0, 1,  0,
       0, 0, -1),
    ncol = 3,
    byrow = TRUE
  )

  offset <- c(10, 20, 30)

  result <- reorient_movement(
    df = df,
    R = R,
    offset = offset
  )

  # Rotation:
  # (1, 2, 3) -> (-1, 2, -3)
  #
  # Translation:
  # (-1, 2, -3) + (10, 20, 30)
  # -> (9, 22, 27)
  expect_equal(result$pos_x, 9)
  expect_equal(result$pos_y, 22)
  expect_equal(result$pos_z, 27)
})

test_that("reorient_movement correctly rotates a position list", {
  df <- list(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  # 90-degree rotation around y
  # +X -> +Z and +Z -> -X
  R <- matrix(
    c(0, 0, -1,
      0, 1,  0,
      1, 0,  0),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  # (1, 2, 3) -> (-3, 2, 1)
  expect_equal(result$pos_x, -3)
  expect_equal(result$pos_y,  2)
  expect_equal(result$pos_z,  1)
})


test_that("reorient_movement correctly translates a position list", {
  df <- list(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  offset <- c(10, 20, 30)

  result <- reorient_movement(
    df = df,
    offset = offset
  )

  # (1, 2, 3) + (10, 20, 30) -> (11, 22, 33)
  expect_equal(result$pos_x, 11)
  expect_equal(result$pos_y, 22)
  expect_equal(result$pos_z, 33)
})


test_that("reorient_movement rotates and then translates a position list", {
  df <- list(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  # 90-degree rotation around y
  R <- matrix(
    c(0, 0, -1,
      0, 1,  0,
      1, 0,  0),
    ncol = 3,
    byrow = TRUE
  )

  offset <- c(10, 20, 30)

  result <- reorient_movement(
    df = df,
    R = R,
    offset = offset
  )

  # First rotate:
  # (1, 2, 3) -> (-3, 2, 1)
  #
  # Then translate:
  # (-3, 2, 1) + (10, 20, 30)
  # -> (7, 22, 31)
  expect_equal(result$pos_x, 7)
  expect_equal(result$pos_y, 22)
  expect_equal(result$pos_z, 31)
})


test_that("reorient_movement correctly handles negative position list values", {
  df <- list(
    pos_x = -1,
    pos_y = -2,
    pos_z = -3
  )

  R <- matrix(
    c(0, 0, -1,
      0, 1,  0,
      1, 0,  0),
    ncol = 3,
    byrow = TRUE
  )

  offset <- c(10, 20, 30)

  result <- reorient_movement(
    df = df,
    R = R,
    offset = offset
  )

  # Rotate:
  # (-1, -2, -3) -> (3, -2, -1)
  #
  # Translate:
  # (3, -2, -1) + (10, 20, 30)
  # -> (13, 18, 29)
  expect_equal(result$pos_x, 13)
  expect_equal(result$pos_y, 18)
  expect_equal(result$pos_z, 29)
})


test_that("reorient_movement correctly applies a 180-degree rotation to a position list", {
  df <- list(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  # 180-degree rotation around y
  R <- matrix(
    c(-1, 0,  0,
       0, 1,  0,
       0, 0, -1),
    ncol = 3,
    byrow = TRUE
  )

  result <- reorient_movement(
    df = df,
    R = R
  )

  # (1, 2, 3) -> (-1, 2, -3)
  expect_equal(result$pos_x, -1)
  expect_equal(result$pos_y,  2)
  expect_equal(result$pos_z, -3)
})


test_that("reorient_movement handles a position list with identity rotation and translation", {
  df <- list(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  R <- diag(3)
  offset <- c(-5, 10, -15)

  result <- reorient_movement(
    df = df,
    R = R,
    offset = offset
  )

  expect_equal(result$pos_x, -4)
  expect_equal(result$pos_y,  12)
  expect_equal(result$pos_z, -12)
})


test_that("reorient_movement preserves a position list with identity transformation", {
  df <- list(
    pos_x = 1,
    pos_y = 2,
    pos_z = 3
  )

  result <- reorient_movement(df = df)

  expect_equal(result$pos_x, 1)
  expect_equal(result$pos_y, 2)
  expect_equal(result$pos_z, 3)
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
  expect_equal(identify_na_columns(df, print_output = FALSE), c("a", "b"))

  # Test with return_counts = TRUE
  expect_equal(identify_na_columns(df, print_output = FALSE, return_counts = TRUE), c(a = 1, b = 1))

  # Test with no NAs
  df_no_na <- data.frame(a = 1:3, b = 4:6)
  expect_equal(identify_na_columns(df_no_na, print_output = FALSE), character(0))

  # Test with empty data frame
  df_empty <- data.frame()
  expect_equal(identify_na_columns(df_empty, print_output = FALSE), character(0))

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

# -- reorient_movement2unity_cols ----------------------------------------------------
