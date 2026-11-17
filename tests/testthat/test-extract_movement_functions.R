# ------------------------------------------------------------------------------
# --- extract functions --------------------------------------------------------
# ------------------------------------------------------------------------------

# -- extract_recording_duration ------------------------------------------------

test_that("extract_duration correctly extracts duration", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 20:30)
  expect_equal(
    test_df %>%
      extract_recording_duration(),
    10
  )
})

# -- extract_at_time -----------------------------------------------------------

test_that("extract_at_time can round down correctly", {
  test_df <- tibble::tibble(time = 0:10, test_col = 10:20)
  expect_equal(
    extract_at_time(test_df, "test_col", 1.05),
    11
  )
})

test_that("extract_at_time can round up correctly", {
  test_df <- tibble::tibble(time = 0:10, test_col = 10:20)
  expect_equal(
    extract_at_time(test_df, "test_col", 1.95),
    12
  )
})

# -- extract_direction ---------------------------------------------------------

test_that("extract_direction can calculate direction correctly for simple movement", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 0, pos_y = 0, pos_z = 0:10)
  expect_equal(
    extract_direction(test_df),
    list(dir_x = 0, dir_y = 0, dir_z = 1)
  )
  test_df <- tibble::tibble(time = 0:10, pos_x = 0, pos_y = 0:10, pos_z = 0:10)
  expect_equal(
    extract_direction(test_df),
    list(dir_x = 0, dir_y = sqrt(2) / 2, dir_z = sqrt(2) / 2)
  )
  test_df <- tibble::tibble(time = 0:10, pos_x = -1:-11, pos_y = 0:10, pos_z = 0:10)
  expect_equal(
    extract_direction(test_df),
    list(dir_x = -1 / sqrt(3), dir_y = 1 / sqrt(3), dir_z = 1 / sqrt(3))
  )
})

test_that("extract_direction min_time and max_time work correctly", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 0, pos_y = 0, pos_z = c(0:4, 5:0))
  # test averaging
  expect_equal(
    extract_direction(test_df),
    list(dir_x = 0, dir_y = 0, dir_z = 0)
  )
  # test min_time
  expect_equal(
    extract_direction(test_df, min_time = 5),
    list(dir_x = 0, dir_y = 0, dir_z = -1)
  )
  # test max_time
  expect_equal(
    extract_direction(test_df, max_time = 5),
    list(dir_x = 0, dir_y = 0, dir_z = 1)
  )
})

# -- extract_durations ---------------------------------------------------------

test_that("extract_durations can check for non-available column", {
  test_df <- tibble::tibble(time = 0:10, signal_0 = "a", signal_2 = "c")
  expect_equal(
    extract_durations(test_df, "signal_1"),
    NULL
  )
})

test_that("extract_durations correctly computes durations", {
  test_df <- tibble::tibble(time = 0:10, signal_0 = "a", signal_2 = "c")
  expect_equal(
    extract_durations(test_df, "signal_0"),
    tibble::tibble(signal_0 = "a", duration = 10)
  )

  test_df <- tibble::tibble(time = 0:10, signal_0 = "c", signal_2 = c(rep("a", 5), rep("b", 6)))
  expect_equal(
    extract_durations(test_df, "signal_2"),
    tibble::tibble(signal_2 = c("b", "a"), duration = c(5.5, 4.5))
  )

  test_df <- tibble(
    time  = seq(0, 10, 0.1),
    pos_x = c(seq(0, 5, 0.1), seq(5.2, 15, 0.2)),
    pos_y = 0,
    pos_z = 0
  )
  expect_equal(
    sum(extract_durations(add_speed(test_df), "speed")$duration),
    10
  )
})

test_that("extract_durations min_time, max_time work correctly", {
  test_df <- tibble::tibble(time = 0:10, signal_0 = "c", signal_2 = c(rep("a", 5), rep("b", 6)))
  expect_equal(
    extract_durations(test_df, "signal_2", min_time = 5),
    tibble::tibble(signal_2 = c("b"), duration = c(5.5))
  )

  test_df <- tibble::tibble(time = 0:10, signal_0 = "c", signal_2 = c(rep("a", 5), rep("b", 6)))
  expect_equal(
    extract_durations(test_df, "signal_2", max_time = 5),
    tibble::tibble(signal_2 = c("a", "b"), duration = c(4.5, 1))
  )
})

# -- extract_movement_dist -----------------------------------------------------

test_that("extract_movement_dist computes accurately with min, max and mean options", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 0:10, pos_y = 0, pos_z = 0)
  test_list <- list(pos_x = 0, pos_y = 0, pos_z = 0)

  # checks that default method is min as well
  expect_equal(
    extract_movement_dist(test_df, test_list),
    0
  )

  # method is max
  expect_equal(
    extract_movement_dist(test_df, test_list, method = "max"),
    10
  )

  # method is mean
  expect_equal(
    extract_movement_dist(test_df, test_list, method = "mean"),
    5.5
  )
})

test_that("extract_movement_dist computes accurately with min_time and max_time options", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 0:10, pos_y = 0, pos_z = 0)
  test_list <- list(pos_x = 0, pos_y = 0, pos_z = 0)

  # min_time
  expect_equal(
    extract_movement_dist(test_df, test_list, min_time = 5),
    5
  )

  test_df <- tibble::tibble(time = 0:10, pos_x = 10:0, pos_y = 0, pos_z = 0)
  # max_time
  expect_equal(
    extract_movement_dist(test_df, test_list, max_time = 4),
    6
  )
})

# -- extract_movement2_dist ----------------------------------------------------

test_that("extract_movement2_dist computes accurately with min, max and mean options", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 0:10, pos_y = 0, pos_z = 0)
  test_list <- tibble::tibble(pos_x = 0, pos_y = 0, pos_z = 0)

  # checks that default method is min as well
  expect_equal(
    extract_movement_dist(test_df, test_list),
    0
  )

  # method is max
  expect_equal(
    extract_movement_dist(test_df, test_list, method = "max"),
    10
  )

  # method is mean
  expect_equal(
    extract_movement_dist(test_df, test_list, method = "mean"),
    5.5
  )
})

test_that("extract_movement2_dist computes accurately with min_time and max_time options", {
  test_df <- tibble::tibble(time = 0:10, pos_x = 0:10, pos_y = 0, pos_z = 0)
  test_list <- tibble::tibble(pos_x = 0, pos_y = 0, pos_z = 0)

  # min_time
  expect_equal(
    extract_movement_dist(test_df, test_list, min_time = 5),
    5
  )

  test_df <- tibble::tibble(time = 0:10, pos_x = 10:0, pos_y = 0, pos_z = 0)
  # max_time
  expect_equal(
    extract_movement_dist(test_df, test_list, max_time = 4),
    6
  )
})

# -- extract_path_length -------------------------------------------------------

test_that("extract_path_length computes correctly", {
  test_df <- tibble(time = 0:10, pos_x = 0:10, pos_y = 0, pos_z = 0)
  expect_equal(
    extract_path_length(test_df),
    10
  )

  # Path length is 2D, so must only use pos_x and pos_z
  test_df <- tibble(time = 0:10, pos_x = 0:10, pos_y = 0:10, pos_z = 0:10)
  expect_equal(
    extract_path_length(test_df),
    10 * sqrt(2)
  )
})

test_that("extract_path_length min_time and max_time works correctly", {
  test_df <- tibble(time = 0:10, pos_x = 0:10, pos_y = 0, pos_z = c(rep(0, 6), 1:5))

  expect_equal(
    extract_path_length(test_df),
    5 + sqrt(2) * 5
  )

  expect_equal(
    extract_path_length(test_df, min_time = 5),
    sqrt(2) * 5
  )

  expect_equal(
    extract_path_length(test_df, max_time = 5),
    5
  )
})

# -- extract_pupil -------------------------------------------------------------

test_that("extract_pupil computes mean correctly with default settings", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = rep(4, 11),
    pupil_diameter_right = rep(6, 11),
    eye_openness_left = rep(1, 11),
    eye_openness_right = rep(1, 11)
  )

  # left and right have equal NA proportions. Default combination "best" defaults to left
  expect_equal(
    extract_pupil(test_df),
    4
  )
})

test_that("extract_pupil method argument mean max min works correctly", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = 1:11,
    pupil_diameter_right = 11:1,
    eye_openness_left = rep(1, 11),
    eye_openness_right = rep(1, 11)
  )

  expect_equal(
    extract_pupil(test_df, method = "mean", combination = "left"),
    mean(1:11)
  )

  expect_equal(
    extract_pupil(test_df, method = "max", combination = "left"),
    11
  )

  expect_equal(
    extract_pupil(test_df, method = "min", combination = "left"),
    1
  )
})

test_that("extract_pupil min_time and max_time work correctly", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = 1:11,
    pupil_diameter_right = 1:11,
    eye_openness_left = rep(1, 11),
    eye_openness_right = rep(1, 11)
  )

  expect_equal(
    extract_pupil(test_df, min_time = 5, combination = "left"),
    mean(6:11)
  )

  expect_equal(
    extract_pupil(test_df, max_time = 5, combination = "left"),
    mean(1:6)
  )

  expect_equal(
    extract_pupil(test_df, min_time = 3, max_time = 7, combination = "left"),
    mean(4:8)
  )
})

test_that("extract_pupil openness filtering works correctly", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = c(rep(5, 6), rep(0, 5)),
    pupil_diameter_right = c(rep(5, 6), rep(0, 5)),
    eye_openness_left = c(rep(1, 6), rep(0.4, 5)),
    eye_openness_right = rep(1, 11)
  )

  # left eye has some values filtered out
  expect_equal(
    extract_pupil(test_df, combination = "left"),
    5
  )

  # right eye unaffected
  expect_equal(
    extract_pupil(test_df, combination = "right"),
    30 / 11
  )
})

test_that("extract_pupil best eye selection prefers fewer NAs", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = c(rep(NA, 5), rep(4, 6)),
    pupil_diameter_right = rep(6, 11),
    eye_openness_left = rep(1, 11),
    eye_openness_right = rep(1, 11)
  )

  # right eye has fewer NAs
  expect_equal(
    extract_pupil(test_df, combination = "best"),
    6
  )
})

test_that("extract_pupil combined averages left and right correctly", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = rep(4, 11),
    pupil_diameter_right = rep(6, 11),
    eye_openness_left = rep(1, 11),
    eye_openness_right = rep(1, 11)
  )

  expect_equal(
    extract_pupil(test_df, combination = "combined"),
    5
  )
})

test_that("extract_pupil combined warns and falls back to best if NA proportions differ", {
  test_df <- tibble(
    time = 0:10,
    pupil_diameter_left = c(rep(NA, 6), rep(4, 5)),
    pupil_diameter_right = rep(6, 11),
    eye_openness_left = rep(1, 11),
    eye_openness_right = rep(1, 11)
  )

  expect_warning(
    result <- extract_pupil(test_df, combination = "combined")
  )

  # falls back to best -> right eye
  expect_equal(result, 6)
})

# -- extract_speed -------------------------------------------------------------

test_that("extract_speed computes average speed correctly", {
  # Constant 3D motion: 1 unit per second in x, y, z
  test_df <- tibble(
    time  = seq(0, 10, 0.1),
    pos_x = seq(0, 10, 0.1),
    pos_y = seq(0, 10, 0.1),
    pos_z = seq(0, 10, 0.1)
  )

  expect_equal(
    extract_speed(test_df),
    sqrt(3)
  )
})

test_that("extract_speed distinguishes mean and peak speed", {
  test_df <- tibble(
    time  = seq(0, 10, 0.1),
    pos_x = c(seq(0, 5, 0.1), seq(5.2, 15, 0.2)),
    pos_y = 0,
    pos_z = 0
  )

  expect_equal(
    extract_speed(test_df, method = "max"),
    2
  )

  # switch over is in favour of speed 2
  speed_1_obs_count <- length(seq(0, 5, 0.1)) - 1
  speed_2_obs_count <- length(seq(5.2, 15, 0.2)) + 1

  expect_equal(
    extract_speed(test_df),
    mean(c(rep(1, speed_1_obs_count), rep(2, speed_2_obs_count)))
  )
})

test_that("extract_speed min_time and max_time work correctly", {
  test_df <- tibble(
    time  = seq(0, 10, 0.1),
    pos_x = seq(0, 10, 0.1),
    pos_y = 0,
    pos_z = c(rep(0, 50), seq(0, 5, 0.1))
  )

  # From t = 0–5: speed = 1
  # From t = 5–10: speed = sqrt(2)

  expect_equal(
    extract_speed(test_df),
    mean(c(rep(1, 50), rep(sqrt(2), 51)))
  )

  expect_equal(
    extract_speed(test_df, min_time = 5.1),
    sqrt(2)
  )

  expect_equal(
    extract_speed(test_df, max_time = 4.9),
    1
  )
})

# -- extract_sequence ----------------------------------------------------------

test_that("extract_sequence returns full column by default", {
  test_df <- tibble(
    time = 0:10,
    value = 11:21
  )

  expect_equal(
    extract_sequence(test_df, "value"),
    11:21
  )
})

test_that("extract_sequence respects min_time and max_time", {
  test_df <- tibble(
    time = 0:10,
    value = 0:10
  )

  expect_equal(
    extract_sequence(test_df, "value", min_time = 3, max_time = 6),
    3:6
  )
})

test_that("extract_sequence returns correct type", {
  test_df <- tibble(
    time = 0:4,
    value = as.complex(1:5)
  )

  result <- extract_sequence(test_df, "value")

  expect_type(result, "complex")
  expect_equal(result, as.complex(1:5))
})

test_that("extract_sequence returns empty vector when no rows match", {
  test_df <- tibble(
    time = 0:10,
    value = 0:10
  )

  result <- extract_sequence(
    test_df,
    "value",
    min_time = 20,
    max_time = 30
  )

  expect_equal(result, NA)
})

# -- extract_speed_angular -----------------------------------------------------

test_that("extract_speed_angular returns zero for constant direction", {
  test_df <- tibble(
    time = seq(0, 1, by = 0.1),
    rot_x = 1,
    rot_y = 0,
    rot_z = 0
  )

  expect_equal(
    extract_speed_angular(test_df),
    0
  )
})

test_that("extract_speed_angular computes non-zero angular speed for rotation", {
  test_df <- tibble(
    time = seq(0, 1, by = 0.1),
    rot_x = seq(0, 10, 1),
    rot_y = 0,
    rot_z = 0
  )

  result <- extract_speed_angular(test_df)

  expect_equal(result, 10)
  expect_type(result, "double")
})

test_that("extract_speed_angular respects min_time and max_time", {
  test_df <- tibble(
    time = seq(0, 5, by = 0.1),
    rot_x = seq(0, 50, 1),
    rot_y = c(rep(0, 25), seq(0, 2.5, 0.1)),
    rot_z = 0
  )

  full_speed <- extract_speed_angular(test_df)

  partial_speed <- extract_speed_angular(
    test_df,
    min_time = 1,
    max_time = 2
  )

  expect_equal(partial_speed, 10)
  expect_true(partial_speed < full_speed)
})

test_that("extract_speed_angular method = 'max' returns >= mean", {
  test_df <- tibble(
    time = seq(0, 1, by = 0.1),
    rot_x = seq(10, 12, 0.2),
    rot_y = seq(5, 6, 0.1),
    rot_z = 0
  )

  mean_speed <- extract_speed_angular(
    test_df,
    method = "mean"
  )

  max_speed <- extract_speed_angular(
    test_df,
    method = "max"
  )

  expect_true(max_speed >= mean_speed)
})

test_that("extract_speed_angular works with gaze direction columns", {
  test_df <- tibble(
    time = seq(0, 1, by = 0.1),
    gaze_direction_x = cos(seq(0, pi / 2, length.out = 11)),
    gaze_direction_y = sin(seq(0, pi / 2, length.out = 11)),
    gaze_direction_z = 0
  )

  result <- extract_speed_angular(test_df)

  expect_equal(result, 90)
})

test_that("extract_speed_angular returns NA for insufficient data", {
  test_df <- tibble(
    time = 0,
    rot_x = 1,
    rot_y = 0,
    rot_z = 0
  )

  expect_warning(result <- extract_speed_angular(test_df))

  expect_true(is.na(result))
})

# -- extract_time_bins ---------------------------------------------------------

test_that("extract_time_bins returns total number of rows by default", {
  test_df <- tibble(
    time = 0:10,
    pos_x = rnorm(11)
  )

  expect_equal(
    extract_time_bins(test_df),
    11L
  )
})

test_that("extract_time_bins returns integer", {
  test_df <- tibble(
    time = seq(0, 1, by = 0.1),
    pos_x = rnorm(11)
  )

  result <- extract_time_bins(test_df)

  expect_type(result, "integer")
})

test_that("extract_time_bins respects min_time and max_time", {
  test_df <- tibble(
    time = 0:10,
    pos_x = rnorm(11)
  )

  expect_equal(
    extract_time_bins(test_df, min_time = 3, max_time = 6),
    4L
  )
})


test_that("extract_time_bins returns zero when no rows match", {
  test_df <- tibble(
    time = 0:10,
    pos_x = rnorm(11)
  )

  expect_equal(
    extract_time_bins(test_df, min_time = 20, max_time = 30),
    0L
  )
})

# -- extract_timestamp_duplicates ----------------------------------------------

test_that("extract_timestamp_duplicates returns zero when no duplicates exist", {
  test_df <- tibble(
    time = 0:5,
    n = rep(1, 6)
  )

  expect_equal(
    extract_timestamp_duplicates(test_df),
    0
  )
})

test_that("extract_timestamp_duplicates returns max duplicates by default", {
  test_df <- tibble(
    time = c(0, 1, 2, 3),
    n = c(1, 2, 1, 3)
  )

  # duplicates per timestamp = n - 1
  # max = max(c(0, 1, 0, 2)) = 2
  expect_equal(
    extract_timestamp_duplicates(test_df),
    3
  )
})

test_that("extract_timestamp_duplicates method = 'total' works correctly", {
  test_df <- tibble(
    time = c(0, 1, 2, 3),
    n = c(1, 2, 1, 3)
  )

  # total duplicates = (2 - 1) + (3 - 1) = 3
  expect_equal(
    extract_timestamp_duplicates(test_df, method = "total"),
    5
  )
})

test_that("extract_timestamp_duplicates method = 'number' works correctly", {
  test_df <- tibble(
    time = c(0, 1, 2, 3),
    n = c(1, 2, 1, 3)
  )

  # duplicated timestamps = times with n > 1
  expect_equal(
    extract_timestamp_duplicates(test_df, method = "number"),
    2
  )
})

test_that("extract_timestamp_duplicates respects min_time and max_time", {
  test_df <- tibble(
    time = 0:5,
    n = c(1, 2, 1, 3, 1, 1)
  )

  expect_equal(
    extract_timestamp_duplicates(
      test_df,
      min_time = 1,
      max_time = 2
    ),
    2
  )
})

test_that("extract_timestamp_duplicates returns NA when no rows match time window", {
  test_df <- tibble(
    time = 0:5,
    n = c(1, 2, 1, 3, 1, 1)
  )

  expect_equal(
    extract_timestamp_duplicates(
      test_df,
      min_time = 10,
      max_time = 20
    ),
    NA_real_
  )
})

# -- extract_fixation -----------------------------------------------------------

test_that("extract_fixation computes fixation ratio correctly", {
  test_df <- tibble(
    time = 0:10,
    focus_object_raw = c(
      "target", "target", "other", "target", "other",
      "other", "target", "other", "other", "other", "other"
    )
  )

  # Durations take half of the previous and next intervals around
  # a timestamp
  expect_equal(
    extract_fixation(test_df, target = "target"),
    0.35
  )
})

test_that("extract_fixation min_time and max_time work correctly", {
  test_df <- tibble(
    time = 0:9,
    focus_object_raw = c(
      "target", "target", "other", "target", "other",
      "other", "target", "other", "other", "other"
    )
  )

  # time 3 to 7 inclusive: 2 samples
  # target at time 3 and 6
  expect_equal(
    extract_fixation(test_df, target = "target", min_time = 3, max_time = 7),
    1.5 / 4
  )

  # from time 5 onward: 5 samples, 1 target
  expect_equal(
    extract_fixation(test_df, target = "target", min_time = 5),
    1 / 4
  )

  # up to time 2: 3 samples, 2 targets
  expect_equal(
    extract_fixation(test_df, target = "target", max_time = 2),
    1.5 / 2
  )
})

test_that("extract_fixation returns 0 when target is never fixated", {
  test_df <- tibble(
    time = 0:4,
    focus_object_raw = c("a", "b", "c", "d", "e")
  )

  expect_equal(
    extract_fixation(test_df, target = "target"),
    0
  )
})

test_that("extract_fixation returns 1 when target is always fixated", {
  test_df <- tibble(
    time = 0:4,
    focus_object_raw = rep("target", 5)
  )

  expect_equal(
    extract_fixation(test_df, target = "target"),
    1
  )
})

test_that("extract_fixation handles empty time window", {
  test_df <- tibble(
    time = 0:4,
    focus_object_raw = rep("target", 5)
  )

  expect_equal(
    extract_fixation(test_df, target = "target", min_time = 10),
    NA_real_
  )
})

# -- extract_gaze_elevation ----------------------------------------------------

test_that("extract_gaze_elevation computes correctly for simple gaze vectors", {
  # Forward gaze only elevation should be 0
  test_df <- tibble(
    time = 0:10,
    gaze_direction_x = rep(0, 11),
    gaze_direction_y = rep(0, 11),
    gaze_direction_z = rep(1, 11)
  )

  expect_equal(
    extract_gaze_elevation(test_df),
    0
  )

  # Straight upward gaze elevation should be 90 degrees
  test_df <- tibble(
    time = 0:10,
    gaze_direction_x = rep(0, 11),
    gaze_direction_y = rep(1, 11),
    gaze_direction_z = rep(0, 11)
  )

  expect_equal(
    extract_gaze_elevation(test_df),
    90
  )
})

test_that("extract_gaze_elevation averages correctly over time", {
  # Half forward (0 degrees), half upward (90 degrees)
  test_df <- tibble(
    time = 0:9,
    gaze_direction_x = 0,
    gaze_direction_y = c(rep(0, 5), rep(1, 5)),
    gaze_direction_z = c(rep(1, 5), rep(0, 5))
  )

  expect_equal(
    extract_gaze_elevation(test_df, samplingrate = 1),
    mean(c(rep(0, 5), rep(90, 5)))
  )
})

test_that("extract_gaze_elevation min_time and max_time work correctly", {
  test_df <- tibble(
    time = 0:9,
    gaze_direction_x = 0,
    gaze_direction_y = c(rep(0, 5), rep(1, 5)),
    gaze_direction_z = c(rep(1, 5), rep(0, 5))
  )

  # Only forward gaze period
  expect_equal(
    extract_gaze_elevation(test_df, max_time = 4),
    0
  )

  # Only upward gaze period
  expect_equal(
    extract_gaze_elevation(test_df, min_time = 5),
    90
  )
})

# the direction column is not utilized if gaze_direction is provided as gaze_ray
test_that("extract_gaze_elevation direction works correctly with gaze data", {
  # Half forward (0 degrees), half upward (90 degrees)
  test_df <- tibble(
    time = 0:9,
    gaze_direction_x = 0,
    gaze_direction_y = c(rep(0, 5), rep(1, 5)),
    gaze_direction_z = c(rep(1, 5), rep(0, 5))
  )

  expect_equal(
    extract_gaze_elevation(test_df, samplingrate = 1, direction = c(1, 0, 0)),
    45
  )
})


test_that("extract_gaze_elevation direction works correctly with movement tibbles", {
  # data means fixed 90deg rotation to the neutral position in unity coords (left handed)
  test_df <- tibble(
    time = 0:10,
    rot_x = 90,
    rot_y = 0,
    rot_z = 0
  )

  expect_equal(
    extract_gaze_elevation(test_df, samplingrate = 1, direction = c(1, 0, 0)),
    0
  )

  expect_equal(
    extract_gaze_elevation(test_df, samplingrate = 1, direction = c(0, 0, 1)),
    -90
  )

  expect_equal(
    extract_gaze_elevation(test_df, samplingrate = 1, direction = c(0, 1, 1)),
    -45
  )

  expect_equal(
    extract_gaze_elevation(test_df, samplingrate = 1, direction = c(0, 1, 0)),
    0
  )
})

test_that("extract_gaze_elevation works with rotation columns", {
  # Assuming rot_x encodes elevation directly for small angles
  test_df <- tibble(
    time = 0:10,
    rot_x = rep(0.1, 11),
    rot_y = 0,
    rot_z = 0
  )

  # rotation is clockwise as viewed from the positive axis.
  # unity is left handed with the thumb being y and the index finger as z.
  expect_equal(
    extract_gaze_elevation(test_df),
    -0.1
  )
})

# -- extract_orientation -------------------------------------------------------

test_that("extract_orientation computes cosine orientation correctly", {
  # Reference moves along +X, facing +Z (rot_y = 0 assumed facing +X)
  test_df <- tibble(
    time  = 0:10,
    pos_x = 0:10,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target is straight ahead on +X axis
  target <- list(pos_x = 15, pos_y = 0, pos_z = 0)

  # Always 90 degrees off
  expect_equal(
    extract_orientation(test_df, target),
    0
  )
})


test_that("extract_orientation returns 0 cosine when orthogonal", {
  test_df <- tibble(
    time  = 0:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target is straight ahead on +Z
  target <- list(pos_x = 0, pos_y = 0, pos_z = 10)

  expect_equal(
    extract_orientation(test_df, target),
    1
  )
})


test_that("extract_orientation returns absolute angle when convert2cos = FALSE", {
  test_df <- tibble(
    time  = 0:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target at (1,1) 45 degrees
  target <- list(pos_x = 10, pos_y = 0, pos_z = 10)

  expect_equal(
    extract_orientation(test_df, target, convert2cos = FALSE),
    pi / 4
  )
})


test_that("extract_orientation works min_time, max_time and rot data", {
  # First half aligned (cos = 1), second half orthogonal (cos = 0)
  test_df <- tibble(
    time  = 1:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 45 * rep(0:1, each = 5), # sequence is 0,0,0,0,0,45,45,45,45,45
    rot_z = 0
  )

  target <- list(pos_x = 0, pos_y = 0, pos_z = 10) # aligned

  # Whole duration average: (1/sqrt(2) + 1)/2
  expect_equal(
    extract_orientation(test_df, target),
    (1 / sqrt(2) + 1) / 2
  )

  # First half
  expect_equal(
    extract_orientation(test_df, target, max_time = 5),
    1
  )

  # Second half
  expect_equal(
    extract_orientation(test_df, target, min_time = 6),
    1 / sqrt(2)
  )
})

# -- extract_orientation2 ------------------------------------------------------

test_that("extract_orientation2 computes cosine orientation correctly (static target)", {
  # Reference at origin, facing +Z
  ref_df <- tibble(
    time  = 0:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target stationary straight ahead (+Z)
  target_df <- tibble(
    time  = 0:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 10,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Always perfectly aligned therefore cosine = 1
  expect_equal(
    extract_orientation2(ref_df, target_df),
    1
  )
})


test_that("extract_orientation2 returns 0 cosine when orthogonal", {
  ref_df <- tibble(
    time  = 0:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target along +X = 90deg offset from +Z forward
  target_df <- tibble(
    time  = 0:10,
    pos_x = 10,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  expect_equal(
    extract_orientation2(ref_df, target_df),
    0
  )
})


test_that("extract_orientation2 returns absolute angle when convert2cos = FALSE", {
  ref_df <- tibble(
    time  = 0:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target at 45 in XZ plane
  target_df <- tibble(
    time  = 0:10,
    pos_x = 10,
    pos_y = 0,
    pos_z = 10,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  expect_equal(
    extract_orientation2(ref_df, target_df, convert2cos = FALSE),
    pi / 4
  )
})


test_that("extract_orientation2 works with moving target", {
  # Reference always facing +Z
  ref_df <- tibble(
    time  = 1:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Target moves: first half +Z (aligned), second half +X (orthogonal)
  target_df <- tibble(
    time  = 1:10,
    pos_x = c(rep(0, 5), rep(10, 5)),
    pos_y = 0,
    pos_z = c(rep(10, 5), rep(0, 5)),
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # Whole duration average: (1 + 0) / 2
  expect_equal(
    extract_orientation2(ref_df, target_df),
    0.5
  )

  # First half only
  expect_equal(
    extract_orientation2(ref_df, target_df, max_time = 5),
    1
  )

  # Second half only
  expect_equal(
    extract_orientation2(ref_df, target_df, min_time = 6),
    0
  )
})


test_that("extract_orientation2 correctly incorporates rotation (rot_y)", {
  # Reference rotates halfway through
  ref_df <- tibble(
    time  = 1:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 0,
    rot_x = 0,
    rot_y = 45 * rep(0:1, each = 5), # 0, then 45
    rot_z = 0
  )

  # Target always straight ahead (+Z)
  target_df <- tibble(
    time  = 1:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = 10,
    rot_x = 0,
    rot_y = 0,
    rot_z = 0
  )

  # First half cos = 1
  expect_equal(
    extract_orientation2(ref_df, target_df, max_time = 5),
    1
  )

  # Second half cos = cos(45) = 1/sqrt(2)
  expect_equal(
    extract_orientation2(ref_df, target_df, min_time = 6),
    1 / sqrt(2)
  )

  # Whole duration average
  expect_equal(
    extract_orientation2(ref_df, target_df),
    (1 + 1 / sqrt(2)) / 2
  )
})

# -- extract_at_distance -------------------------------------------------------

test_that("extract_at_distance works with extract_orientation", {
  # First half aligned (cos = 1), second half orthogonal (cos = 0)
  test_df <- tibble(
    time  = 1:10,
    pos_x = 0,
    pos_y = 0,
    pos_z = -10 * rep(0:1, each = 5), # jump backwards 10 meters 5 seconds in
    rot_x = 0,
    rot_y = 45 * rep(0:1, each = 5), # sequence is 0,0,0,0,0,45,45,45,45,45
    rot_z = 0
  )

  target <- list(pos_x = 0, pos_y = 0, pos_z = 10) # aligned

  # maximum distance prevents orientation after jump from being counted
  expect_equal(
    extract_at_distance(test_df, test_df, list(pos_x = 0, pos_y = 0, pos_z = 0), 5, extract_orientation, "mean", target),
    1
  )

  # All time included
  expect_equal(
    extract_at_distance(test_df, test_df, list(pos_x = 0, pos_y = 0, pos_z = 0), 15, extract_orientation, "mean", target),
    (1 + 1 / sqrt(2)) / 2
  )
})

# -- extract_with_timemask -----------------------------------------------------

test_that("extract_with_timemask works correctly with speed data and argument passing", {
  test_df <- tibble(
    time  = seq(0, 10, 0.1),
    pos_x = c(seq(0, 5, 0.1), seq(5.2, 15, 0.2)),
    pos_y = 0,
    pos_z = 0
  )

  mask1 <- extract_timemask(test_df, time > 5)
  expect_equal(
    extract_with_timemask(extract_speed, mask1)(test_df),
    2
  )

  # switch over is in favour of speed 2
  speed_1_obs_count <- length(seq(0, 5, 0.1)) - 1
  speed_2_obs_count <- length(seq(5.2, 15, 0.2)) + 1

  # See if can extract on entire time
  mask2 <- extract_timemask(test_df, time >= 0)
  expect_equal(
    extract_with_timemask(extract_speed, mask2)(test_df),
    mean(c(rep(1, speed_1_obs_count), rep(2, speed_2_obs_count)))
  )

  # See if can extract on entire time with argument passing
  mask3 <- extract_timemask(test_df, time >= 0)
  expect_equal(
    extract_with_timemask(extract_speed, mask3)(test_df, method = "max"),
    2
  )
})

# -- extract_fruit_task -------------------------------------------------------------
test_that("extract_fruit_task counts collected fruit correctly (sum)", {
  test_df <- tibble(
    time = 1:10,
    event = c(
      "appear", "collect", "collect", "appear",
      "collect", "appear", "collect", "collect",
      "appear", "collect"
    )
  )

  # There are 6 "collect" events
  expect_equal(
    extract_fruit_task(test_df),
    6
  )
})

test_that("extract_fruit_task ignores 'appear' events", {
  test_df <- tibble(
    time  = 1:5,
    event = rep("appear", 5)
  )

  expect_equal(
    extract_fruit_task(test_df),
    0
  )
})

test_that("extract_fruit_task respects min_time and max_time (sum)", {
  test_df <- tibble(
    time = 1:10,
    event = c(
      "collect", "collect", "collect", "collect", "collect",
      "collect", "collect", "collect", "collect", "collect"
    )
  )

  # Whole duration
  expect_equal(
    extract_fruit_task(test_df),
    10
  )

  # First half
  expect_equal(
    extract_fruit_task(test_df, max_time = 5),
    5
  )

  # Second half
  expect_equal(
    extract_fruit_task(test_df, min_time = 6),
    5
  )
})


test_that("extract_fruit_task computes rate correctly when time window provided", {
  test_df <- tibble(
    time  = c(1, 2, 3, 6, 8),
    event = c("collect", "collect", "collect", "collect", "collect")
  )

  # 5 fruits collected between time 1 and 11 → duration = 10s
  expect_equal(
    extract_fruit_task(test_df, min_time = 1, max_time = 11, method = "rate"),
    5 / 10
  )
})


test_that("extract_fruit_task rate respects time window filtering", {
  test_df <- tibble(
    time  = 1:10,
    event = rep("collect", 10)
  )

  # 6 collections in duration 6-1 i.e 5 seconds
  expect_equal(
    extract_fruit_task(test_df, min_time = 1, max_time = 6, method = "rate"),
    6 / 5
  )

  # 5 collects in 5 seconds (collections at 6, 7, 8, 9 and 10)
  expect_equal(
    extract_fruit_task(test_df, min_time = 6, max_time = 11, method = "rate"),
    1
  )
})
