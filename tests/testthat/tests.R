
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
test_that("eulunity2rot gives correct outputs",
          {
            ## test basic rotations
            # clockwise rotation by pi/2 about x-axis converts y into z
            expect_equal(as.vector(eulunity2rot(90, 0, 0) %*% c(0, 1, 0)), c(0, 0, 1))
            # clockwise rotation by pi/2 about y-axis converts z into x
            expect_equal(as.vector(eulunity2rot(0, 90, 0) %*% c(0, 0, 1)), c(1, 0, 0))
            # clockwise rotation by pi/2 about z-axis converts x into y
            expect_equal(as.vector(eulunity2rot(0, 0, 90) %*% c(1, 0, 0)), c(0, 1, 0))
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
})

# -- proj_vec2vec --------------------------------------------------------------
test_that("proj_vec2vec gives correct outputs", {
  test_proj_vec2vec <- function(v1, v2, v3) {
    norm(proj_vec2vec(v1, v2) - v3, type = "2")
  }
  expect_equal(c(
    test_proj_vec2vec(c(1, 1, 1), c(1, 0, 0), c(1, 0, 0)),
    test_proj_vec2vec(c(2, 2, 2), c(0, 1, 0), c(0, 2, 0)),
    test_proj_vec2vec(c(1, 0, 0), c(1, 1, 0), c(sqrt(1/2), sqrt(1/2), 0)),
    test_proj_vec2vec(c(9, 9), c(1, 0), c(9, 0))
  ),
  c(0, 0, 0, 0))
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


# -- cart2sph ------------------------------------------------------------------
test_that("cart2sph gives the same output as pracma", {
  v <- runif(3)
  expect_equal(norm(cart2sph(v) - pracma::cart2sph(v), type = "2"), 0)
})

# -- create_resampling_index ---------------------------------------------------
test_that("create_resampling_index gives correct output", {
  expect_equal(norm(
    create_resampling_index(1, 10) - seq(from = .1, to = 1, by = .1),
    type = "2"
  ), 0)
})

# ------------------------------------------------------------------------------
# --- add_movement functions ------------------------------------------------------------
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
extract_var <- function(df, varname) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::select(tidyselect::all_of(varname)) %>%
    dplyr::pull()
}

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
  newdf1 <- df(pos = c(0, 0, 0), rot = c(0, 0, 0), time = .1)
  newdf2 <- df(pos = c(0, 0, 0), rot = c(34, 0, 0), time = .2)
  newdf  <- dplyr::bind_rows(newdf1, newdf2) %>%
    add_angular_diff()
  expect_equal(extract_var(newdf, "angular_diff"), 34)
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
test_that("add_gaze_elevation adds correct values", {
  # x rotation (clockwise) by 45 degrees corresponds to 45 degrees downward
  expect_equal(
      df(pos = c(0, 0, 0), rot = c(45, 0, 0)) %>%
        add_gaze_elevation() %>%
        extract_var("gaze_elevation"),
      -45
    )
  # other rotations have no impact on gaze elevation
  expect_equal(
    df(pos = c(0, 0, 0), rot = c(0, 45, 0)) %>%
      add_gaze_elevation() %>%
      extract_var("gaze_elevation"),
    0
  )
  expect_equal(
    df(pos = c(0, 0, 0), rot = c(0, 0, 45)) %>%
      add_gaze_elevation() %>%
      extract_var("gaze_elevation"),
    0
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

test_that("find_hierarchy_object_any finds first matched name", {
  test_hierarchy <- jsonlite::read_json("test_files/test_hierarchy.json")

  result <-
    find_hierarchy_object_any(test_hierarchy, c("Bush_LOD0", "Bush_LOD1"))

  expect_equal(result[["name"]], "Bush_LOD0")
})

test_that("find_hierarchy_object_any finds first matched name", {
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

