# ------------------------------------------------------------------------------
# --- maths functions ----------------------------------------------------------
# ------------------------------------------------------------------------------

# -- angle_diff_deg() ----------------------------------------------------------
test_that("Angle difference is calculated correctly", {
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
  expect_equal(
    c(
      calculate_2d_dist(1, 1, 2, 1),
      calculate_2d_dist(2, 0)
    ),
    c(1, 2)
  )
})


# -- timeseries_mean() ---------------------------------------------------------
test_that("Mean of timeseries is calculated correctly", {
  expect_equal(
    timeseries_mean(1:5, 1:5),
    3
  )
  expect_equal(
    timeseries_mean(1:5, 101:105),
    3
  )
  expect_equal(
    timeseries_mean(seq(10, 50, by = 10), 1:5),
    30
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
test_that("eulunity2rot clockwise rotation by pi/2 about x-axis converts y into z", {
  expect_equal(as.vector(eulunity2rot(90, 0, 0) %*% c(0, 1, 0)), c(0, 0, 1))
})


test_that("eulunity2rot clockwise rotation by pi/2 about y-axis converts z into x", {
  expect_equal(as.vector(eulunity2rot(0, 90, 0) %*% c(0, 0, 1)), c(1, 0, 0))
})

test_that("eulunity2rot clockwise rotation by pi/2 about z-axis converts x into y", {
  expect_equal(as.vector(eulunity2rot(0, 0, 90) %*% c(1, 0, 0)), c(0, 1, 0))
})

test_that("eulunity2rot gives correct outputs", {
  ## test compositions of basic rotations
  # a simple composition
  expect_equal(as.vector(eulunity2rot(90, 0, 90) %*% c(1, 0, 0)), c(0, 0, 1))
  # a composition with two rotations that leave the rotated vector unchanged only when
  # rotated in correct order
  expect_equal(as.vector(eulunity2rot(90, 133, 39) %*% c(0, 0, 1)), c(0, -1, 0))
})

# -- rot2eulunity --------------------------------------------------------------
test_that("rot2eulunity is an inverse for eulunity2rot", {
  sample <- example_data$head_movement_data_0[[2]]
  sample <- sample %>% rowwise() %>% 
            mutate(R = list(eulunity2rot(rot_x, rot_y, rot_z))) %>% 
            mutate(new_rot_x = unname(rot2eulunity(R))[1],
                   new_rot_y = unname(rot2eulunity(R))[2],
                   new_rot_z = unname(rot2eulunity(R))[3])
  
  expect_equal(sample$rot_x, sample$new_rot_x)
  expect_equal(sample$rot_y, sample$new_rot_y)
  expect_equal(sample$rot_z, sample$new_rot_z)
})

# -- vec2rot() -----------------------------------------------------------------

test_that("vec2rot gives correct output for norm vectors", {
  test_vec2rot <- function() {
    x <- runif(3)
    x <- x / norm(x, type = "2")
    y <- runif(3)
    y <- y / norm(y, type = "2")
    R <- vec2rot(x, y)
    yhat <- R %*% x
    sum((yhat - y)^2)
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

test_that("vec2rot handles identical inputs", {
  x <- runif(3)
  x <- x / norm(x, type = "2")
  result <- vec2rot(x, x)
  expect_equal(result, diag(3))
})

# -- angle_between -------------------------------------------------------------
test_that("angle_between_vec gives correct outputs", {
  expect_equal(angle_between(c(1, 0, 0), c(0, 1, 0)), 90)
  expect_equal(angle_between(c(1, 0, 0), c(1, 1, 0)), 45)
  expect_equal(angle_between(c(-1, 0, 0), c(1, 0, 0)), 180)
  expect_equal(angle_between(c(1, 0, 0), c(1, 0, 0)), 0)
})

# -- rot2centralangle ----------------------------------------------------------
test_that("rot2centralangle gives correct outputs", {
  # exchange x and y axis rotates x axis by 90 deg
  expect_equal(
    rot2centralangle(
      diag(3),
      matrix(
        c(0, 1, 0, 1, 0, 0, 0, 0, 1),
        ncol = 3, byrow = TRUE
      ),
      c(1, 0, 0)
    ),
    90
  )
  # exchange x and y axis rotates z axis by 0 deg
  expect_equal(
    rot2centralangle(
      diag(3),
      matrix(
        c(0, 1, 0, 1, 0, 0, 0, 0, 1),
        ncol = 3, byrow = TRUE
      ),
      c(0, 0, 1)
    ),
    0
  )
  # output does not depend on magnitude of direction vector
  expect_equal(
    rot2centralangle(
      diag(3),
      matrix(
        c(0, 1, 0, 1, 0, 0, 0, 0, 1),
        ncol = 3, byrow = TRUE
      ),
      c(1, 0, 0)
    ),
    rot2centralangle(
      diag(3),
      matrix(
        c(0, 1, 0, 1, 0, 0, 0, 0, 1),
        ncol = 3, byrow = TRUE
      ),
      c(3, 0, 0)
    )
  )

  # Rotation by 180 degrees around the z axis
  expect_equal(
    rot2centralangle(
      diag(3),
      matrix(c(-1, 0, 0, 0, -1, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
      c(1, 0, 0)
    ),
    180
  )

  # Output does not depend on magnitude of direction vector
  expect_equal(
    rot2centralangle(
      diag(3),
      matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
      c(1, 0, 0)
    ),
    rot2centralangle(
      diag(3),
      matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
      c(3, 0, 0)
    )
  )

  # Negative rotation angle
  expect_equal(
    rot2centralangle(
      diag(3),
      matrix(c(0, -1, 0, -1, 0, 0, 0, 0, 1), ncol = 3, byrow = TRUE),
      c(1, 0, 0)
    ),
    90
  )
})

# -- proj_vec2vec --------------------------------------------------------------
test_that("proj_vec2vec gives correct outputs", {
  test_proj_vec2vec <- function(v1, v2, v3) {
    norm(proj_vec2vec(v1, v2) - v3, type = "2")
  }
  expect_equal(
    c(
      test_proj_vec2vec(c(1, 1, 1), c(1, 0, 0), c(1, 0, 0)),
      test_proj_vec2vec(c(2, 2, 2), c(0, 1, 0), c(0, 2, 0)),
      test_proj_vec2vec(c(2, 2, 2), c(0, 2, 0), c(0, 2, 0)),
      test_proj_vec2vec(c(1, 0, 0), c(1, 1, 0), c(.5, .5, 0)),
      test_proj_vec2vec(c(9, 9), c(1, 0), c(9, 0))
    ),
    c(0, 0, 0, 0, 0)
  )
})

# -- proj_vec2plane ------------------------------------------------------------
test_that("proj_vec2plane gives correct outputs", {
  test_proj_vec2plane <- function(v1, v2, v3, v4) {
    norm(proj_vec2plane(v1, v2, v3) - v4, type = "2")
  }
  expect_equal(
    c(
      test_proj_vec2plane(c(1, 1, 1), c(1, 0, 0), c(0, 1, 0), c(1, 1, 0)),
      test_proj_vec2plane(c(2, 2, 2), c(1, 0, 0), c(0, 1, 0), c(2, 2, 0)),
      test_proj_vec2plane(c(3, 1, 1), c(0, 1, 0), c(0, 0, 1), c(0, 1, 1))
    ),
    c(0, 0, 0)
  )
})

# -- orthdist ------------------------------------------------------------------
test_that("orthdist gives correct outputs", {
  expect_equal(
    c(
      orthdist(c(1, 0), c(0, 0), c(0.5, 100)),
      orthdist(c(1, 0), c(0, 0), c(0.3, 100)),
      orthdist(c(1, 0, 0), c(0, 0, 0), c(0.5, 100, 100))
    ),
    c(0.5, 0.7, 0.5)
  )
})

# -- cart2sph ------------------------------------------------------------------
test_that("cart2sph gives the same output as pracma", {
  v <- runif(3)
  expect_equal(norm(cart2sph(v) - pracma::cart2sph(v), type = "2"), 0)
})

# -- create_resampling_index ---------------------------------------------------
test_that("create_resampling_index gives correct output", {
  expect_equal(sum(
    create_resampling_index(1, 10) - seq(from = 0, to = 1, by = .1)
  ), 0)
  expect_equal(sum(
    create_resampling_index(2, 10) - seq(from = 0, to = 2, by = .1)
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

# -- cummean_na ---------------------------------------------------------------
test_that("cummean_na works correctly", {
  expect_equal(cummean_na(c(1:5)), c(1, 1.5, 2, 2.5, 3))
  expect_equal(cummean_na(c(1, NA, 2)), c(1, 1, 1.5))
  expect_equal(cummean_na(c(NA, 1, 2)), c(NA, 1, 1.5))
})

# -- eulunity2quaternion -------------------------------------------------------
quat <- function(w, x, y, z) array(c(w, x, y, z), dim = c(1,4), dimnames = list(NULL, c("w", "x", "y", "z")))

test_that("eulunity2quaternion gives identity quaternion for zero rotation", {
  expect_equal(
    eulunity2quaternion(0, 0, 0),
    quat(1, 0, 0, 0)
  )
})


test_that("eulunity2quaternion gives correct quaternion for pi/2 rotation about x-axis", {
  expect_equal(
    eulunity2quaternion(90, 0, 0),
    quat(sqrt(2)/2, sqrt(2)/2, 0, 0)
  )
})


test_that("eulunity2quaternion gives correct quaternion for pi/2 rotation about y-axis", {
  expect_equal(
    eulunity2quaternion(0, 90, 0),
    quat(sqrt(2)/2, 0, sqrt(2)/2, 0)
  )
})


test_that("eulunity2quaternion gives correct quaternion for pi/2 rotation about z-axis", {
  expect_equal(
    eulunity2quaternion(0, 0, 90),
    quat(sqrt(2)/2, 0, 0, sqrt(2)/2)
  )
})


test_that("eulunity2quaternion returns normalized quaternions", {
  result <- eulunity2quaternion(
    c(0, 30, 90),
    c(0, 45, 180),
    c(0, 60, 270)
  )

  expect_equal(
    sqrt(rowSums(result^2)),
    rep(1, 3)
  )
})


test_that("eulunity2quaternion handles vectorized Euler angles", {
  result <- eulunity2quaternion(
    c(0, 90),
    c(0, 0),
    c(0, 0)
  )

  expect_equal(
    result,
    rbind(
      quat(1, 0, 0, 0),
      quat(sqrt(2)/2, sqrt(2)/2, 0, 0)
    )
  )
})


test_that("eulunity2quaternion gives the same rotation as eulunity2rot", {
  angles <- list(
    c(0, 0, 0),
    c(90, 0, 0),
    c(0, 90, 0),
    c(0, 0, 90),
    c(90, 133, 39)
  )

  for (a in angles) {
    q <- eulunity2quaternion(a[1], a[2], a[3])
    R_from_q <- quaternion2rot(q)
    R_from_euler <- eulunity2rot(a[1], a[2], a[3])

    expect_equal(R_from_q, R_from_euler, tolerance = 1e-10)
  }
})


# -- quaternion2eulunity -------------------------------------------------------

test_that("quaternion2eulunity converts identity quaternion to zero Euler angles", {
  expect_equal(
    unname(quaternion2eulunity(c(1, 0, 0, 0))),
    c(0, 0, 0)
  )
})


test_that("quaternion2eulunity is an inverse for eulunity2quaternion", {
  sample <- tibble(
    rot_x = c(0, 30, 90, -45),
    rot_y = c(0, 45, 133, -20),
    rot_z = c(0, 60, 39, 120)
  )

  test <- sample %>%
    rowwise() %>%
    mutate(
      q = list(eulunity2quaternion(rot_x, rot_y, rot_z)),
      un_q = list(quaternion2rot(q)),
      R = list(eulunity2rot(rot_x, rot_y, rot_z)),
    )
  
  expect_equal(test$R, test$un_q)
})


test_that("quaternion2eulunity gives the same Euler angles for q and -q", {
  q <- eulunity2quaternion(30, 45, 60)

  expect_equal(
    quaternion2eulunity(q),
    quaternion2eulunity(-q),
    tolerance = 1e-10
  )
})


test_that("quaternion2eulunity returns named Euler angles", {
  result <- quaternion2eulunity(c(1, 0, 0, 0))

  expect_named(
    result,
    c("rot_x", "rot_y", "rot_z")
  )
})


# -- quaternion2rot -----------------------------------------------------------

test_that("quaternion2rot gives identity matrix for identity quaternion", {
  expect_equal(
    quaternion2rot(c(1, 0, 0, 0)),
    diag(3)
  )
})


test_that("quaternion2rot gives correct rotation about x-axis", {
  q <- eulunity2quaternion(90, 0, 0)

  expect_equal(
    quaternion2rot(q),
    eulunity2rot(90, 0, 0),
    tolerance = 1e-10
  )
})


test_that("quaternion2rot gives correct rotation about y-axis", {
  q <- eulunity2quaternion(0, 90, 0)

  expect_equal(
    quaternion2rot(q),
    eulunity2rot(0, 90, 0),
    tolerance = 1e-10
  )
})


test_that("quaternion2rot gives correct rotation about z-axis", {
  q <- eulunity2quaternion(0, 0, 90)

  expect_equal(
    quaternion2rot(q),
    eulunity2rot(0, 0, 90),
    tolerance = 1e-10
  )
})


test_that("quaternion2rot gives the same rotation for q and -q", {
  q <- eulunity2quaternion(30, 45, 60)

  expect_equal(
    quaternion2rot(q),
    quaternion2rot(-q),
    tolerance = 1e-10
  )
})


test_that("quaternion2rot returns a valid rotation matrix", {
  q <- eulunity2quaternion(37, 82, 143)
  R <- quaternion2rot(q)

  ## R must be orthogonal
  expect_equal(
    t(R) %*% R,
    diag(3),
    tolerance = 1e-10
  )

  ## A proper rotation has determinant one
  expect_equal(
    det(R),
    1,
    tolerance = 1e-10
  )
})


# -- rot2quaternion ------------------------------------------------------------

test_that("rot2quaternion converts identity matrix to identity quaternion", {
  expect_equal(
    rot2quaternion(diag(3)),
    quat(1, 0, 0, 0)
  )
})


test_that("rot2quaternion gives correct quaternion for rotation about x-axis", {
  R <- eulunity2rot(90, 0, 0)

  expect_equal(
    abs(rot2quaternion(R)),
    abs(eulunity2quaternion(90, 0, 0)),
    tolerance = 1e-10
  )
})


test_that("rot2quaternion gives correct quaternion for rotation about y-axis", {
  R <- eulunity2rot(0, 90, 0)

  expect_equal(
    abs(rot2quaternion(R)),
    abs(eulunity2quaternion(0, 90, 0)),
    tolerance = 1e-10
  )
})


test_that("rot2quaternion gives correct quaternion for rotation about z-axis", {
  R <- eulunity2rot(0, 0, 90)

  expect_equal(
    abs(rot2quaternion(R)),
    abs(eulunity2quaternion(0, 0, 90)),
    tolerance = 1e-10
  )
})


test_that("rot2quaternion is an inverse for quaternion2rot", {
  quaternions <- list(
    quat(1, 0, 0, 0),
    eulunity2quaternion(30, 45, 60),
    eulunity2quaternion(90, 0, 0),
    eulunity2quaternion(0, 90, 0),
    eulunity2quaternion(0, 0, 90),
    eulunity2quaternion(90, 133, 39)
  )

  for (q in quaternions) {
    R <- quaternion2rot(q)
    new_q <- rot2quaternion(R)

    ## q and -q represent the same rotation
    expect_equal(
      abs(new_q),
      abs(q),
      tolerance = 1e-10
    )
  }
})


test_that("rot2quaternion returns a normalized quaternion", {
  R <- eulunity2rot(37, 82, 143)
  q <- rot2quaternion(R)

  expect_equal(
    sqrt(sum(q^2)),
    1,
    tolerance = 1e-10
  )
})


# -- median_quaternion ---------------------------------------------------------

test_that("median_quaternion returns the identity quaternion for identical inputs", {
  q <- quat(1, 0, 0, 0)

  result <- median_quaternion(
    list(q, q, q, q, q)
  )

  expect_equal(
    abs(result),
    abs(q),
    tolerance = 1e-10
  )
})


test_that("median_quaternion handles the quaternion double-cover property", {
  q <- eulunity2quaternion(30, 45, 60)

  result <- median_quaternion(
    list(q, -q, q, -q, q)
  )

  ## q and -q represent the same rotation
  expect_equal(
    abs(result),
    abs(q),
    tolerance = 1e-10
  )
})


test_that("median_quaternion returns a normalized quaternion", {
  quaternions <- list(
    eulunity2quaternion(10, 20, 30),
    eulunity2quaternion(12, 21, 31),
    eulunity2quaternion(11, 19, 29),
    eulunity2quaternion(13, 20, 30),
    eulunity2quaternion(9, 21, 31)
  )

  result <- median_quaternion(quaternions)

  expect_equal(
    sqrt(sum(result^2)),
    1,
    tolerance = 1e-10
  )
})


test_that("median_quaternion gives the same rotation when quaternion signs are flipped", {
  quaternions <- list(
    eulunity2quaternion(10, 20, 30),
    eulunity2quaternion(12, 21, 31),
    eulunity2quaternion(11, 19, 29),
    eulunity2quaternion(13, 20, 30),
    eulunity2quaternion(9, 21, 31)
  )

  flipped_quaternions <- lapply(
    quaternions,
    function(q) if (q[1] < 0) -q else q
  )

  ## Explicitly flip alternating signs to test the double cover
  flipped_quaternions[c(2, 4)] <- lapply(
    flipped_quaternions[c(2, 4)],
    function(q) -q
  )

  result <- median_quaternion(quaternions)
  flipped_result <- median_quaternion(flipped_quaternions)

  expect_equal(
    abs(result),
    abs(flipped_result),
    tolerance = 1e-10
  )
})


test_that("median_quaternion gives the expected result for rotations about one axis", {
  quaternions <- list(
    eulunity2quaternion(0, 0, 0),
    eulunity2quaternion(10, 0, 0),
    eulunity2quaternion(20, 0, 0)
  )

  result <- median_quaternion(quaternions)
  expected <- eulunity2quaternion(10, 0, 0)

  expect_equal(
    abs(result),
    abs(expected),
    tolerance = 1e-10
  )
})


test_that("median_quaternion gives the expected result when the middle rotation is repeated", {
  q1 <- eulunity2quaternion(0, 0, 0)
  q2 <- eulunity2quaternion(30, 0, 0)
  q3 <- eulunity2quaternion(60, 0, 0)

  result <- median_quaternion(
    list(q1, q2, q2, q3, q2)
  )

  expect_equal(
    abs(result),
    abs(q2),
    tolerance = 1e-10
  )
})


test_that("median_quaternion returns a rotation quaternion for composed rotations", {
  quaternions <- list(
    eulunity2quaternion(10, 20, 30),
    eulunity2quaternion(12, 21, 31),
    eulunity2quaternion(11, 19, 29),
    eulunity2quaternion(13, 20, 30),
    eulunity2quaternion(9, 21, 31)
  )

  result <- median_quaternion(quaternions)
  R <- quaternion2rot(result)

  ## The median must represent a proper rotation matrix
  expect_equal(
    t(R) %*% R,
    diag(3),
    tolerance = 1e-10
  )

  expect_equal(
    det(R),
    1,
    tolerance = 1e-10
  )
})