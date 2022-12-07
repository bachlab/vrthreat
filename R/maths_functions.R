#' Calculate angle difference in degrees, respecting angles <0 or >360.
#'
#' @param target_ang Angle
#' @param ref_ang Reference angle
#'
#' @return Difference in angles in degrees
#' @export
#'
#' @examples
angle_diff_deg <- function(target_ang, ref_ang) {
  ((target_ang - ref_ang) + 180) %% 360 - 180
}


#' Calculate 2D angle positive z-axis and a line from reference to target position in degrees
#'
#' @param target_x Vector of target x positions
#' @param target_z Vector of target y positions
#' @param ref_x Vector of reference x positions (default: origin)
#' @param ref_z Vector of reference y positions (default: origin)
#'
#'
#' @return Angle between a line from reference to target vector and positive
#' z-axis, measured clockwise and in [-180, 180]
#' @export
#'
#' @examples
calculate_2d_angle_deg <- function(target_x, target_z, ref_x = 0, ref_z = 0) {
  x <- target_x - ref_x
  z <- target_z - ref_z

  atan2(x, z) * 180 / pi
}



#' Calculate 2D Euclidean distance between two positions
#'
#' @param target_x Vector of target x positions
#' @param target_z Vector of target y positions
#' @param ref_x Vector of reference x positions (default: origin)
#' @param ref_z Vector of reference y positions (default: origin)
#'
#' @return
#' @export
#'
#' @examples
calculate_2d_dist <-
  function(target_x,
           target_z,
           ref_x = 0,
           ref_z = 0) {

    sqrt((target_x - ref_x) ^ 2.0 +
         (target_z - ref_z) ^ 2.0)

  }


#' Compute the mean across a vector `x` respecting time `t`, using trapezium
#' rule.
#'
#' @param x Numeric values
#' @param t Time values
#'
#' @return Estimate of `x` across time values
#' @export
#'
#' @examples
timeseries_mean <- function(x, t) {
  m <- length(x)
  n <- length(t)
  if (m != n)
    stop(paste0(
      "Arguments 'x', 't' must be vectors of the same length. (",
      m,
      ", ",
      n,
      ")."
    ))

  if (m <= 1) return(x)

  trapez <- sum(0.5 * (x[1:(m-1)] + x[2:m]) * (t[2:m] - t[1:(m-1)]))
  return(trapez / (t[m] - t[1]))
}


#' Convert rotation in Unity Euler angles (in degrees) into 3D rotation matrix
#'
#' Converts Unity's specific (improper) Euler angles into a 3D rotation matrix for
#' further computations.
#'
#' 1. Unity uses (improper) Euler angles to output their internal quaternion representation.
#'    (https://docs.unity3d.com/2018.4/Documentation/Manual/QuaternionAndEulerRotationsInUnity.html)
#'
#' 2. These angles describe extrinsic rotations around the Z - X - Y coordinate axis
#'    (https://docs.unity3d.com/ScriptReference/Transform-eulerAngles.html)
#'
#'    (also verified from Unity editor and by comparing Unity output to videos)
#'
#' 3. The angles are given as clockwise when the axis is pointing towards the observer
#'    as the coordinate system is left-handed
#'   (verified from Unity editor)
#'
#' 4. The function uses basic rotation matrices and composition of rotations
#'    (e.g. https://en.wikipedia.org/wiki/Rotation_matrix#Basic_rotations, the matrices
#'    given there are for counterclockwise rotation for right-handed system;
#'    the same rotations produce clockwise rotation in left-handed system
#'
#' @param rot_x Euler x angle in degrees
#' @param rot_y Euler y angle in degrees
#' @param rot_z Euler z angle in degrees
#'
#' @return A 3D rotation matrix in left-handed coordinates
#' @export
#'
#' @examples
eulunity2rot <- function(rot_x, rot_y, rot_z) {

  thetaX <- rot_x * pi / 180
  thetaY <- rot_y * pi / 180
  thetaZ <- rot_z * pi / 180

  Rx <- c(1,           0,            0,
          0, cos(thetaX),  -sin(thetaX),
          0, sin(thetaX),  cos(thetaX))
  Rx <- matrix(Rx, ncol = 3, nrow = 3, byrow = T)

  Ry <- c( cos(thetaY),  0, sin(thetaY),
           0,            1,           0,
           -sin(thetaY), 0, cos(thetaY))
  Ry <- matrix(Ry, ncol = 3, nrow = 3, byrow = T)

  Rz <- c(cos(thetaZ), -sin(thetaZ), 0,
          sin(thetaZ),  cos(thetaZ), 0,
          0,            0,           1)
  Rz <- matrix(Rz, ncol = 3, nrow = 3, byrow = T)


  Ry %*% Rx %*% Rz
}

#' Find a rotation matrix that maps one vector onto the other
#'
#' Uses base R functions and pracma, to find the matrix that maps x to y by a rotation through
#' the normal to the plane spanned by x and y. This is the "shortest" mapping
#' between the two vectors, which does not involve any rotation about the rotated
#' vector itself.
#'
#' This formula is independent of the coordinate system used, as the properties
#' of the output are defined by y = Rx and hold in any coordinate system.
#' This function is verified to be numerically precise for norm vectors
#'
#' @param x A vector in R3
#' @param y Another vector in R3
#'
#' @return a 3x3 rotation matrix R with y = Rx
#' @export
#'
#' @examples
vec2rot <- function(x, y) {
  # (see https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle and
  # https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula; note the different
  # notation in the last term - u x u vs. K2 -  which leads to a different first
  # term, i.e. I vs. I cos(theta))
  # vector angle. Because we rotate through the normal to the plane spanned by
  # x and y using the right-hand rule, the rotation is always counterclockwise
  # by a positive angle when the normal is pointing towards the observer
  costheta <- as.double(x %*% y / (norm(x, type = "2") * norm(y, type = "2")))
  sintheta <- sqrt(1-(costheta^2))
  # cross product (normal to the plane spanned by x, z)
  u <- pracma::cross(x, y)
  u <- u/norm(u, type = "2")
  # cross product matrix of u
  # https://en.wikipedia.org/wiki/Cross_product#Conversion_to_matrix_multiplication
  ux <- matrix(c(0, u[3], -u[2], -u[3], 0, u[1], u[2], -u[1],0), ncol = 3, nrow = 3)
  # outer product of u
  uo <- u %o% u

  # Rodrigues formula, which describes right-hand rule rotation (counterclockwise
  # when rotation axis is pointing towards observer)
  costheta * diag(3) + sintheta * ux + (1-costheta) * uo

}

# 'Calculate angle between two Rd vectors, taking vectors as input
#'
#' This uses R base functions to calculate the angle between two vectors in degrees,
#' accounting for rounding errors to avoid NaNs.
#'
#' @param x Vector of length d
#' @param y Vector of length d
#'
#' @return Angle in degrees
#' @export
#'
#' @examples
angle_between <- function(x,y) {
  cos_ang <-
    as.double(x %*% y / (norm(x, type = "2") * norm(y, type = "2")))

  # rounding errors above can produce NaNs in acos below
  if (!is.null(cos_ang) && !is.na(cos_ang) && abs(cos_ang) > 1) cos_ang <- sign(cos_ang)

  acos(cos_ang) * 180 / pi
}


#' Compute central angle between two rotation matrices, for a direction vector
#'
#' For a given direction vector, computes the central angle between the rotated
#' versions of the vector.
#'
#' This is different from the distance between the rotations, because rotations
#' around the direction vector are ignored here. This is useful for computing
#' visual scan speed where rotation around the eye axis is irrelevant.
#'
#' @param R1 First rotation matrix
#' @param R2 Second rotation matrix
#' @param e  Base vector (e.g. c(0,0,1) for forward gaze), magnitude is ignored
#'
#' @return Angle in degrees
#' @export
#'
#' @examples
rot2centralangle <- function(R1, R2, e) {
  if (is.null(R1) || is.null(R2) || any(is.na(R1)) || any(is.na(R2))) return(NA_real_)

  angle_between(as.vector(R1 %*% matrix(e)),
                as.vector(R2 %*% matrix(e)))
}


#' Project vector x onto vector y
#'
#'
#' @param x A vector of any length
#' @param y A vector of the same length as x
#'
#' @return Another vector of the same length
#'
proj_vec2vec <-
  function(x, y) {
    as.vector(x %*% y) / norm(y, type = "2") * y
  }

#' Project 3D-vector x into the plane spanned by y and z
#'
#' @param x A vector of length 3
#' @param y A vector of length 3
#' @param z Yet a vector of length 3
#'
#' @return Another vector of length 3
#'
proj_vec2plane <-
  function(x, y, z) {
    # find the normal of the plane
    n <- pracma::cross(y, z)
    # normalise normal
    n <- n / norm(n, type = "2")
    # project x onto the normal
    n_x <- proj_vec2vec(x, n)
    # subtract the result from x
    x - n_x
  }

#' Transform cartesian to spherical coordinates
#'
#' Wrapper around the pracma function with same name, handles NULL and NA arguments
#' @param x Vector of x/y/z coordinates
#'
#' @return Spherical coordinates, using pracma conventions
#' @export
#'
#' @examples
cart2sph <- function(x) {
  if (is.null(x) || any(is.na(x))) return(c(NA_real_, NA_real_, NA_real_))

  pracma::cart2sph(x)

}


#' Create resampling index at desired sample rate
#'
#' @param max_t Maximal time in seconds (numeric)
#' @param sample_rate   Sample rate in Hz (numeric)
#'
#' @return A resampling index vector.
#' @export
#'
#' @examples
#
create_resampling_index <-
  function(max_t, sample_rate) {
    (1:ceiling(sample_rate * max_t)) / sample_rate
  }
