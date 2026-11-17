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


#' Calculate 2D angle between positive z-axis and a line from reference to target position in degrees
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
#' NA encountered in x can be handled three ways. 
#' "keep" where final computation will be NA if NAs encountered.
#' "discard" time intervals with NA observation dropped.
#' "zero" NA replaced with 0 and included in the time weighted sum
#' 
#' @param x Numeric values
#' @param t Time values
#' @param na_handling String options "keep" "discard" "zero" (default "keep") 
#'
#' @return Estimate of `x` across time values
#' @export
#'
#' @examples
timeseries_mean <- function(x, t, na_handling = "keep") {
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

  trapez_diffs <- 0.5 * (x[1:(m-1)] + x[2:m]) * (t[2:m] - t[1:(m-1)])

  #handle NA
  if(na_handling == "zero"){
    x[is.na(x)] <- 0
    na.rm <- TRUE
    NA_time <- 0
  } else if (na_handling == "discard"){
    na.rm <- TRUE
    NA_time <- sum((t[2:m] - t[1:(m-1)])[is.na(trapez_diffs)]) #subtract NA_time from total time
  } else { #na_handling == "keep" default case
    na.rm <- FALSE
    NA_time <- 0 # Computing NA_time would be useless
  }

  return(sum(trapez_diffs, na.rm = na.rm) / (t[m] - t[1] - NA_time))
}

#' Convert rotation in Unity Euler angles (in degrees) into a quaternion
#'
#' Converts Unity's specific (improper) Euler angles into a 4-dimensional 
#' quaternion vector (w, x, y, z) for further computations.
#'
#' 1. Unity uses (improper) Euler angles to output their internal quaternion representation.
#'
#' 2. These angles describe extrinsic rotations around the Z - X - Y coordinate axis.
#'
#' 3. The angles are given as clockwise when the axis is pointing towards the observer
#'    as the coordinate system is left-handed.
#' 
#' If rot_x rot_y and rot_z are columns then this function returns an nx4 matrix where n is the length of the column
#'
#' @param rot_x Euler x angle in degrees
#' @param rot_y Euler y angle in degrees
#' @param rot_z Euler z angle in degrees
#'
#' @return A numeric vector of length 4 representing the unit quaternion `c(w, x, y, z)`.
#' @export
eulunity2quaternion <- function(rot_x, rot_y, rot_z){
  # Convert degrees to radians
  rad_x <- rot_x * pi / 180
  rad_y <- rot_y * pi / 180
  rad_z <- rot_z * pi / 180
  
  # Half angles (Because of how Quaternions rotate)
  # i.e you rotate the quaternion v around the axis specified by quaternion q as -  qvq^-1
  # See https://stuff.mit.edu/afs/athena/course/2/2.998/www/QuaternionReport1.pdf for more details
  cx <- cos(rad_x / 2); sx <- sin(rad_x / 2)
  cy <- cos(rad_y / 2); sy <- sin(rad_y / 2)
  cz <- cos(rad_z / 2); sz <- sin(rad_z / 2)
  
  # Unity Z-X-Y composition order (qy * qx * qz)
  # Left-handed with CW rotation maps identically to standard right-handed formulas
  # Obtained from the following computation. (Here i, j and k are the unit quaternions)
  # quaternion for rotation about x axis is: qx = cx + isx
  # quaternion for rotation about y axis is: qy = cy + jsy
  # quaternion for rotation about z axis is: qz = cz + ksz
  # We need to rotate first about Z then X then Y in that order (This is how Unity applies euler angles)
  # So we compute the quaternion qy*qx*qz = (cy + jsy)(cx + isx)(cz + ksz)
  w <- cx*cy*cz + sx*sy*sz
  x <- sx*cy*cz + cx*sy*sz
  y <- cx*sy*cz - sx*cy*sz
  z <- cx*cy*sz - sx*sy*cz
  
  return(cbind(w = w, x = x, y = y, z = z))
}

#' Convert a 3D rotation quaternion into a Rotation Matrix
#'
#' Converts a 4-dimensional unit quaternion vector `c(w, x, y, z)` into
#' a rotation matrix
#'
#' @param quat A numeric vector of length 4 representing the quaternion `c(w, x, y, z)`.
#'
#' @return A rotation matrix `R`
#' @export
quaternion2rot <- function(q) {
  # Normalize the quaternion
  q <- q / sqrt(sum(q^2))
  
  # Extract components
  w <- q[1]
  x <- q[2]
  y <- q[3]
  z <- q[4]
  
  # Create rotation matrix
  R <- matrix(c(
    1 - 2*(y^2 + z^2),  2*(x*y - w*z),      2*(x*z + w*y),
    2*(x*y + w*z),      1 - 2*(x^2 + z^2),  2*(y*z - w*x),
    2*(x*z - w*y),      2*(y*z + w*x),      1 - 2*(y^2 + x^2)
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  return(R)
}

#' Convert a 3D rotation quaternion into Unity Euler angles (in degrees)
#'
#' Converts a 4-dimensional unit quaternion vector `c(w, x, y, z)` back into 
#' Unity's specific (improper) extrinsic Z-X-Y Euler representation.
#'
#' @param q A numeric vector of length 4 representing the quaternion `c(w, x, y, z)`.
#'
#' @return A named numeric vector of length 3 containing `rot_x`, `rot_y`, and `rot_z` in degrees.
#' @export
quaternion2eulunity <- function(q){
  # The convention is that cx = cos(rot_x), cy = cos(rot_y), sx = sin(rot_x) etc.
  # Derived specifically from Unity's qy * qx * qz matrix composition sequence
  #
  #      |  cy*cz + sx*sy*sz     -cy*sz + sx*sy*cz      cx*sy  |
  #  R = |  cx*sz                 cx*cz                -sx     |
  #      | -sy*cz + cy*sx*sz      sy*sz + sx*cy*cz      cx*cy  |
  #
  # See also https://en.wikipedia.org/wiki/Euler_angles#Conversion_to_other_orientation_representations
  # 
  # We compare these against quaternion (w, x, y, z) being equivalent to the rotation matrix
  # 
  #     | 1 - 2*(y^2 + z^2),  2*(x*y - w*z),      2*(x*z + w*y),     |
  # R = | 2*(x*y + w*z),      1 - 2*(x^2 + z^2),  2*(y*z - w*x),     |
  #     | 2*(x*z - w*y),      2*(y*z + w*x),      1 - 2*(y^2 + x^2)  |
  # 
  # And apply the same logic as rot2eulunity
  # Computing first sin(x) and so on.
  # But to avoid code replication we invoke

  rot2eulunity(quaternion2rot(q))
}

#' Convert a 3x3 rotation matrix to a rotation quaternion
#'
#' This is equivalent to eulunity2quaternion(rot2eulunity(R))
#' 
#' @param R a 3x3 Rotation matrix
#' 
#' @return A numeric vector of length 4 representing the quaternion `c(w, x, y, z)`.
#'
#' @export
rot2quaternion <- function(R){
  eul_angles <- rot2eulunity(R)
  eulunity2quaternion(eul_angles[[1]], eul_angles[[2]], eul_angles[[3]])
}


#' Compute the geometric median rotation from a list of quaternions
#'
#' Takes a matrix of 4-element quaternion vectors, unifies their hemispherical 
#' signs to handle the double-cover property, and finds the geometric median 
#' rotation using Weiszfeld's algorithm via the `pracma` library.
#'
#' @param q_matrix A list of numeric vectors, where each element is a `c(w, x, y, z)` quaternion.
#'
#' @return A normalized 4-element numeric vector representing the median quaternion.
#' @importFrom pracma geo_median
#' @export
median_quaternion <- function(q_matrix) {
  # Bind the list into a matrix where rows are points in 4D space
  if(is.list(q_matrix))
    q_matrix <- do.call(rbind, q_matrix)

  # Align signs to the same hemisphere relative to the first quaternion entry
  ref_q <- q_matrix[1, ]
  for (i in 1:nrow(q_matrix)) {
    if (sum(q_matrix[i, ] * ref_q) < 0) {
      q_matrix[i, ] <- -q_matrix[i, ]
    }
  }
  
  # Guard against all equal q_matrix inputs
  if(all(
    apply(q_matrix,
          MARGIN = 1,
          function(row)
           isTRUE(all.equal(row, q_matrix[1,], check.attributes = FALSE)))
  )){
    return(array(ref_q, dim = c(1,4), dimnames = list(NULL, c("w", "x", "y", "z"))))
  }

  # Compute the geometric spatial median
  spatial_median <- pracma::geo_median(q_matrix)$p
  
  # Renormalize the result to project it back onto the unit hypersphere (S3)
  median_q <- spatial_median / sqrt(sum(spatial_median^2))
  
  # Math format of quaternion in eulunity2quaternion
  median_q <- array(median_q, dim = c(1,4), dimnames = list(NULL, c("w", "x", "y", "z")))

  return(median_q)
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

#' Convert a 3x3 rotation matrix to Unity-style Euler angles (in degrees)
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
#' @param R a 3x3 Rotation matrix
#'
#' @return a vector containing rot_x rot_y and rot_z
#' @export
#'
#' @examples
rot2eulunity <- function(R) {
  if (!is.matrix(R) || nrow(R) != 3L || ncol(R) != 3L) {
    stop("R must be a 3x3 matrix")
  }

  # By hand multiplication we have that
  # R = Ry %*% Rx %*% Rz
  #
  # The convention is that cx = cos(rot_x), cy = cos(rot_y), sx = sin(rot_x) etc.
  #
  #      |  cy*cz + sx*sy*sz     -cy*sz + sx*sy*cz      cx*sy  |
  #  R = |  cx*sz                 cx*cz                -sx     |
  #      | -sy*cz + cy*sx*sz      sy*sz + sx*cy*cz      cx*cy  |
  #
  # See also https://en.wikipedia.org/wiki/Euler_angles#Conversion_to_other_orientation_representations
  # We use these to compute rot_x rot_y and rot_z

  # Numerical safety
  sx <- max(-1, min(1, -R[2, 3]))   # = sin(x)
  x <- asin(sx)
  cx <- cos(x)

  eps <- 1e-6
  if (abs(cx) > eps) {
    # Standard case
    y <- atan2(R[1, 3], R[3, 3])
    z <- atan2(R[2, 1], R[2, 2])
  } else {
    # Gimbal lock (x = ±90 deg or cx = 0 roughly)
    z <- 0
    if (sx > 0) {
      # x = +90
      y <- atan2(-R[3, 1], R[1, 1])
    } else {
      # x = -90
      y <- atan2(R[3, 1], R[1, 1])
    }
  }

  # Convert to degrees
  angles <- c(
    rot_x = x * 180 / pi,
    rot_y = y * 180 / pi,
    rot_z = z * 180 / pi
  )

  # Wrap to [0, 360)
  angles <- angles %% 360
  # Convert to (-180, 180]
  angles[angles > 180] <- angles[angles > 180] - 360

  angles
}


#' Apply a rotation matrix R to rot_x rot_y and rot_z columns
#' 
#' This function is mathematically equivalent to `rot2eulunity(R %*% eulunity2rot(rot_x, rot_y, rot_z))`
#' But is instead vectorized and can operate on columns instead of needing to be computed row wise. 
#'
#' @param rot_x
#' @param rot_y
#' @param rot_z
#' @param R a 3x3 Rotation matrix
#'
#' @return a vector containing rot_x rot_y and rot_z
#' @export
#'
#' @examples
reorient_eulunity <- function(rot_x, rot_y, rot_z, R) {
  if(!validate_numeric(rot_x, .fun_name = "reorient_eulunity") ||
     !validate_numeric(rot_y, .fun_name = "reorient_eulunity") ||
     !validate_numeric(rot_z, .fun_name = "reorient_eulunity") ||
     !validate_numeric(R, .fun_name = "reorient_eulunity") ||
     !validate_vector_lengths(rot_x, rot_y, rot_z, .fun_name = "reorient_eulunity") ||
     !is.matrix(R)){
    return(cbind(rot_x, rot_y, rot_z))
  }

  # Angles -> radians
  x <- rot_x * pi / 180
  y <- rot_y * pi / 180
  z <- rot_z * pi / 180

  sx <- sin(x)
  cx <- cos(x)
  sy <- sin(y)
  cy <- cos(y)
  sz <- sin(z)
  cz <- cos(z)

  # ------------------------------------------------------------
  # A = Ry %*% Rx %*% Rz
  #
  # A =
  # | cy*cz + sx*sy*sz    -cy*sz + sx*sy*cz    cx*sy |
  # | cx*sz               cx*cz                -sx   |
  # | -sy*cz + cy*sx*sz   sy*sz + sx*cy*cz     cx*cy |
  # ------------------------------------------------------------

  a11 <- cy*cz + sx*sy*sz
  a12 <- -cy*sz + sx*sy*cz
  a13 <- cx*sy

  a21 <- cx*sz
  a22 <- cx*cz
  a23 <- -sx

  a31 <- -sy*cz + cy*sx*sz
  a32 <- sy*sz + sx*cy*cz
  a33 <- cx*cy

  # ------------------------------------------------------------
  # B = R %*% A
  #
  # We only need:
  # B[1,1], B[1,3]
  # B[2,1], B[2,2], B[2,3]
  # B[3,1], B[3,3]
  #
  # for rot2eulunity().
  # ------------------------------------------------------------

  b11 <- R[1,1] * a11 + R[1,2] * a21 + R[1,3] * a31
  b13 <- R[1,1] * a13 + R[1,2] * a23 + R[1,3] * a33

  b21 <- R[2,1] * a11 + R[2,2] * a21 + R[2,3] * a31
  b22 <- R[2,1] * a12 + R[2,2] * a22 + R[2,3] * a32
  b23 <- R[2,1] * a13 + R[2,2] * a23 + R[2,3] * a33

  b31 <- R[3,1] * a11 + R[3,2] * a21 + R[3,3] * a31
  b33 <- R[3,1] * a13 + R[3,2] * a23 + R[3,3] * a33

  # ------------------------------------------------------------
  # B -> Euler angles
  # Same formulas as rot2eulunity()
  # ------------------------------------------------------------

  # Numerical safety
  sx2 <- pmax(-1, pmin(1, -b23))

  x2 <- asin(sx2)
  cx2 <- cos(x2)

  eps <- 1e-6

  y2 <- numeric(length(x2))
  z2 <- numeric(length(x2))

  normal <- abs(cx2) > eps

  # Standard case
  y2[normal] <- atan2(
    b13[normal],
    b33[normal]
  )

  z2[normal] <- atan2(
    b21[normal],
    b22[normal]
  )

  # Gimbal lock
  gimbal <- !normal

  z2[gimbal] <- 0

  positive <- gimbal & sx2 > 0
  negative <- gimbal & sx2 <= 0

  # x = +90 deg
  y2[positive] <- atan2(
    -b31[positive],
    b11[positive]
  )

  # x = -90 deg
  y2[negative] <- atan2(
    b31[negative],
    b11[negative]
  )

  # Radians -> degrees
  angles <- cbind(
    rot_x = x2 * 180 / pi,
    rot_y = y2 * 180 / pi,
    rot_z = z2 * 180 / pi
  )

  # Convert to (-180, 180]
  angles <- angles %% 360
  angles[angles > 180] <- angles[angles > 180] - 360

  angles
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
  sintheta <- sqrt(1- min(1, costheta^2))
  # cross product (normal to the plane spanned by x, z)
  u <- pracma::cross(x, y)
  if(any(is.na(u)) || (norm(u, type="2")== 0)){
    return(diag(3))
  }
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
    as.vector(x %*% y) / sum(y^2) * y
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


#' Perpendicular distance of p3 from p1 along p1p2
#'
#' This can be used to determine when an object crosses a line
#' perpendicular to p1p2, for example along a line from threat to safety
#'
#' @param p1 A vector of arbitrary dimension
#' @param p2 Another vector with he same dimensions as p1
#' @param p3 Yet another vector with the same dimensions
#'
#' @return A scalar
#' @export
#'
#' @examples
orthdist <- function(p1, p2, p3) {
  # translate to origin
  u <- p2 - p1
  v <- p3 - p1
  # project v onto u
  w <- proj_vec2vec(v, u)
  norm(w, type = "2")
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
  if(max_t <= 0)
    return(numeric)
  (0:floor(sample_rate * max_t)) / sample_rate
  }


#' Find minimum, avoiding warnings
#'
#'This is a wrapper for Rbase min() which avoids warnings when the supplied vector
#'is empty or contains only NaNs
#' @param vec A numerical vector
#'
#' @return The minimum (a number or NA)
#'
find_min <- function(vec) {
    if (length(vec) == 0 || all(is.na(vec))) {
      NA_real_
    } else {
      min(vec, na.rm = TRUE)
    }
  }

#' Find index of minimum, avoiding warnings and errors
#'
#'This is a wrapper for Rbase which.min() which avoids warnings and errors
#' when the supplied vector is empty or contains only NaNs
#' @param vec A numerical vector
#'
#' @return The index of the minimum (a number or NA)
find_argmin <- function(vec) {
    if (length(vec) == 0 || all(is.na(vec))) {
      NA_real_
    } else {
      which.min(vec)
    }
}

#' Find maximum, avoiding warnings
#'
#'This is a wrapper for Rbase max() which avoids warnings when the supplied vector
#'is empty or contains only NaNs
#' @param vec A numerical vector
#'
#' @return The maximum (a number or NA)
#'
find_max <- function(vec) {
  if (length(vec) == 0 || all(is.na(vec))) {
    NA_real_
  } else {
    max(vec, na.rm = TRUE)
  }
}

#' Find index of maximum, avoiding warnings and errors
#'
#'This is a wrapper for Rbase which.max() which avoids warnings and errors
#' when the supplied vector is empty or contains only NaNs
#' @param vec A numerical vector
#'
#' @return The index of the minimum (a number or NA)
find_argmax <- function(vec) {
  if (length(vec) == 0 || all(is.na(vec))) {
    NA_real_
  } else {
    which.max(vec)
  }
}



#' Calculate cummean with previous neighbour interpolation
#'
#' @param x A vector of numeric
#'
#' @return A vector of numeric
#' @export
#'
#' @examples
cummean_na <- function(x) {
  valid <- !is.na(x)
  x_cum <- rep(NA, times = length(x))
  x_cum[valid] <- cumsum(x[valid]) / seq_along(x[valid])

  # replace NA values by previous neighbour interpolation
  invalid <- !valid

  if (sum(invalid) > 0) {
    invalid <- which(invalid)
    for (k in 1:length(invalid)) {
      if (invalid[k] > 1) {
        x_cum[invalid[k]] <- x_cum[invalid[k] - 1]
      }
    }
  }
  return(x_cum)
}
