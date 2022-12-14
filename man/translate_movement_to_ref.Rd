% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_movement_functions.R
\name{translate_movement_to_ref}
\alias{translate_movement_to_ref}
\title{Translate positions in movement data frame to reference position.}
\usage{
translate_movement_to_ref(df, ref_pos = list(pos_x = 0, pos_y = 0, pos_z = 0))
}
\arguments{
\item{df}{A movement data frame.}

\item{ref_pos}{A named list of x,y,z reference position (default: 0, 0, 0)}
}
\value{
A translated movement data frame with the same column names
}
\description{
This is a convenience function to make escape responses comparable between
scenarios with different orientation (e.g. different fruit picking place). It
does not (yet) allow reorienting the rotations.
(NOTE: if required, such rotation re-orientation would require a function to
convert a rotation matrix to Euler angles)
}
