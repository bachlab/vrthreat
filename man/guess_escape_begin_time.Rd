% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_movement_functions.R
\name{guess_escape_begin_time}
\alias{guess_escape_begin_time}
\title{Guess start of escape}
\usage{
guess_escape_begin_time(
  df,
  ref_pos,
  esc_dist = 0.75,
  span = 5,
  min_speed = 0.1,
  min_time = min(df$time),
  max_time = max(df$time),
  indx = 1
)
}
\arguments{
\item{df}{movement data frame}

\item{ref_pos}{reference position, a list containing `pos_x` and `pos_z`}

\item{esc_dist}{minimum distance from reference position to determine escape}

\item{span}{span of the median filter}

\item{min_speed}{minimum speed to determine start of escape}

\item{min_time}{minimum time to start searching for escape}

\item{max_time}{maximum time to search for escape}

\item{indx}{currently disabled (only needed for debugging)}
}
\description{
This function determines the time point when the player was first away from the
reference position, and then determines the last time that its velocity away
from the reference position was smaller than a threshold. Positions are initially
median-filtered, and so is the computed velocity. All calculations are performed
without resampling.
}
\details{
Developer note: if escape is imposed on an ongoing movement away from bush, it
might not be detected by this algorithm based on a fixed velocity criterion.
However, it can actually be ambiguous to decide whether the escape was planned
before or after the min_time. The algorithm could potentially be improved by
using an acceleration criterion (e.g. zero crossing) but the tracker data are
a bit noisy and this would require some more elaborate signal processing/filtering.
}
