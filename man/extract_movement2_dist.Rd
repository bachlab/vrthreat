% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_movement_functions.R
\name{extract_movement2_dist}
\alias{extract_movement2_dist}
\title{Extract (min, max or) 2D distance between two movement trajectories.}
\usage{
extract_movement2_dist(
  df1,
  df2,
  min_time = min(c(df1$time, df2$time)),
  max_time = max(c(df1$time, df2$time)),
  method = "min",
  samplingrate = 10
)
}
\arguments{
\item{df1}{A dataframe of movement (must contain standard trajectory
columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).}

\item{df2}{A dataframe of movement (must contain standard trajectory
columns, i.e. `"time"`, `"pos_x"`, `"pos_y"`, `"pos_z"`).}

\item{min_time}{Minimum time within the ref_movement (taken from `"time"`
column) to search.}

\item{max_time}{Maximum time within the ref_movement (taken from `"time"`
column) to search.}

\item{method}{whether to extract min (default), max or mean}

\item{samplingrate}{resampling rate}
}
\value{
A numeric value of the minimum 2D distance of the two movement trajectories.
}
\description{
Searches two dataframes of movement trajectories and summarises distance
(ignoring y) between the two trajectories as average or minimum.
}
\details{
NOTE: because sampling times can differ between data frames, data are
resampled at default rate of 10 Hz, and to avoid an impact of tracker
glitches they are median-smoothed over 3 data points (300 ms).
}
