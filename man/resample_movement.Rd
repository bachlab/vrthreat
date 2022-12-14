% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_movement_functions.R
\name{resample_movement}
\alias{resample_movement}
\title{Resample a movement data frame to new time index.}
\usage{
resample_movement(df, new_time, from = 0, to = Inf)
}
\arguments{
\item{df}{A movement data frame. Expected column: 'time'}

\item{new_time}{New time index, with respect to "from"}

\item{from}{A numerical value of the starting time for resampling (default 0)}

\item{to}{A numerical value of the end time for resampling (default inf, i.e. end of the data frame)}
}
\value{
A resampled movement data frame
}
\description{
Takes a movement data frame, removes value outside range (from, to), and
resamples to new index, where 0 in the new index corresponds to "from".
For use with 'summarise'.
}
