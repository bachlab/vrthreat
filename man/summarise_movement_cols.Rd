% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_trial_functions.R
\name{summarise_movement_cols}
\alias{summarise_movement_cols}
\title{Summarise movement data columns to ensure unique time stamps}
\usage{
summarise_movement_cols(
  df,
  .cols = tidyselect::ends_with("_movement_data_0") & !(tidyselect::starts_with("valid")),
  by = "time"
)
}
\arguments{
\item{df}{A trial results data frame.}

\item{.cols}{Columns containing movement dataframes
(Default: all movement data).}

\item{by}{Time stamp column, default is `time`}
}
\value{
The same data frame with old movement data columns replaced
}
\description{
Convenient wrapper around `summarise_movement`. For provided columns,
summarises movement data frames and returns the same data frame with unique
time stamp values and a new column `n` to indicate duplicated time stamps.
}
