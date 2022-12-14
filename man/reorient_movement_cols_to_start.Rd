% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_trial_functions.R
\name{reorient_movement_cols_to_start}
\alias{reorient_movement_cols_to_start}
\title{Reorient movement columns to starting position.}
\usage{
reorient_movement_cols_to_start(
  df,
  .cols = tidyselect::ends_with("waist_movement_data_0"),
  ref_pos_col = c()
)
}
\arguments{
\item{df}{A trial results data frame.}

\item{.cols}{Columns containing dataframes to reorient
(Default: waist tracker data).}

\item{ref_pos_col}{Reference position column (for example, generated with
`find_start_pos`). Default: no reference, will default to a list of zeros.}
}
\value{
The data frame with a new columns of resampled movements added, with
new column name ending with `_reoriented`.
}
\description{
Convenient wrapper around `reorient_movement_to_start`. For provided columns, reorients
movement dataframes contained in the provided columns.
Returns the data frame with new columns added.
}
