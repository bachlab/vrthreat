% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_trial_functions.R
\name{resample_fruit_task_cols}
\alias{resample_fruit_task_cols}
\title{Resample fruit task data columns}
\usage{
resample_fruit_task_cols(
  df,
  .cols = tidyselect::contains("_fruittask0.csv_data"),
  start_resample_col,
  end_resample_col,
  max_duration,
  sample_rate = 10
)
}
\arguments{
\item{df}{A trial results data frame.}

\item{.cols}{Columns containing dataframes to resample.
(Default: all fruit task data). Each data frame must have a column 'time'
and a column 'events'.}

\item{start_resample_col}{Column containing start timestamps.}

\item{end_resample_col}{Column containing end timestamps.}

\item{max_duration}{Maximum duration of the resample.}

\item{sample_rate}{New sample rate (Hz).}
}
\value{
The data frame with a new columns of resampled movements added, with
new column name ending with `_resampled`.
}
\description{
Convenient wrapper around `resample_fruit_task`. For provided columns, resamples
fruit task dataframes contained in the #' columns between times stored in
`start_resample_col` and `end_resample_col`. #' Resampling is done up to a max
duration of `max_duration` with a sample rate #' `sample_rate`. Non-existing
time points for any trial are filled with "NA".
Returns the data frame with new columns added.
}
\details{
See `resample_movement_cols` for resampling movement or eyetracker data.
}
