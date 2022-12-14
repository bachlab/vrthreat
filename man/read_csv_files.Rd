% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_functions.R
\name{read_csv_files}
\alias{read_csv_files}
\title{Read CSV Files}
\usage{
read_csv_files(
  data,
  data_dir,
  .cols = tidyselect::ends_with("_location_0"),
  drop_tld = TRUE,
  ...
)
}
\arguments{
\item{data}{A dataframe of trial results}

\item{data_dir}{Parent directory where data are stored}

\item{.cols}{A tidyselect specification of columns (e.g. `c(col1, col2)`).}

\item{drop_tld}{Drop top-level directory in stored file location? (default TRUE)}

\item{...}{Additional arguments passed to `read_csv`.
Default are columns that end with `_location_0`}
}
\description{
Read CSV files associated with trials (e.g. Movement files) from paths stored
in supplied columns (with top-level directory removed).
Column `directory` must be present in dataframe.
Function uses `read_csv` to read files.
}
