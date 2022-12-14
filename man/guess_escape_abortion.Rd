% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing_movement_functions.R
\name{guess_escape_abortion}
\alias{guess_escape_abortion}
\title{Guess escape abortion position and time}
\usage{
guess_escape_abortion(
  ref_movement,
  ref_position,
  begin_escape_time,
  end_escape_time,
  end_state
)
}
\arguments{
\item{ref_movement}{Movement data frame}

\item{ref_position}{Reference position of safe place}

\item{begin_escape_time}{Start of escape}

\item{end_escape_time}{End of escape}

\item{end_state}{End state}
}
\description{
Guess whether escape was aborted and extract position (distance from safe place)
and absolute time. Looks for trials with outcome "survived" in which the
minimum distance from the safe place was achieved at least 0.5 s before trial
end (as derived from `guess_escape_end_time()`)
}
