% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/questionnaire_functions.R
\name{replace_redcapid_ppid}
\alias{replace_redcapid_ppid}
\title{Replace Redcap id in questionnaires by VR ppid}
\usage{
replace_redcapid_ppid(df, redcap_ppid_map)
}
\arguments{
\item{df}{raw questionnaire data frame}

\item{redcap_ppid_map}{redcap id (survey_id) to VR id (ppid) map}
}
\value{
raw questionnaire data frame with updated ppid_quest
}
\description{
Replaces the Redcap record id of the questionnaire data by the ppid of the VR data.
Removes records of participants that did the questionnaire several times or didn't do the VR study.
}
