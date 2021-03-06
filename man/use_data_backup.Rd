% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_app.R
\name{use_data_backup}
\alias{use_data_backup}
\title{Restore previous shinyBZtopics data}
\usage{
use_data_backup(fname)
}
\arguments{
\item{fname}{The name of a folder in shinyBZtopics_data/}
}
\description{
TLDR: give the name of a backup folder in shinyBZtopics_data/ to restore this data.
}
\details{
All previously created data with \code{\link{create_bz_topics_data}} is stored in the shinyBZtopics_data folder,
with timestamps of the time the data was first created. Whenever you run run_topicbrowser, the most recent
data is used in the app. If you pass the name of one of the backup folder (named 'created_DATETIME')
to the use_data_backup function, it will create a new copy with a new timestamp.

Note that if you updated data (using if_existing = 'update'), this will have created a new data folder (the original data will still have its own backup)
}
