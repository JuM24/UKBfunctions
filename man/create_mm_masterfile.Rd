% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_mm_masterfile.R
\name{create_mm_masterfile}
\alias{create_mm_masterfile}
\title{Create the MM data frame}
\usage{
create_mm_masterfile(
  df,
  time_zero = NULL,
  mm_source,
  conv_table = NULL,
  mm_codes_file,
  random_seed = 24
)
}
\arguments{
\item{df}{The input data frame.}

\item{time_zero}{The date at which the diagnoses are to be ascertained; the
required format is \verb{\%d/\%m/\%Y}. If \code{NULL}, or when \code{mm_source = 'self_report'},
\code{time_zero} is set to the date of the UKB baseline assessment.}

\item{mm_source}{The source of disease history. Currently \code{self_report} and
\code{inpatient} are accepted.}

\item{conv_table}{The path to the ICD10/ICD9 conversion table.}

\item{mm_codes_file}{The path to the codes for each MM.}

\item{random_seed}{The number of \code{set.seed}.}
}
\description{
\code{create_mm_masterfile} takes the data frame with all the UKB data and
returns a data frame with participants as rows and binary indicators for
each disorder as columns.
}
