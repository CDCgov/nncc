% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_strata_or.R
\name{test_mh}
\alias{test_mh}
\title{Calculate odds ratios}
\usage{
test_mh(case, exp, strata)
}
\arguments{
\item{case}{The case statuses}

\item{exp}{The exposure statuses}

\item{strata}{The strata identifiers}
}
\value{
The list of statistical results
}
\description{
Calculate odds ratios using the M-H method when the matched dataset has more
than 1 stratum, and using the Fisher's exact test when the matched dataset
has only one stratum.
}
\details{
For more information, please refer to the vignette using
\code{browseVignettes("nncc")}.
}
