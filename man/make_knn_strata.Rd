% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_knn_strata.R
\name{make_knn_strata}
\alias{make_knn_strata}
\title{Make case-control strata using k nearest neighbors (knn)}
\usage{
make_knn_strata(
  expvar,
  matchvars,
  df,
  rmvars = data.frame(exp_var = character(), rm_vars = character(), stringsAsFactors =
    FALSE),
  casevar = "case",
  ncntls = 250,
  metric = "gower",
  silent = FALSE
)
}
\arguments{
\item{expvar}{A character - the name of the exposure variable in \code{df}.}

\item{matchvars}{Character vector - what are the variables to match on. Note
that the function automatically excludes the the exposure variable.}

\item{df}{A dataframe that contains the case-control data.}

\item{rmvars}{A data frame that lists variables to be excluded from matching
for each exposure. For details, please see the vignette of this package.}

\item{casevar}{A character - what is the name of the variable indicating case
status (1 = case, 0 = control)}

\item{ncntls}{An integer to specify number of controls to find for each case
(k in knn).}

\item{metric}{A character to specify a metric for measuring distance between
a case and a control. See \code{\link[cluster]{daisy}}.}

\item{silent}{Suppress exposure info useful for *apply/loop implementations?}
}
\value{
A list of data frames with a \code{length} of number of exposures of
  interest.
}
\description{
Select a pre-defined number of controls for each case based on calculated
distances between cases and controls.
}
\details{
For more information, please refer to the vignette using
\code{browseVignettes("nncc")}.
}
