% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_min_length.R
\name{ts_min_length}
\alias{ts_min_length}
\title{Function to identify datasets with at least n samples}
\usage{
ts_min_length(dl_obj, n)
}
\arguments{
\item{dl_obj}{Neotoma download object}

\item{n}{integer; minimum number of samples}
}
\value{
A list of Neotoma dataset ids that have at least n samples
}
\description{
This function identifies Neotoma dastasets which have at least a user-defined minimum number of samples
}
\author{
M. Allison Stegner
}
