% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_hiatus_datasets.R
\name{cut_hiatus_datasets}
\alias{cut_hiatus_datasets}
\title{Function to identify datasets that have excessivley long hiatuses}
\usage{
cut_hiatus_datasets(dl_obj, prop.length.cutoff, agemodels)
}
\arguments{
\item{dl_obj}{Neotoma download object}

\item{agemodels}{list object of agemodels. Can be provided by user, or downloaded from Neotoma. If downloaded from Neotoma, default chronology is used.}

\item{prop.length}{proportionate hiatus length, as a propotion of the total dataset duration}
}
\value{
A list of Neotoma dataset ids for sites that do not have hiatuses
}
\description{
This function identifies Neotoma dastasets which have hiatuses of user-defined length, and returns a list of dataset ids for the remaining sites (which do not have hiatuses)
}
\author{
M. Allison Stegner
}
