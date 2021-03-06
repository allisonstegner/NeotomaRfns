% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_pollen.R
\name{clean_pollen}
\alias{clean_pollen}
\title{Function to clean pollen datasets from Neotoma}
\usage{
clean_pollen(pol_ds, type, topN = NULL, eco.group, list.name = NULL)
}
\arguments{
\item{pol_ds}{a single site from a Neotoma download object}

\item{type}{If type="pct", return is proportion. Else, counts are returned.}

\item{topN}{integer; allows user to limit result to the topN most abundant taxa. Default (NULL) is to return all taxa.}

\item{eco.group}{Neotoma ecological group codes. e.g., UPHE=upland herbs}

\item{list.name}{Neotoma pre-defined pollen list name}
}
\value{
A matrix of pollen counts or proportions
}
\description{
This function compiles pollen using Neotoma-defined pollen lists or selects user-specified ecological groups
And optionally limits to abundant taxa
Then calculates pollen percent or returns counts
}
\author{
M. Allison Stegner
}
