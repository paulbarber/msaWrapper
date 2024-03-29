% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataCorrelations.R
\name{plotDataCorrelations}
\alias{plotDataCorrelations}
\title{plotDataCorrelations}
\usage{
plotDataCorrelations(
  msa,
  method = c("kendall", "pearson", "spearman"),
  textSize = 5,
  groups = vector()
)
}
\arguments{
\item{msa}{The msaWrapper object to work with.}

\item{method}{Correlation method to use.}

\item{textSize}{Axes labels text size.}

\item{groups}{To restrict covars to those in certain groups.}
}
\description{
This function plots a correlation matrix of all covars against all others.
}
