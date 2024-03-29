% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictElasticNet.R
\name{predictElasticNet}
\alias{predictElasticNet}
\title{predictElasticNet}
\usage{
predictElasticNet(elasnetmodel, testdata, lambda)
}
\arguments{
\item{elasnetmodel}{The buildElasticNet.msaWrapperOclass object built from buildElasticNet}

\item{testdata}{Testdata to predict outcome}

\item{lambda}{Best Lambda value}
}
\value{
A best_Predicted_class for the input
}
\description{
Predict ordinal class outcome from  Binomial Elastic Net regularized model
}
