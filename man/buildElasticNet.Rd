% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildElasticNet.R
\name{buildElasticNet}
\alias{buildElasticNet}
\title{buildElasticNet
buildElasticNet.msaWrapperOclass}
\usage{
buildElasticNet(msa, iterations = 200, alpha = 1)
}
\arguments{
\item{msa}{The msaWrapper object to work with.}

\item{iterations}{The number of cv.glmnet calls to make.}
}
\value{
An object containing the trained model and it's performance.
}
\description{
Build a Binomial Elastic Net regularized model for ordinal class outcome
}
