% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildSPSignature.R
\name{calculateTreatmentResponseScore}
\alias{calculateTreatmentResponseScore}
\title{calculateTreatmentResponseScore}
\usage{
calculateTreatmentResponseScore(benefitSignatureDataframe, data)
}
\arguments{
\item{data}{The data frame that contains the covariates}

\item{riskSignature}{The data frame of the risk signature}
}
\value{
The vector of risk scores, one for each row of data
}
\description{
Hint, load from a weights file like this:
riskSignatureDataframe <- read.table("RiskScore_formula.txt",
sep = '*', col.names = c("Weight", "Covariate"),
stringsAsFactors = F, skip = 2, fill = T)
}
