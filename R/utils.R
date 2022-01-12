#' msaWrapperCreate
#'
#' This function creates a msaWrapper object that contains the data
#' and will contain future results..
#' @param data A data frame of numeric data (one hot encoded etc).
#' @param outcome A vector of classes (for ordinal class data), or a data frame (for time to event data) with 2 cols: time, event
#' @param rowsLabelled Indicates if data contains an ID as first column T/F.
#' @param group A vector of group labels for each covariate.
#' @param groupLabels A vector of names for the groups 1-G.
#' @export

msaWrapperCreate <- function(data, outcome, rowsLabelled = F,
                             group = NA, groupLabels = NA){

  if(is.vector(outcome)){ # Ordinal class data
    outcome <- as.factor(outcome)
    type <- "ordinal class data"
    thisClass <- "msaWrapperOclass"
  } else if (is.data.frame(outcome)){ # tte data
    names(outcome) <- c("time", "event")
    type <- "time to event data"
    thisClass <- "msaWrapperTte"
  } else {
    stop("outcome must be a vector or data.frame")
  }

  if(rowsLabelled){
    rowLabels <- data[,1]
    data <- data[,-1]
  } else {
    rowLabels <- vector()
  }

  if(is.na(group)) group <- rep(1, dim(data)[2])
  if(is.na(groupLabels)) groupLabels <- c("All")

  names(group) <- names(data)
  colLabels <- names(data)

  outcomeCorrelations <- data.frame(covars=character(),
                                    correlation=double(),
                                    logpval=double())

  # return this structure with the correct class
  structure(list(type, data, rowLabels, colLabels,
                 group, groupLabels,
                 outcome, outcomeCorrelations),
            .Names = c("type", "data", "rowLabels", "colLabels",
                       "group", "groupLabels",
                       "outcome", "outcomeCorrelations"),
            class = thisClass)

}