#' plotOutcome
#'
#' Generic function.
#' @export
plotOutcome <- function(msa) UseMethod("plotOutcome")

#' plotOutcome.msaWrapperOclass
#'
#' Plot bar chart of outcome classes for an ordinal class outcome.
#' @param msa The msaWrapper object to work with.
#' @export
plotOutcome.msaWrapperOclass <- function(msa){
  stopifnot(msa$type == "ordinal class data")

  data <- data.frame(msa$outcome)
  names(data) <- "outcome"

  ggplot(data, aes(outcome)) +
    geom_histogram(stat = "count")

}

#' plotOutcome.msaWrapperTte
#'
#' Plot bar chart of outcome classes for time to event outcome.
#' @param msa The msaWrapper object to work with.
#' @export
plotOutcome.msaWrapperTte <- function(msa){
  stopifnot(msa$type == "time to event data")

  outcome <- msa$outcome

  fit <- survival::survfit(survival::Surv(time, event) ~ 1,
                           data = outcome)

  survminer::ggsurvplot(fit, data = outcome,
             conf.int = TRUE,          # Add confidence interval
             risk.table = TRUE,        # Add risk table
             ggtheme = ggplot2::theme_bw())

}

#' plotOutcomeByClass
#'
#' Generic function.
#' @export
plotOutcomeByClass <- function(msa, ...) UseMethod("plotOutcomeByClass")

#' plotOutcomeByClass.msaWrapperOclass
#'
#' Plot bar chart of outcome classes for an ordinal class outcome.
#' @param msa The msaWrapper object to work with.
#' @param classVector Vector of factors that identify the classes.
#' @param strataName A string name for the class division.
#' @export
plotOutcomeByClass.msaWrapperOclass <- function(msa, classVector, strataName){
  stopifnot(msa$type == "ordinal class data")

  data <- data.frame(classVector, msa$outcome)
  names(data) <- c(strataName, "outcome")

  ggplot(data, aes_string("outcome", fill = strataName)) +
    geom_histogram(stat = "count")

}

#' plotOutcomeByClass.msaWrapperTte
#'
#' Plot bar chart of outcome classes for time to event outcome.
#' @param msa The msaWrapper object to work with.
#' @param classVector Vector of factors that identify the classes.
#' @param strataName A string name for the class division.
#' @export
plotOutcomeByClass.msaWrapperTte <- function(msa, classVector, strataName){
  stopifnot(msa$type == "time to event data")

  outcome <- cbind(classVector, msa$outcome)
  names(outcome)[1] <- strataName

  # Need to use surv_fit rather than survfit with code generated formula.
  form <- as.formula(paste("survival::Surv(time, event) ~", strataName))
  fit <- survminer::surv_fit(form, data = outcome)

  survminer::ggsurvplot(fit, data = outcome,
                        conf.int = TRUE,          # Add confidence interval
                        risk.table = TRUE,        # Add risk table
                        pval = TRUE,              # Add p value
                        ggtheme = ggplot2::theme_bw())

}
