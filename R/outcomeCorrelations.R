#' generateOutcomeCorrelations
#'
#' This function correlates all covars against outcome.
#' It places the results in the msaWrapper.
#' Usually use kendall rank correlation for tte and ordinal class.
#' For tte data, use only the events, ignoring censored data.
#'
#' @param msa The msaWrapper object to work with.
#' @param method Correlation method to use.
#' @param textSize Data labels text size.
#' @param groups To restrict covars to those in certain groups.
#' @export
#'
generateOutcomeCorrelations <- function(msa, method = c("kendall", "pearson", "spearman")){

  method <- match.arg(method)

  if(class(msa) == "msaWrapperTte"){
    outcome <- ifelse(msa$outcome$event, msa$outcome$time, NA)
  } else {
    outcome <- msa$outcome
  }

  all_data <- msa$data
  covars <- vector()
  correlation <- vector()
  logpval <- vector()

  for(i in 1:dim(all_data)[2]){

    covar <- all_data[[colnames(all_data)[i]]]

    t = cor.test(covar, outcome, method = method)

    covars <- c(covars, colnames(all_data)[i])
    correlation <- c(correlation, t$estimate)
    logpval <- c(logpval, t$p.value)
    # take log below
  }

  #logpval <- log10(p.adjust(logpval, method = "fdr"))
  logpval <- log10(logpval)

  cor_data <- data.frame(covars, correlation, logpval)

  msa$outcomeCorrelations <- cor_data

  return(msa)
}


#' plotOutcomeCorrelations
#'
#' This function plots a volcano plot all covars against outcome.
#' Must use generateOutcomeCorrelations first.
#' The values and method use are determined by generateOutcomeCorrelations.
#'
#' @param msa The msaWrapper object to work with.
#' @param textSize Data labels text size.
#' @param groups To restrict covars to those in certain groups.
#' @export
#'
plotOutcomeCorrelations <- function(msa, textSize = 3, groups = vector()){

  if(dim(msa$outcomeCorrelations)[1] < 1) stop()

  if(length(groups)>0){
    cor_data <- msa$outcomeCorrelations[msa$group %in% groups,]
  } else {
    cor_data <- msa$outcomeCorrelations
  }

  e <- ifelse(cor_data$logpval < log10(0.1), 1, 0)

  ggplot(cor_data, aes(x = correlation, y = -logpval)) +
    geom_point() +
    geom_hline(aes(yintercept = -log10(0.05)), colour="light grey", linetype="dashed") +
    geom_text(data = cor_data[e==1,], aes(label = covars), size = textSize)

}
