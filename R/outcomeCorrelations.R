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
generateOutcomeCorrelations <- function (msa, method = c("kendall", "pearson", "spearman", "AUC")) 
{
  method <- match.arg(method)
  if (class(msa) == "msaWrapperTte") {
    outcome <- ifelse(msa$outcome$event, msa$outcome$time, 
                      NA)
  }
  else {
    outcome <- as.numeric(msa$outcome)
  }
  
  all_data <- msa$data
  
  covars <- vector()
  
  correlation <- vector()
  logpval <- vector()
  
  
  if(method == "AUC"){
    
    if (length(unique(outcome)) > 2)  # not binary outcome
      stop()
    
    for (i in 1:dim(all_data)[2]) {
      covar <- all_data[[colnames(all_data)[i]]]
      
      p <- suppressMessages(pROC::roc(outcome, covar))
      
      ci <- pROC::ci.auc(p)
      
      # Calc p value from ci (https://www.bmj.com/content/343/bmj.d2304)
      se = (ci[3]-ci[1])/(2*1.96)   # calc std err
      z = (ci[2]-0.5)/se            # use difference of estimate to NULL
      p.value = exp(-0.717*z - 0.416*z^2)
      
      covars <- c(covars, colnames(all_data)[i])
      correlation <- c(correlation, ci[2])
      logpval <- c(logpval, p.value)
    }
    
  } else {
    for (i in 1:dim(all_data)[2]) {
      covar <- all_data[[colnames(all_data)[i]]]
      t = cor.test(covar, outcome, method = method)
      covars <- c(covars, colnames(all_data)[i])
      correlation <- c(correlation, t$estimate)
      logpval <- c(logpval, t$p.value)
    }
  }
  
  
  logpval <- log10(logpval)
  cor_data <- data.frame(covars, correlation, logpval)
  msa$outcomeCorrelations <- cor_data
  msa$outcomeCorrelationsMethod <- method
  return(msa)
}

#' plotOutcomeCorrelations
#'
#' This function plots a volcano plot all covars against outcome.
#' Must use generateOutcomeCorrelations first.
#' The values and method use are determined by generateOutcomeCorrelations.
#'
#' @param msa The msaWrapper object to work with.
#' @param labelAlpha Significance Value level to display Signature names
#' @param hlineAlpha Horizontal line significance Value leve
#' @param textSize Data labels text size.
#' @param groups To restrict covars to those in certain groups.
#' @export
#'
plotOutcomeCorrelations <- function(msa, labelAlpha=0.1, hlineAlpha=0.05, textSize = 3, groups = vector()){
  
  if(dim(msa$outcomeCorrelations)[1] < 1) stop()
  
  if(length(groups)>0){
    cor_data <- msa$outcomeCorrelations[msa$group %in% groups,]
  } else {
    cor_data <- msa$outcomeCorrelations
  }
  
  e <- ifelse(cor_data$logpval < log10(labelAlpha), 1, 0)  #Display Signature anems if p values is lower than Significance
  
  ggplot(cor_data, aes(x = correlation, y = -logpval)) +
    geom_point() +
    geom_hline(aes(yintercept = -log10(hlineAlpha)), colour="light grey", linetype="dashed") +
    geom_text(data = cor_data[e==1,], aes(label = covars), size = textSize) +
    labs(x = msa$outcomeCorrelationsMethod)
  
}
