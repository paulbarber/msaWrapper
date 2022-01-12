#' reduceByCorrelationWithinGroups
#'
#' Reduce the number of covars by removing those that are strongly correlated
#' within the groups, retaining the one best correlated with outcome.
#' @param msa The msaWrapper object to work with.
#' @param method Correlation method to use.
#' @return A new msaWrapper object with reduced data.
#' @export
reduceByCorrelationWithinGroups <- function(msa,
                                            method = c("kendall", "pearson", "spearman"),
                                            threshold = 0.6){

  method <- match.arg(method)

  msa <- generateOutcomeCorrelations(msa, method)

  cor_data <- msa$outcomeCorrelations
  group <- msa$group
  group_names <- msa$groupLabels

  # get data correlations
  cormat <- cor(msa$data, method = method, use = "pairwise.complete.obs")

  # order the table
  outcome_cor <- cor_data[order(abs(cor_data$correlation), decreasing = T),]

  keep <- vector()
  discard <- vector()

  for(i in 1:length(outcome_cor$covars)){
    var_name <- outcome_cor$covars[i]
    g = group[var_name == names(msa$data)]

    cat(paste("Check:", i, var_name, group_names[g], "\n"))

    # skip those already in discard list
    if(var_name %in% discard){
      cat(" Already discarded.\n")
      next
    }

    # add to keep list
    keep <- c(keep, var_name)

    # get correlations with other variables
    cors <- cormat[var_name,]

    vstrong_vals <- cors[abs(cors) > threshold]
    vstrong_cors <- names(cors)[abs(cors) > threshold]

    # remove var_name and any NAs
    vstrong_vals <- vstrong_vals[!(vstrong_cors == var_name)]
    vstrong_cors <- vstrong_cors[!(vstrong_cors == var_name)]
    vstrong_vals <- vstrong_vals[!is.na(vstrong_vals)]
    vstrong_cors <- vstrong_cors[!is.na(vstrong_cors)]

    # retain those in same group
    vstrong_cors <- vstrong_cors[group[vstrong_cors]==g]

    # add others to discard list
    discard <- c(discard, vstrong_cors)

    if(length(vstrong_cors>0))
      cat(paste(" Discarding", vstrong_cors, signif(vstrong_vals, 3), "\n"))
  }

  # reduce the data
  new_data <- msa$data[, keep]
  new_group <- msa$group[keep]
  new_groupLabels <- msa$groupLabels

  new_msa <- msaWrapperCreate(new_data, msa$outcome,
                              group = new_group, groupLabels = new_groupLabels)

  return(new_msa)
}

