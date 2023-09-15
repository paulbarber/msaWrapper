
#' predictElasticNet
# Could use ElasticNet model like Saddle Point
# Or https://cran.r-project.org/web/packages/glmnetcr/vignettes/glmnetcr.pdf
#'
#' Predict ordinal class outcome from  Binomial Elastic Net regularized model
#' @param elasnetmodel The buildElasticNet.msaWrapperOclass object built from buildElasticNet
#' @param testdata  Testdata to predict outcome
#' @param lambda Best Lambda value
#' @return A best_Predicted_class for the input
#' @export
#'
predictElasticNet <- function(elasnetmodel,testdata, lambda){
  stopifnot(elasnetmodel$type == "msaWrapperElasticNet")

  if(is.null(lambda)){
    lambda=elasnetmodel$strict_lambda
  }
  best_Predicted_class <- predict(elasnetmodel$glmnet_model, as.matrix(testdata),type ="link",s = lambda)

}

#' getElasticNetPerformance
#' @param predictResults Test data to predict with.
#' @param Outcome Test data to predict with.
#' @return return AUC
#' @export
#'
getElasticNetPerformance <- function(predictResults, Outcome){

  data <- data.frame(predictResults, Outcome)
  names(data) <- c("RiskScore", "Class")

  print(cor.test(data$RiskScore, data$Class, method = "kendall"))

  data$Class <- as.factor(data$Class)
  # Perform t-test and calculate p-value
  t.test_result <- t.test(RiskScore ~ Class, data =data)
  p_value <-sapply(t.test_result$p.value, JNCI_pvals)

  ggp <- ggplot(data, aes(x = Class, y = RiskScore, group = Class)) +
    geom_boxplot(outlier.colour = NA) +
    geom_jitter(width = 0.2, col = rgb(0.1, 0.2, 0.8, 0.3)) +
    theme_classic()+
    labs(title = paste("p-value", p_value))

  print(ggp)

  roc(data$Class, data$RiskScore,
      smoothed = TRUE, quiet = TRUE,
      # arguments for ci
      ci=TRUE, ci.alpha=0.95, stratified=FALSE,
      # arguments for plot
      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE,
      grid=TRUE,
      print.auc=TRUE, print.thres=TRUE)

  old_par = par()
  par(pty = "s")
  roc(data$Class, data$RiskScore,
      smoothed = TRUE, quiet = TRUE,
      # arguments for ci
      ci=TRUE, ci.alpha=0.95, stratified=FALSE,
      # arguments for plot
      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE,
      grid=TRUE,
      print.auc=FALSE, print.thres=FALSE, asp = NA)
  par(old_par)

  return (auc(data$Class, data$RiskScore))

}
