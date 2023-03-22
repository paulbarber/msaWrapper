#' predictSPSignature
#' @param testData Test data to predict with.
#' @param runName Test data to predict with.
#' @param regressionFolder The regressionfolder saved in the training step.
#' @return Predicted Results
#' @export
#'
predictSPSignature <- function(testData, runName, regressionFolder=Reg_SETCV_MAP_L2){

  # extract basic results

  sigFile <- paste0(runName, "/", regressionFolder, "/RiskScore_formula.txt")
  if(!file.exists(sigFile)) stop("No Riskscore found. Is it a valid foder?")

  riskSignatureDataframe <- read.table(sigFile,
                                       sep = '*', col.names = c("Weight", "Covariate"),
                                       stringsAsFactors = F, skip = 2, fill = T)

  print(riskSignatureDataframe)
  testresults <-calculateRiskScore(riskSignatureDataframe, testData)
  return (testresults)

}


#' getSPSignaturePerformance
#' @param predictResults Test data to predict with.
#' @param Outcome Test data to predict with.
#' @return return AUC
#' @export
#'
getSPSignaturePerformance <- function(predictResults, Outcome){

  data <- data.frame(predictResults, Outcome)
  names(data) <- c("RiskScore", "Class")

  print(cor.test(data$RiskScore, data$Class, method = "kendall"))

  data$Class <- as.factor(data$Class)

  ggp <- ggplot(data, aes(x = Class, y = RiskScore, group = Class)) +
    geom_boxplot(outlier.colour = NA) +
    geom_jitter(width = 0.2, col = rgb(0.1, 0.2, 0.8, 0.3)) +
    theme_classic()

  print(ggp)

  roc(data$Class, data$RiskScore,
      smoothed = TRUE, quiet = TRUE,
      # arguments for ci
      ci=TRUE, ci.alpha=0.95, stratified=FALSE,
      # arguments for plot
      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE,
      grid=TRUE,
      print.auc=TRUE, print.thres=TRUE)
  return (ci.auc(data$Class, data$RiskScore))

}
