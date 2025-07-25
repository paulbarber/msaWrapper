#' predictSPSignature
#' @param testData Test data to predict with.
#' @param runName Test data to predict with.
#' @param regressionFolder The regressionfolder saved in the training step.
#' @return Predicted Results
#' @export
#'
predictSPSignature <- function(testData, runName, regressionFolder="Reg_SETCV_MAP_L2"){

 # extract basic results
  
  sigFile <- paste0(runName, "/", regressionFolder, "/RiskScore_formula.txt")
  if(!file.exists(sigFile)) stop("No Riskscore found. Is it a valid folder?")
  
  lines <- readLines(sigFile)
  
  # The Risk signature file now (July 2025) contains 2 signatures
  # Find the line number containing the target string for the non-spurious version
  start_line <- grep("non-spurious covariates only", lines)[1]  # [1] ensures the first match
  
  relevant_lines <- lines[start_line:length(lines)]
  
  # Write the relevant lines to a temporary file
  temp_file <- tempfile()
  writeLines(relevant_lines, temp_file)
  
  # Read the table from the temporary file
  riskSignatureDataframe <- read.table(temp_file,
                                       sep = '*', col.names = c("Weight", "Covariate"),
                                       stringsAsFactors = F, skip = 1, fill = T)
  
  # Clean up the temporary file
  unlink(temp_file)
  
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

  par(pty = "s")
  roc(data$Class, data$RiskScore,
      smoothed = TRUE, quiet = TRUE,
      # arguments for ci
      ci=TRUE, ci.alpha=0.95, stratified=FALSE,
      # arguments for plot
      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE,
      grid=TRUE,
      print.auc=FALSE, print.thres=FALSE, asp = NA)
  return (auc(data$Class, data$RiskScore))

}
