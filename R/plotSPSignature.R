# TODO Test and tidy up all these...
# TODO use survminer to do the KM plot in line with plot outcomes in this package.
# TODO Setup bootstrapping to get a 95% CI on the measures - maybe in another file? (see beta_tests)


#' plotSPSignatureOClass
#'
#' Plot performance of ordinal class signature/risk score.
#' Required from the results folder:
#'   RiskScoreDataset_ordinal_reg2.dat
#'
#' @param SPSig Object returned by buildSPSignature.msaWrapperOclass
#' @export
#'
plotSPSignatureOClass <- function(SPSig){

  stopifnot(class(SPSig)=="msaWrapperSPSignatureOclass")

  title <- SPSig$runName
  data.name <- SPSig$runName
  data.folder <- paste0(data.name, "/")
  results.folder <- paste0(data.folder, SPSig$regressionFolder, "/")

  # Load file of risk scores and outcomes
  risk.file <- paste0(results.folder, "RiskScoreDataset_ordinal_reg2.dat")
  N <- strex::str_last_number(grep("N=", readLines(risk.file), value = TRUE), decimals = F)
  data <- read.delim(risk.file, sep = " ", skip = 3,
                     header = F,
                     nrows = N)

  if(ncol(data) == 4){
    names(data) = c("ID", "RiskScore", "Unknown", "Class")
  }  else if(ncol(data) == 3){
    names(data) = c("ID", "RiskScore", "Class")
  }  else {
    names(data) = c("RiskScore", "Class")
  }

  # Perform t-test and calculate p-value
  t.test_result <- t.test(data$RiskScore ~ data$Class)
  p_value <-sapply(t.test_result$p.value, JNCI_pvals)


  print(cor.test(data$RiskScore, data$Class, method = "kendall"))

  data$Class <- as.factor(data$Class)

  ggp <- ggplot(data, aes(x = Class, y = RiskScore, group = Class)) +
    geom_boxplot(outlier.colour = NA) +
    geom_jitter(width = 0.2, col = rgb(0.1, 0.2, 0.8, 0.3)) +
    theme_classic()+
    labs(title = paste("p-value", p_value))

  print(ggp)
}



#' plotSPSignatureTte
#'
#' Plot performance of tte signature/risk score. Print other stats.
#' Required from the results folder:
#'  RiskScoreDataset_tte_reg2.dat
#'
#' @param SPSig Object returned by buildSPSignature.msaWrapperTte
#' @export
#'
plotSPSignatureTte <- function(SPSig){

  stopifnot(class(SPSig)=="msaWrapperSPSignatureOclass")

  title <- SPSig$runName
  data.name <- SPSig$runName
  data.folder <- paste0(data.name, "/")
  results.folder <- paste0(data.folder, SPSig$regressionFolder, "/")

  # Load file of risk scores and outcomes
  risk.file <- paste0(results.folder, "RiskScoreDataset_tte_reg2.dat")
  N <- strex::str_last_number(grep("N=", readLines(risk.file), value = TRUE), decimals = F)
  data <- read.delim(risk.file, sep = " ", skip = 3,
                     header = F,
                     col.names = c("ID", "RiskScore", "time", "event"),
                     nrows = N)

  if(ncol(data) == 4){
    names(data) = c("ID", "RiskScore", "time", "event")
  }  else {
    names(data) = c("RiskScore", "time", "event")
  }

  # C-index
  # Consider only events
  survivalscore <- -(data$RiskScore[data$event == 1])
  realeventtimes <- data$time[data$event == 1]
  concordancetest <- survival::concordance(realeventtimes ~ survivalscore)
  concordance <- concordancetest[["count"]][["concordant"]] + concordancetest[["count"]][["tied.xy"]]
  tied <- concordancetest[["count"]][["tied.x"]] + concordancetest[["count"]][["tied.y"]]
  cind <- (concordance+tied)/sum(concordancetest$count)

  print(paste("C-Index score:", signif(cind, digits = 3)))

  # rank correlation
  print(cor.test(data$RiskScore, data$time, method = "kendall"))

  # Plot KM curve, and get log rank pval
  breaks <- quantile(data$RiskScore, probs = c(0.0, 0.5, 1.0), na.rm = T)
  data$Risk <- cut(data$RiskScore, breaks = breaks, labels = c("low", "high"),
                   include.lowest = T)

  data$SurvObj <- with(data, Surv(time, event))

  title <- data.name
  col <- c("orange", "magenta")
  km <- survfit(SurvObj ~ Risk, data=data)

  print(plot(km, col = col, xlab="time", ylab="Prob. survival", main = title, mark.time = TRUE))
  legend("topright", col = col, lty = 1, legend = names(km$strata), cex=0.75)
  #      print(km)

  print(survminer::ggrisktable(km, data=data) + theme_cleantable())
  print(survival::survdiff(SurvObj ~ Risk, data=data))
  print(summary(coxph(SurvObj ~ Risk, data=data)))

  print(ggp)
}

#' plotSPSignatureBetaSwimmers
#'
#' Plot of Betas through the optimisation of SP Signature batch regression.
#'
#' Required from data folder:
#'   <data.name>.names file
#'
#' Required from the parent folder of data.folder
#'   SPS_Signature_logfile.txt
#'
#' Required from results.folder
#'  Betas_optimised.txt
#'   Beta_values.txt
#'
#' @param SPSig Object returned by buildSPSignature.msaWrapperTte
#' @param show_best_performance_line TRUE/FALSE to show or not this dotted line at the optimal number of covariates.
#' @param selectedSig TRUE/FALSE to plot full or selected Signatures in the Plot;
#' @param textreducescale To reduce the Text size, pass value >1; Useful for lower number of covariates
#' @export
#'
plotSPSignatureBetaSwimmers <- function(SPSig, show_best_performance_line = TRUE, selectedSig=FALSE, textreducescale=1) {

  title <- SPSig$runName
  data.name <- SPSig$runName
  data.folder <- paste0(data.name, "/")
  results.folder <- paste0(data.folder, SPSig$regressionFolder, "/")

  # Get covariate names
  covar.names <- read.delim(paste0(results.folder, "RiskScoreEngine/", data.name, ".names"), sep = " ", header = F)
  covar.names <- covar.names[,2]

  # Get critical Beta value
  critical.value <- strex::str_last_number(grep("critical line:",
                                         readLines(paste0(results.folder, "Betas_optimised.txt")),
                                         value = TRUE), decimals = T)

  # Read all the Beta values for all optimisation stages
  # Betas[stage, covariate] and corresponding Betas.error
  Betas <- read.delim(paste0(results.folder, "Beta_values.txt"), sep = ",", header = F)
  Betas <- Betas[,-c(dim(Betas)[2])]   # Last col was all NA
  Betas <- Betas[-grep("active=", Betas[,1]),]  # Alternate rows are labels
  colnames(Betas) <- covar.names

  Betas.error <- as.data.frame(sapply(Betas, strex::str_last_number, decimals = T))
  Betas <- as.data.frame(sapply(Betas, strex::str_first_number_after_first, pattern = "]=",
                                decimals = T, negs = T))

  # Get progression of active covariates from active_covariates.txt
  #active <- read.delim(paste0(results.folder, "active_covariates.txt"), sep = " ", header = F)
  #active <- active[,-c(1)] # first col is a label, get rid

  # Find the optimal points
  optimum_performance <- strex::str_last_number(grep("proposed nr of covariates, test data performance:",
                                              readLines(paste0(data_name,
                                                               "_SPS_Signature_logfile.txt")),
                                              value = TRUE))
  optimum_overfitting <- strex::str_last_number(grep("proposed nr of covariates, avoiding overfitting:",
                                              readLines(paste0(data_name,
                                                               "_SPS_Signature_logfile.txt")),
                                              value = TRUE))


  if (selectedSig == TRUE)
  {
    Betas <- Betas[, colSums(Betas == 0.00) < 2*optimum_performance]
  }

  data_long <- tidyr::gather(Betas, Covariate, Beta, 1:dim(Betas)[2], factor_key=TRUE)
  data_long$Active <- dim(Betas)[1]:1
  text.size <- 256 / ((dim(Betas)[2])*textreducescale)   # by trial and error, this is a good height, if names not too long.
  largest.Beta <- max(2*critical.value, max(abs(data_long$Beta)))
  data_long$Beta <- ifelse(data_long$Beta == 0.0, NA, data_long$Beta)  # make zero values NA so they become transparent in the heatmap

  ggp <- ggplot(data_long, aes(y = Covariate, x = Active, fill = Beta)) +
    theme_light() +
    theme(axis.text.y = element_text(size = text.size)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("dark green", "#AACCAA", "light blue", "#CCAAAA", "red"),
                         limits = c(-largest.Beta, largest.Beta),
                         values = scales::rescale(c(-1/largest.Beta, -critical.value, 0, critical.value, 1/largest.Beta), c(0, 1)),    # scale with extreme colours at beta=1.0
                         na.value = "transparent") +
    geom_vline(xintercept = optimum_overfitting, linetype="dashed") +
    ggtitle(title) +
    scale_y_discrete(limits = rev)

  if(show_best_performance_line){
    ggp <- ggp + geom_vline(xintercept = optimum_performance, linetype="dashed")
  }

  print(ggp)

}


#' plotBetaTable
#'
#' Plot of Betas through the optimisation of SP Signature batch regression using betas_with_errors_optimised.txt
#'
#' Required from the parent folder of data.folder
#'   betas_with_errors_optimised.txt
#'
#' @param results.folder Result folder to get required file
#' @param cex Text font
#' @export
#'
plotBetaTable <- function(results.folder, cex = 0.5 , rowspacing = 0.5, col = 1, pch = 18, xmin = -2.9, xmax = 4.2,
                           betaTicks = c(-1.0, -0.5, 0, 0.5, 1.0),
                           covariatepos = xmin, betapos = 2.1, pvaluepos = 3.2,
                           rankpos = 3.6,weightpos = 4,
                           useRank = FALSE, useWeight = FALSE){

  raw.dataIn <- read.delim(paste0(results.folder, "betas_with_errors_optimised.txt"),
                           sep = ",", header = F)

  Covariate <- str_before_first(raw.dataIn$V1, ":")
  Beta <- str_first_number_after_first(raw.dataIn$V1, "]=", decimals = T, negs = T)
  BetaLower <- Beta - str_first_number_after_first(raw.dataIn$V1, "pm ", decimals = T, negs = T)
  BetaUpper <- Beta + str_first_number_after_first(raw.dataIn$V1, "pm ", decimals = T, negs = T)
  Pvalue <- str_first_number(raw.dataIn$V3, decimals = T)
  Weight <- str_first_number(raw.dataIn$V2, decimals = T)
  Rank <- str_last_number(raw.dataIn$V1, decimals = F)

  Pvalue <- sapply(Pvalue, JNCI_pvals)

  # Get critical beta value
  critical.value <- str_last_number(grep("critical line:",
                                         readLines(paste0(results.folder, "betas_optimised.txt")),
                                         value = TRUE), decimals = T)

  mydf <- data.frame(
    Covariate,
    Beta,
    BetaLower,
    BetaUpper,
    Pvalue,
    Rank,
    Weight,
    stringsAsFactors=FALSE
  )

  # extremes of the page
  xmin_v <- xmin
  xmax_v <- xmax


  nrows = nrow(mydf)
  rowseq <- seq(nrows*rowspacing, 0.1, length.out = nrows)
  title_pos = (nrows + 1.2) * rowspacing
  pos4offset = -2*rowspacing/19

  # do the beta plot
  par(mai=c(1,0,0,0))
  plot(NULL, xlim=c(xmin_v, xmax_v), ylim=c(0, title_pos+rowspacing),
       xlab='', ylab='', yaxt='n', xaxt='n',
       bty='n')
  points(mydf$Beta, rowseq, pch=pch, col=col)
  axis(1, betaTicks, labels = as.character(betaTicks), cex.axis=cex)

  # add vertical lines at beta=0 and critical values
  segments(0, -1, 0, (nrows + 0.5) * rowspacing, lty=1)
  segments(critical.value, -1, critical.value, (nrows + 0.5) * rowspacing, lty=3)
  segments(-critical.value, -1, -critical.value, (nrows + 0.5) * rowspacing, lty=3)

  # add confidence interval lines
  segments(mydf$BetaLower, rowseq, mydf$BetaUpper, rowseq, col=col)

  # x axis legend
  mtext('Beta', 1, line=2.5, at=0, cex=cex, font=1)

  # other cols

  pos <- covariatepos
  text(pos, title_pos+pos4offset, "Covariate", cex=cex, font=0.1, pos=4)
  t <- ifelse(!is.na(mydf$Covariate), mydf$Covariate, '')
  text(pos, rowseq+pos4offset, t, cex=cex, font=1, pos=4)  # set font=3 for italic


  pos <- betapos
  text(pos, title_pos, "Beta", cex=cex, font=1)
  t <- ifelse(!is.na(mydf$Beta), with(mydf, paste(format(round(Beta, 2), nsmall = 2),' (',
                                                  format(round(BetaLower, 2), nsmall = 2),'-',
                                                  format(round(BetaUpper, 2), nsmall = 2),')',sep='')), '')
  text(pos, rowseq, t, cex=cex)

  pos <- pvaluepos
  text(pos, title_pos, "p-value", cex=cex, font=1)
  t <- ifelse(!is.na(mydf$Pvalue), mydf$Pvalue, '')
  text(pos, rowseq, t, cex=cex)

  if(useWeight){
    pos <- weightpos
    text(pos, title_pos, "Z-score", cex=cex, font=1)
    t <- ifelse(!is.na(mydf$Weight), format(mydf$Weight, big.mark=","), '')
    text(pos, rowseq, t, cex=cex)
  }


  if(useRank){
    pos <- rankpos
    text(pos, title_pos, "Rank", cex=cex, font=1)
    t <- ifelse(!is.na(mydf$Rank), format(mydf$Rank, big.mark=","), '')
    text(pos, rowseq, t, cex=cex)
  }


}
