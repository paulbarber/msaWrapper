#' buildSPSignature
#'
#' Generic function.
#' @param msa The msaWrapper object to work with.
#' @export
#'
buildSPSignature <- function(msa, runName, iterations) UseMethod("buildSPSignature")

#' buildSPSignature.msaWrapperOclass
#'
#' Build a Saddle Point Signature for ordinal class outcome.
#' This requires the installation and valid license for that software.
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cross validation iterations to make.
#' @return An object containing the signature and it's performance.
#' @export
#'
buildSPSignature.msaWrapperOclass <- function(msa, runName, iterations=200){

  filename <- runName
  templateIniFilename <- "msaWrapper_SPSignature_tte_template.ini"
  iniFilename <- paste0(runName, ".ini")
  regression_folder <- "Reg_SETCV_MAP_L2"

  data <- cbind(msa$data, msa$outcome)
  names(data) <- c(names(msa$data), names(msa$outcome))
  C <- nlevels(as.factor(msa$outcome))  # number of levels in the class outcome
  export_SPS_file(data, filename, type = 4, C = C)

  # make SPS ini files
  createTemplateIni_BatchRegression(templateIniFilename)
  ini.data <- ini::read.ini(templateIniFilename)
  ini.data$SESSION$`project dir` <-  getwd()
  ini.data$DATASET$filename <- filename
  ini.data$DATASET$type <- "Ordinal, no sample IDs"
  ini.data$`BATCH REGRESSION`$`number of training sets` <- iterations
  ini.data$`OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION`$`analysis dir` <- filename
  ini.data$`OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION`$`regression dir` <- regression_folder
  ini::write.ini(ini.data, iniFilename)
  # replace "=" by " = " because SPS needs the spaces that ini package does not add.
  addSpacesToIniFile(iniFilename)

  # run SPS
  system(paste("SaddlePoint-Signature.exe", iniFilename))

  # extract basic results
  sigFile <- paste(filename, "\\", regression_folder, "\\RiskScore_formula.txt")
  if(!file.exists(sigFile)) stop("No output produced by Saddle Point software. Is it installed with a valid license?")

  riskSignatureDataframe <- read.table(sigFile,
                                       sep = '*', col.names = c("Weight", "Covariate"),
                                       stringsAsFactors = F, skip = 2, fill = T)

  print(riskSignatureDataframe)

  riskSignatureDataframe
}

#' buildSPSignature.msaWrapperTte
#'
#' Build a Saddle Point Signature for time to event outcome.
#' This requires the installation and valid license for that software.
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cross validation iterations to make.
#' @return An object containing the signature.
#' @export
#'
buildSPSignature.msaWrapperTte <- function(msa, runName, iterations=200){

  filename <- runName
  templateIniFilename <- "msaWrapper_SPSignature_tte_template.ini"
  iniFilename <- paste0(runName, ".ini")
  regression_folder <- "Reg_SETCV_MAP_L2"

  data <- cbind(msa$data, msa$outcome)
  names(data) <- c(names(msa$data), names(msa$outcome))
  R <- nlevels(as.factor(msa$outcome$event)) # Number of event types
  export_SPS_file(data, filename, type = 2, R = R)

  # make SPS ini files
  createTemplateIni_BatchRegression(templateIniFilename)
  ini.data <- ini::read.ini(templateIniFilename)
  ini.data$SESSION$`project dir` <-  getwd()
  ini.data$DATASET$filename <- filename
  ini.data$DATASET$type <- "TTE, no sample IDs"
  ini.data$`BATCH REGRESSION`$`number of training sets` <- iterations
  ini.data$`OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION`$`analysis dir` <- filename
  ini.data$`OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION`$`regression dir` <- regression_folder
  ini::write.ini(ini.data, iniFilename)
  # replace "=" by " = " because SPS needs the spaces that ini package does not add.
  addSpacesToIniFile(iniFilename)

  # run SPS
  system(paste("SaddlePoint-Signature.exe", iniFilename))

  # extract basic results
  sigFile <- paste(filename, "\\", regression_folder, "\\RiskScore_formula.txt")
  if(!file.exists(sigFile)) stop("No output produced by Saddle Point software. Is it installed with a valid license?")

  riskSignatureDataframe <- read.table(sigFile,
                                       sep = '*', col.names = c("Weight", "Covariate"),
                                       stringsAsFactors = F, skip = 2, fill = T)

  print(riskSignatureDataframe)

  riskSignatureDataframe
}

#' addSpacesToIniFile
#'
#' replace "=" by " = " in a file.
#' @param filename The file to read from and write to.
#'
addSpacesToIniFile <- function(filename){
  tx  <- readLines(filename)
  tx  <- gsub("=", " = ", x = tx)
  writeLines(tx, con = filename)
}


#' export_SPS_file
#'
#' Export a data frame to .data and .names files for use with Saddle Point software
#'
#' Saddle Point data types
#' 1: time-to-event outcome data with sample identifiers
#' 2: time-to-event outcome data without sample identifiers
#' 3: ordinal class outcome data with sample identifiers
#' 4: ordinal class outcome data without sample identifiers
#' 5: general real outcome data with sample identifiers
#' 6: general real outcome data without sample identifiers
#'
#' @param export_dat Data frame of covariates and outcome (last columns), which may have sample identifiers as the first column.
#' @param export_name A name for the data to be used for file names.
#' @param type A Saddle Point data type. See description.
#' @param C For ordinal class data, the number of classes.
#' @param R For time to event data, the number of event types.
#' @export
#'
export_SPS_file <- function(export_dat, export_name, type, C=NA, R=NA){

  N = dim(export_dat)[1]
  P = dim(export_dat)[2]

  # Get the right inputs for tte or other
  if(type %in% c(1, 2, 5, 6)){
    if(is.na(R)){
      stop("type is tte or real data, but R was not provided.")
    }
    outcomes <- paste0("R=", R, "\n")
    P = P - 2
  }
  if(type %in% c(3, 4)){
    if(is.na(C)){
      stop("type is class, but C was not provided.")
    }
    outcomes <- paste0("C=", C, "\n")
    P = P - 1
  }

  # Mask for column names
  names.mask <- c(rep(T,P),F,F,F,F)
  # If we have sample identifiers, mask that at the beginning
  if(type %in% c(1, 3, 5)){
    P = P - 1
    names.mask <- c(F,rep(T,P),F,F,F,F)
  }

  # Export (see APPENDIX for details)
  {sink(paste0(export_name, ".dat"))
    cat(paste0("N=", N, "\n"))
    cat(paste0("P=", P, "\n"))
    cat(outcomes)
    sink()}
  write.table(export_dat, file = paste0(export_name, ".dat"), append = T, sep = " ",
              quote = F, row.names = F, col.names = F)
  write.table(names(export_dat)[names.mask], file = paste0(export_name, ".names"),
              quote = F, sep = " ", row.names = T, col.names = F)

}

#' createTemplateIni_BatchRegression
#'
#' Create a template SPS ini file for batch regression.
#' @param filename The filename to write to.
#'
createTemplateIni_BatchRegression <- function(filename){
  sink(file = filename)

  cat("[GENERAL SETTINGS]\n")
  cat("test mode = off\n")
  cat("multi-threading = on\n")
  cat("cov pre-selection = rank correlation with outcome\n")
  cat("cov selection in loop = removal based on full coefficient distribution\n")
  cat("overfitting suppression = L2 MAP regression, width = 1\n")
  cat("performance quantifier = fraction of correct classifications\n")
  cat("optimal covariate set = balanced criterion involving overfitting gap\n")
  cat("parametric model for regression = covariate noise only\n")
  cat("image format = EPS\n")
  cat("fixed random number seed = no\n")
  cat("\n")
  cat("[SESSION]\n")
  cat("project dir = <set this> **************\n")
  cat("\n")
  cat("[DATASET]\n")
  cat("filename = <set this> **************\n")
  cat("type =  <set this> **************\n")
  cat("output dir = auto\n")
  cat("extreme covariate value abort = Yes\n")
  cat("ordinal class auto range = Yes\n")
  cat("\n")
  cat("[BATCH REGRESSION]\n")
  cat("regression type = Batch Cox, 50/50 CV\n")
  cat("regulariser = Ridge (L2)\n")
  cat("primary risk = 1\n")
  cat("number of training sets = <set this> **************\n")
  cat("cross-validation = cross-validate by error counting\n")
  cat("cov pre-selection method = rank correlation with outcome\n")
  cat("cov pre-selection = no preselection (initiate batch analysis with all active covariates)\n")
  cat("ordinal class balancing = Yes\n")
  cat("covs. normalised = Yes\n")
  cat("treatment response variable index = 0\n")
  cat("\n")
  cat("[OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION]\n")
  cat("analysis dir = <set this> **************\n")
  cat("regression dir = <set this> **************\n")
  cat("primary risk = 1\n")
  cat("cutoff time auto = Yes\n")
  cat("compute_roc_and_auc = Yes\n")

  sink()
}

#' calculateRiskScore
#'
#' Hint, load from a weights file like this:
#' riskSignatureDataframe <- read.table("RiskScore_formula.txt",
#'                                  sep = '*', col.names = c("Weight", "Covariate"),
#'                                  stringsAsFactors = F, skip = 2, fill = T)
#'
#' @param riskSignature The data frame of the risk signature
#' @param data The data frame that contains the covariates
#' @return The vector of risk scores, one for each row of data
#' @export
#'
calculateRiskScore <- function(riskSignatureDataframe, data){



  library(stringr)

  weights <- riskSignatureDataframe
  weights$Weight <- str_remove(weights$Weight, "S=\\(")
  weights$Weight <- str_remove(weights$Weight, "\\+ \\(")
  weights$Weight <- str_remove(weights$Weight, "\\- \\(")
  weights$Weight <- str_remove(weights$Weight, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\(")
  weights$Covariate <- str_remove_all(weights$Covariate, "\\)")
  weights$Covariate <- str_remove_all(weights$Covariate, " ")
  weights$Weight <- as.numeric(weights$Weight)

  weights <- weights[c(2,1)]

  # List of covariates used
  Covariate <- weights$Covariate[1:(length(weights$Covariate)-1)]  # last row is the const

  # check presence of covariates in data
  for (j in 1:length(Covariate)){
    if(!(Covariate[j] %in% colnames(data)))
      stop(paste("ERROR: Covariate", Covariate[j], "missing from data."))
  }

  j=1
  # match col in data to jth name in Covariate vector
  # multiply that col by the jth weight
  s <- data[,Covariate[j]] * weights$Weight[j]

  for (j in 2:length(Covariate)){
    # add more terms on
    s <- s + data[,Covariate[j]] * weights$Weight[j]
  }

  # subtract constant
  s <- s - weights$Weight[length(weights$Covariate)]

  return(s)
}

#' calculateBenefitScore
#'
#' Hint, load from a weights file like this:
#' riskSignatureDataframe <- read.table("RiskScore_formula.txt",
#'                                  sep = '*', col.names = c("Weight", "Covariate"),
#'                                  stringsAsFactors = F, skip = 5, fill = T)
#'
#' @param riskSignature The data frame of the risk signature
#' @param data The data frame that contains the covariates
#' @return The vector of risk scores, one for each row of data
#' @export
#'
calculateBenefitScore <- function(benefitSignatureDataframe, data){

  weights <- benefitSignatureDataframe
  weights$Weight <- stringr::str_remove(weights$Weight, "\\+ \\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\- \\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\)")
  weights$Covariate <- stringr::str_remove_all(weights$Covariate, "\\(")
  weights$Covariate <- stringr::str_remove_all(weights$Covariate, "\\)")
  weights$Covariate <- stringr::str_remove_all(weights$Covariate, " ")
  weights$Covariate <- sub(":", ".", weights$Covariate)
  weights$Weight <- as.numeric(weights$Weight)

  weights <- weights[c(2,1)]

  # List of covariates used
  Covariate <- weights$Covariate

  # check presence of covariates in data
  for (j in 1:length(Covariate)){
    if(!(Covariate[j] %in% colnames(data)))
      stop(paste("ERROR: Covariate", Covariate[j], "missing from data."))
  }

  j=1
  # match col in data to jth name in Covariate vector
  # multiply that col by the jth weight
  s <- data[,Covariate[j]] * weights$Weight[j]

  for (j in 2:length(Covariate)){
    # add more terms on
    s <- s + data[,Covariate[j]] * weights$Weight[j]
  }

  return(s)
}

#' calculateTreatmentResponseScore
#'
#' Hint, load from a weights file like this:
#' riskSignatureDataframe <- read.table("RiskScore_formula.txt",
#'                                  sep = '*', col.names = c("Weight", "Covariate"),
#'                                  stringsAsFactors = F, skip = 2, fill = T)
#'
#' @param riskSignature The data frame of the risk signature
#' @param data The data frame that contains the covariates
#' @return The vector of risk scores, one for each row of data
#' @export
#'
calculateTreatmentResponseScore <- function(benefitSignatureDataframe, data){

  weights <- benefitSignatureDataframe

  weights$Weight <- stringr::str_remove(weights$Weight, "S= \\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\+ \\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\- \\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\(")
  weights$Weight <- stringr::str_remove(weights$Weight, "\\)")

  # Remove the multiplier that apppears in this type of file.
  weights$Covariate <- stringr::str_remove_all(weights$Covariate, "\\(.*\\)x")

  weights$Covariate <- stringr::str_remove_all(weights$Covariate, "\\(")
  weights$Covariate <- stringr::str_remove_all(weights$Covariate, "\\)")
  weights$Covariate <- stringr::str_remove_all(weights$Covariate, " ")
  weights$Covariate <- sub(":", ".", weights$Covariate)
  weights$Weight <- as.numeric(weights$Weight)

  weights <- weights[c(2,1)]

  # List of covariates used
  Covariate <- weights$Covariate

  # check presence of covariates in data
  for (j in 1:length(Covariate)){
    if(!(Covariate[j] %in% colnames(data)))
      stop(paste("ERROR: Covariate", Covariate[j], "missing from data."))
  }

  j=1
  # match col in data to jth name in Covariate vector
  # multiply that col by the jth weight
  s <- data[,Covariate[j]] * weights$Weight[j]

  for (j in 2:length(Covariate)){
    # add more terms on
    s <- s + data[,Covariate[j]] * weights$Weight[j]
  }

  return(s)
}
