#' buildSPSignature
#'
#' Generic function.
#' @param msa The msaWrapper object to work with.
#' @export
#'
buildSPSignature <- function(msa, iterations) UseMethod("buildSPSignature")

#' buildSPSignature.msaWrapperOclass
#'
#' Build a Saddle Point Signature for ordinal class outcome.
#' This requires the installation and valid license for that software.
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cross validation iterations to make.
#' @return An object containing the signature and it's performance.
#' @export
#'
buildSPSignature.msaWrapperOclass <- function(msa, iterations=200){


}

#' buildSPSignature.msaWrapperTte
#'
#' Build a Saddle Point Signature for time to event outcome.
#' This requires the installation and valid license for that software,
#' and a template ini file of settings.
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cross validation iterations to make.
#' @return An object containing the signature and it's performance.
#' @export
#'
buildSPSignature.msaWrapperTte <- function(msa, iterations=200){

  filename <- "msaWrapperData"
  templateIniFilename <- "msaWrapper_template.ini"
  iniFilename <- "msaWrapper.ini"

  ini.file <- read.ini(templateIniFilename)
  ini.file$SESSION$`project dir` <-  getwd()

  data <- cbind(msa$data, msa$outcome)
  names(data) <- c(names(msa$data), names(msa$outcome))
  export_SPS_file(data, filename, type = 2, R = 2)

  # make SPS ini files
  ini.file$DATASET$filename <- filename
  ini.file$`OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION`$`analysis dir` <- filename
  ini.file$`OUTCOME PREDICTION: MULTI-RISK SCORE APPLICATION`$`regression dir` <- "Reg_Normalised_UNKNOWN"
  write.ini(ini.file, iniFilename)

  # replace "=" by " = "
  tx  <- readLines(iniFilename)
  tx  <- gsub("=", " = ", x = tx)
  writeLines(tx, con = iniFilename)

  # run SPS
  system(paste("SaddlePoint-Signature.exe", iniFilename))

  # extract results


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
