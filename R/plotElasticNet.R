
#' plotElasticNetCoefs
#' Plot the coefficients as a function of Lambda/Norm/Deviance,
#' @param cen The msaWrapperElasticNet object from buildElasticNet().
#' @param xvar lambda/norm/dev to decide X axis
#' @export
#'
plotElasticNetCoefs <- function(cen, xvar="lambda"){
  plot(cen$glmnet_model,xvar=xvar, label = T)
}

#' plotElasticNetPerformance
#' Plot the performance as a function of Lambda,
#' @param cen The msaWrapperElasticNet object from buildElasticNet().
#' @export
#'
plotElasticNetPerformance <- function(cen){

  ggp <- ggplot(cen$performance, aes(x = log10(lambda))) +
    geom_line(aes(y = cvm), col = "blue") +
    geom_line(aes(y = cvm+sd), col = "red", linetype = "dashed") +
    geom_line(aes(y = cvm-sd), col = "green", linetype = "dashed") +
    geom_vline(xintercept = log10(cen$optimal_lambda), linetype="dashed") +
    geom_vline(xintercept = log10(cen$strict_lambda), linetype="dotdash")

  print(ggp)
}

