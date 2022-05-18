
#' plotCoxElasticNetCoefs
#' Plot the coefficients as a function of Lambda,
#' @param cen The msaWrapperCoxElasticNet object from buildCoxElasticNet().
#' @export
#'
plotCoxElasticNetCoefs <- function(cen){
  plot(cen$glmnet_model, label = T)
}

#' plotCoxElasticNetPerformance
#' Plot the performance as a function of Lambda,
#' @param cen The msaWrapperCoxElasticNet object from buildCoxElasticNet().
#' @export
#'
plotCoxElasticNetPerformance <- function(cen){

  ggp <- ggplot(cen$performance, aes(x = log10(lambda))) +
    geom_line(aes(y = c_index), col = "blue") +
    geom_line(aes(y = c_index+sd), col = "blue", linetype = "dashed") +
    geom_line(aes(y = c_index-sd), col = "blue", linetype = "dashed") +
    geom_vline(xintercept = log10(cen$optimal_lambda), linetype="dashed") +
    geom_vline(xintercept = log10(cen$strict_lambda), linetype="dashed")

  print(ggp)
}


