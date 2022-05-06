
#' buildCoxElasticNet
#'
#' Generic function.
#' @param msa The msaWrapper object to work with.
#' @export
#'
buildCoxElasticNet <- function(msa, iterations) UseMethod("buildCoxElasticNet")


# TODO: THIS
# Could use Cox model like Saddle Point
# Or https://cran.r-project.org/web/packages/glmnetcr/vignettes/glmnetcr.pdf

#' buildCoxElasticNet.msaWrapperOclass
#'
#' Build a Cox Elastic Net regularized model for ordinal class outcome
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cv.glmnet calls to make.
#' @return An object containing the trained model and it's performance.
#' @export
#'
buildRandomForest.msaWrapperOclass <- function(msa, iterations=200){
  stopifnot(msa$type == "ordinal class data")
  stop("Cox Elastic Net not implemented for ordinal class data")
}




#' buildCoxElasticNet.msaWrapperTte
#'
#' Build a Cox Elastic Net regularized model for ordinal class outcome.
#' Uses glmnet::cv.glmnet() for optimisation.
#' Uses 3-fold cross validation (the minimum glmnet allows).
#' Data are normalised to zero mean and unit sd.
#' Missing data are imputed with the mean.
#' Returns optimal values from the peak of cross validation performance,
#' and a 'strict' values from the most regularized model with
#' performance within 1 standard deviation of the peak value.
#'
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cv.glmnet calls to make.
#' @return An object containing the trained model and it's performance.
#' @export
#'
buildCoxElasticNet.msaWrapperTte <- function(msa, iterations=200){
  stopifnot(msa$type == "time to event data")

  # Must impute and NA values, use the mean of other values
  NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

  x <- msa$data
  y <- msa$outcome

  # impute
  x[] <- lapply(x, NA2mean)

  # Scale zero mean and unit sd
  x <- scale(x)

  # Remove any NA outcomes
  x <- x[!is.na(y[1]),]
  y <- y[!is.na(y[1]),]

  # convert to how glmnet Cox likes it
  x <- as.matrix(x)
  y <- survival::Surv(time = y$time, event = y$event)

  lambdas = NULL

  ProgressBar <- utils::txtProgressBar(min = 0, max = iterations,
                                       initial = 0, style = 3)

  # Use glmnet's built-in cross validation (nfolds = 3) cv.glmnet()
  # Returns coefficients and performance over a range of elastic net lambda.
  # Use performance over many iterations to find optimal lambdas.
  # Then extract coefficients etc at these lambdas using glmnet() later

  for (i in 1:iterations)
  {
    fit <- glmnet::cv.glmnet(x, y, family = "cox", type.measure = "C", nfolds = 3)
    perf = data.frame(fit$lambda, fit$cvm)
    lambdas <- rbind(lambdas, perf)
    utils::setTxtProgressBar(ProgressBar, i)
  }

  close(ProgressBar)

  # take mean cvm for each lambda
  lambda_sd <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), sd)
  lambdas <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), mean)

  # select the best one, for Cox family perf = c-index, choose the max value
  best_index = which(lambdas[2] == max(lambdas[2]))
  best_lambda = lambdas[best_index, 1]

  # and the the most regularized model with perf within 1 standard deviation of the max
  best_perf = lambdas[best_index, 2]
  best_sd = lambda_sd[best_index, 2]

  best1se_index <- max(which(lambdas[2] > (best_perf - best_sd)))
  best1se_lambda = lambdas[best1se_index, 1]
  best1se_perf = lambdas[best1se_index, 2]

  # and now run glmnet once more
  glmnet_fit <- glmnet::glmnet(x, y, family = "cox")

  coef_matrix <- coef(fit, s = best_lambda)
  optimal_p <- coef_matrix@p[2]
  optimal_scores <- coef_matrix@x
  optimal_names <- coef_matrix@Dimnames[[1]][coef_matrix@i+1]

  coef_matrix <- coef(fit, s = best1se_lambda)
  strict_p <- coef_matrix@p[2]
  strict_scores <- coef_matrix@x
  strict_names <- coef_matrix@Dimnames[[1]][coef_matrix@i+1]

  best_Predicted_risk <- predict(glmnet_fit, x, s = best_lambda)[,1]
  strict_Predicted_risk <- predict(glmnet_fit, x, s = best1se_lambda)[,1]

  performance <- data.frame(lambdas[1], lambdas[2], lambda_sd[2])
  names(performance) <- c("lambda", "c_index", "sd")

    # return this object
  structure(list(iterations, x, y,
                 performance,
                 msa$colLabels,
                 optimal_p, optimal_names, optimal_scores, best_lambda, best_perf,
                 strict_p, strict_names, strict_scores, best1se_lambda, best1se_perf,
                 glmnet_fit, best_Predicted_risk, strict_Predicted_risk),
            .Names = c("iterations", "x", "y",
                       "performance",
                       "covar_names",
                       "optimal_p", "optimal_covars", "optimal_coefs", "optimal_lambda", "optimal_cindex",
                       "strict_p", "strict_covars", "strict_coefs", "strict_lambda", "strict_cindex",
                       "glmnet_model", "best_Predicted_risk", "strict_Predicted_risk"),
            class = "msaWrapperCoxElasticNet")

}
