
#' buildElasticNet
# Could use ElasticNet model like Saddle Point
# Or https://cran.r-project.org/web/packages/glmnetcr/vignettes/glmnetcr.pdf

#' buildElasticNet.msaWrapperOclass
#'
#' Build a Binomial Elastic Net regularized model for ordinal class outcome
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of cv.glmnet calls to make.
#' @return An object containing the trained model and it's performance.
#' @export
#'
buildElasticNet <- function(msa, iterations=200, alpha = 1){
  stopifnot(msa$type == "ordinal class data")
  # Must impute and NA values, use the mean of other values
  NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

  x <- msa$data
  y <- msa$outcome

  # impute
  x[] <- lapply(x, NA2mean)

  # Scale zero mean and unit sd
  x <- scale(x)


  # convert to how glmnet Cox likes it
  x <- as.matrix(x)


  lambdas = NULL
  lambdas1se= NULL

  ProgressBar <- utils::txtProgressBar(min = 0, max = iterations,
                                       initial = 0, style = 3)

  # Use glmnet's built-in cross validation (nfolds = 3) cv.glmnet()
  # Returns coefficients and performance over a range of elastic net lambda.
  # Use performance over many iterations to find optimal lambdas.
  # Then extract coefficients etc at these lambdas using glmnet() later
  #type.measure = "auc" is used.

  for (i in 1:iterations)
  {
    fit <- glmnet::cv.glmnet(x, y, family = "binomial", type.measure = "auc",
                             nfolds = 3, alpha = alpha)
    lambdas <- rbind(fit$lambda.min,fit$cvm[fit$index[1]])
    lambdas1se <- rbind(fit$lambda.1se,fit$cvm[fit$index[2]])
    utils::setTxtProgressBar(ProgressBar, i)
  }

  close(ProgressBar)

  best_lambda = mean(lambdas[1])
  best_perf = mean(lambdas[2])

  best1se_lambda = mean(lambdas1se[1])
  best1se_perf = mean(lambdas1se[2])

  # and now run glmnet once more
  glmnet_fit <- glmnet::glmnet(x, y, family = "binomial", alpha = alpha, trace.it=1)

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

  performance <- data.frame(lambdas[1], lambdas[2])
  names(performance) <- c("lambda", "cvm")

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
                       "optimal_p", "optimal_covars", "optimal_coefs", "optimal_lambda", "optimal_cvm",
                       "strict_p", "strict_covars", "strict_coefs", "strict_lambda", "strict_cvm",
                       "glmnet_model", "best_Predicted_risk", "strict_Predicted_risk"),
            class = "msaWrapperElasticNet")

}
