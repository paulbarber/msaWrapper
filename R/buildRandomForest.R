# TODO
# Do I need to balance the classes? - leave to the caller

#' buildRandomForest
#'
#' Generic function.
#' @param msa The msaWrapper object to work with.
#' @export
#'
buildRandomForest <- function(msa, iterations, byOOB) UseMethod("buildRandomForest")

#' buildRandomForest.msaWrapperOclass
#'
#' Build a Random Forest Classifier for ordinal class outcome
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of randomForest calls to make for the active covars.
#' @param byOOB Use out of bag performance measure, if false use 50:50 cross validation
#' @return An object containing the trained RF and it's performance.
#' @export
#'
buildRandomForest.msaWrapperOclass <- function(msa, iterations=200, byOOB = TRUE){
  stopifnot(msa$type == "ordinal class data")

  # x is data to predict from (no outcomes, no labels)
  x <- msa$data
  # y is the one outcome column
  y <- msa$outcome

  current_x <- x

  train_performance <- vector()
  OOB_performance <- vector()
  valid_performance <- vector()
  train_perf_sd <- vector()
  OOB_perf_sd <- vector()
  valid_perf_sd <- vector()
  active_covars <- vector()

  p = dim(x)[2]
  active_covar_names <- list()
  active_covar_scores <- list()
  n_repeat = 1

  # repeat until 1 covar left
  repeat {

    active_covar_names[[n_repeat]] <- names(current_x)
    current_p = dim(current_x)[2]
    print(paste0(current_p, "/", p, " covariates"))
    #print(names(current_x))

    # vector/matrix to keep scores
    imp_MDAcc <- matrix(nrow = current_p, ncol = iterations)
    imp_MDGini <- matrix(nrow = current_p, ncol = iterations)
    train_perf_i <- vector(length = iterations)
    OOB_perf_i <- vector(length = iterations)
    valid_perf_i <- vector(length = iterations)

    ProgressBar <- utils::txtProgressBar(min = 1, max = iterations,
                                  initial = 1, style = 3)

    for (i in 1:iterations){

      utils::setTxtProgressBar(ProgressBar, i)

      if(byOOB == FALSE){
        # split data randomly for cross validation
        n = dim(current_x)[1]
        ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
        train_x <- current_x[ind,]
        train_y <- y[ind]
        valid_x <- current_x[!ind,]
        valid_y <- y[!ind]
      } else {
        # Not doing cross validation
        train_x <- current_x
        train_y <- y
        valid_x <- NA
        valid_y <- NA
      }

      # train RF
      rf <- randomForest::randomForest(train_x, train_y, importance=TRUE, proximity=TRUE)

      imp <- randomForest::importance(rf)
      imp_MDAcc[,i] <- imp[,"MeanDecreaseAccuracy"]
      imp_MDGini[,i] <- imp[,"MeanDecreaseGini"]

      # raw training prediction
      pred <- predict(rf, newdata = train_x)
      train_perf_i[i] = (sum(pred == train_y) / length(train_y))

      # OOB prediction
      pred <- rf$predicted
      #pred2 <- predict(rf) # same as above
      perf = (sum(pred == train_y) / length(train_y))
      OOB_perf_i[i] = perf

      if(byOOB == FALSE) {
        # Validation prediction instead
        pred <- predict(rf, newdata = valid_x)
        perf = (sum(pred == valid_y) / length(valid_y))
        valid_perf_i[i] = perf
      }
    }

    close(ProgressBar)

    # get stats
    train_performance <- c(train_performance, mean(train_perf_i))
    OOB_performance <- c(OOB_performance, mean(OOB_perf_i))
    valid_performance <- c(valid_performance, mean(valid_perf_i))
    train_perf_sd <- c(train_perf_sd, sd(train_perf_i))
    OOB_perf_sd <- c(OOB_perf_sd, sd(OOB_perf_i))
    valid_perf_sd <- c(valid_perf_sd, sd(valid_perf_i))
    active_covars <- c(active_covars, current_p)


    # Choose a covar to remove
    score <- imp_MDGini
    #score <- imp_MDAcc

    scores <- apply(score, 1, sum)
    active_covar_scores[[n_repeat]] <- scores

    scores_names <- names(current_x)[order(scores)]

    # reduce the covariates by one!!!!!!!!!!!!!
    current_x <- current_x[,scores_names[2:current_p]]
    n_repeat <- n_repeat+1

    # if x has been reduced to a vector, we are on the last covariate
    if(is.vector(current_x)){
      #print(scores_names[2])  # the one left at the end
      active_covar_names[[n_repeat]] <- scores_names[2]
      active_covar_scores[[n_repeat]] <- NA
      break
    }
  }

  # choose optimal model
  if(byOOB == TRUE){
    # Choose max of OOB performance
    max_i <- which.max(OOB_performance)
  }else{
    # Choose max of validation performance
    max_i <- which.max(valid_performance)
  }

  optimal_p <- active_covars[max_i]
  optimal_names <- active_covar_names[[max_i]]

  optimal_x <- subset(x, select = optimal_names)

  print(paste("Optimal p=", optimal_p))

  # Do iterations to find a good rf with optimal p
  best_perf = 0
  best_rf = NULL
  for (i in 1:iterations){

    setTxtProgressBar(ProgressBar, i)

    # train RF
    rf <- randomForest::randomForest(optimal_x, y, importance=TRUE, proximity=TRUE)

    # OOB prediction
    pred <- rf$predicted
    #pred2 <- predict(rf) # same as above
    perf = (sum(pred == y) / length(y))

    if(perf > best_perf){
      best_perf = perf
      best_rf <- rf
    }

  }

  performance <- data.frame(active_covars,
                            train_performance, train_perf_sd,
                            OOB_performance, OOB_perf_sd,
                            valid_performance, valid_perf_sd)

  # Return this object
  structure(list(iterations, byOOB,
                 performance,
                 active_covar_names,
                 active_covar_scores,
                 optimal_p, optimal_names,
                 best_perf,
                 best_rf),
            .Names = c("iterations", "byOOB",
                       "performance",
                       "active_covar_names", "active_covar_scores",
                       "optimal_p", "Optimal_covars",
                       "best_perf",
                       "trained_random_forest"),
            class = "msaWrapperRFClassifier")
}

#' buildRandomForest.msaWrapperTte
#'
#' Build a Random Forest Classifier for time to event outcome
#' @param msa The msaWrapper object to work with.
#' @param iterations The number of randomForest calls to make for the active covars.
#' @param byOOB Use out of bag performance measure, if false use 50:50 cross validation
#' @return An object containing the trained RF and it's performance.
#' @export
#'
buildRandomForest.msaWrapperTte <- function(msa, iterations=200, byOOB = TRUE){
  stopifnot(msa$type == "time to event data")

  # formula ia a symbolic description of the model to be fit. See randomForestSRC
  formula <- Surv(time, event) ~ .

  # data is a Data frame containing the y-outcome (2 cols at the end) and x-variables. No label column.
  data <- cbind(msa$data, msa$outcome)
  names(data) <- c(names(msa$data), names(msa$outcome))

  # data will contain the outcome
  current_data <- data
  # x will have just the covariates
  p = dim(data)[2]
  outcome <- data[, c(p-1,p)]
  current_x <- data[, -c(p-1,p)]

  train_c_index <- vector()
  OOB_c_index <- vector()
  valid_c_index <- vector()
  train_ci_sd <- vector()
  OOB_ci_sd <- vector()
  valid_ci_sd <- vector()
  active_covars <- vector()

  p = dim(current_x)[2]
  active_covar_names <- list()
  active_covar_scores <- list()
  n_repeat = 1

  # repeat until 1 covar left
  repeat {

    active_covar_names[[n_repeat]] <- names(current_x)
    current_p = dim(current_x)[2]
    print(paste0(current_p, "/", p, " covariates"))
    #print(names(current_x))

    # vector/matrix to keep scores
    importance <- matrix(nrow = current_p, ncol = iterations)
    train_ci_i <- vector(length = iterations)
    OOB_ci_i <- vector(length = iterations)
    valid_ci_i <- vector(length = iterations)

    ProgressBar <- utils::txtProgressBar(min = 1, max = iterations,
                                  initial = 1, style = 3)

    for (i in 1:iterations){

      utils::setTxtProgressBar(ProgressBar, i)

      if(byOOB == FALSE){
        # split data randomly for cross validation
        n = dim(current_data)[1]
        ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
        train_x <- current_data[ind,]
        train_data <- current_data[ind,]
        valid_x <- current_x[!ind,]
        valid_data <- current_data[!ind,]
      } else {
        # Not doing cross validation
        train_x <- current_x
        train_data <- current_data
        valid_x <- NA
        valid_data <- NA
      }

      # train RF
      rf <- randomForestSRC::rfsrc(formula,
                  data = train_data,
                  na.action = "na.impute",
                  importance = TRUE,
                  proximity = TRUE)

      importance[,i] <- rf$importance

      # The "predicted" values are mortality = risk score
      # use c.index as a performance score

      # raw training prediction
      train_ci_i[i] = randomForestSRC::get.cindex(rf$yvar[,1], rf$yvar[,2], -rf$predicted)

      # OOB prediction
      OOB_ci_i[i] = randomForestSRC::get.cindex(rf$yvar[,1], rf$yvar[,2], -rf$predicted.oob)

      if(byOOB == FALSE) {
        # Validation prediction instead
        pred <- predict(rf, newdata = valid_data, na.action = "na.impute", outcome = "test")
        valid_ci_i[i] = randomForestSRC::get.cindex(pred$yvar[,1], pred$yvar[,2], -pred$predicted.oob)
      }
    }

    close(ProgressBar)

    # get stats
    train_c_index <- c(train_c_index, mean(train_ci_i))
    OOB_c_index <- c(OOB_c_index, mean(OOB_ci_i))
    valid_c_index <- c(valid_c_index, mean(valid_ci_i))
    train_ci_sd <- c(train_ci_sd, sd(train_ci_i))
    OOB_ci_sd <- c(OOB_ci_sd, sd(OOB_ci_i))
    valid_ci_sd <- c(valid_ci_sd, sd(valid_ci_i))
    active_covars <- c(active_covars, current_p)


    # Choose a covar to remove
    score <- importance

    scores <- apply(score, 1, sum)
    active_covar_scores[[n_repeat]] <- scores

    scores_names <- names(current_x)[order(scores)]

    # reduce the covariates by one!!!!!!!!!!!!!
    current_x <- current_x[,scores_names[2:current_p]]
    current_data <- cbind(current_x, outcome)
    n_repeat <- n_repeat+1

    # if x has been reduced to a vector, we are on the last covariate
    if(is.vector(current_x)){
      #print(scores_names[2])  # the one left at the end
      active_covar_names[[n_repeat]] <- scores_names[2]
      active_covar_scores[[n_repeat]] <- NA
      break
    }
  }

  # choose optimal model
  if(byOOB == TRUE){
    # Choose max of OOB performance
    max_i <- which.max(OOB_c_index)
  }else{
    # Choose max of validation performance
    max_i <- which.max(valid_c_index)
  }

  optimal_p <- active_covars[max_i]
  optimal_names <- active_covar_names[[max_i]]

  x <- data[, -c(p-1,p)]
  optimal_x <- subset(x, select = optimal_names)
  optimal_data <- cbind(optimal_x, outcome)

  print(paste("Optimal p=", optimal_p))

  # Do iterations to find a good rf with optimal p
  best_perf = 0
  best_rf = NULL
  for (i in 1:iterations){

    setTxtProgressBar(ProgressBar, i)

    # train RF
    rf <- randomForestSRC::rfsrc(formula,
                data = optimal_data,
                na.action = "na.impute",
                importance = TRUE,
                proximity = TRUE)

    # OOB prediction
    perf = randomForestSRC::get.cindex(rf$yvar[,1], rf$yvar[,2], -rf$predicted.oob)

    if(perf > best_perf){
      best_perf = perf
      best_rf <- rf
    }

  }

  performance <- data.frame(active_covars,
                            train_c_index, train_ci_sd,
                            OOB_c_index, OOB_ci_sd,
                            valid_c_index, valid_ci_sd)

    # return this object
  structure(list(iterations, byOOB,
                 performance,
                 active_covar_names,
                 active_covar_scores,
                 optimal_p, optimal_names,
                 best_perf,
                 best_rf),
            .Names = c("iterations", "byOOB",
                       "performance",
                       "active_covar_names", "active_covar_scores",
                       "optimal_p", "Optimal_covars",
                       "best_c_index",
                       "trained_random_forest"),
            class = "msaWrapperRandomSurvivalForest")

}
