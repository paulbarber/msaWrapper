# TODO
# Do I need to balance the classes? - leave to the caller
# See help for RandomForestSRC - loads of examples - balancing, brier score, compare to Cox...

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

  # data is a Data frame containing the y-outcome (2 cols at the end) and x-variables. No label column.
  data <- cbind(msa$data, msa$outcome)

  # formula is a symbolic description of the model to be fit. See randomForestSRC
  formula <- as.formula(outcome ~ .)
  names(data) <- c(names(msa$data), "outcome")

  # data will contain the outcome
  current_data <- data
  # x will have just the covariates
  p = dim(data)[2]
  outcome <- msa$outcome
  current_x <- msa$data

  train_performance <- vector()
  OOB_performance <- vector()
  valid_performance <- vector()
  train_perf_sd <- vector()
  OOB_perf_sd <- vector()
  valid_perf_sd <- vector()
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
    train_perf_i <- vector(length = iterations)
    OOB_perf_i <- vector(length = iterations)
    valid_perf_i <- vector(length = iterations)

    ProgressBar <- utils::txtProgressBar(min = 0, max = iterations,
                                         initial = 0, style = 3)

    for (i in 1:iterations){

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
                                   importance = "permute",    # TRUE uses "anti", better to use "permute"  https://explained.ai/rf-importance/
                                   proximity = TRUE)

      importance[,i] <- rf$importance[,1] # 1st col is "All", the other relate to other classes

      # raw training prediction
      pred <- rf$class
      train_perf_i[i] = (sum(pred == train_data$outcome) / length(train_data$outcome))

      # OOB prediction
      pred <- rf$class.oob
      perf = (sum(pred == train_data$outcome) / length(train_data$outcome))
      OOB_perf_i[i] = perf

      if(byOOB == FALSE) {
        # Validation prediction instead
        pred <- predict(rf, newdata = valid_x)
        perf = (sum(pred == valid_data$outcome) / length(valid_data$outcome))
        valid_perf_i[i] = perf
      }

      utils::setTxtProgressBar(ProgressBar, i)

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
    max_i <- which.max(OOB_performance)
  }else{
    # Choose max of validation performance
    max_i <- which.max(valid_performance)
  }

  optimal_p <- active_covars[max_i]
  optimal_names <- active_covar_names[[max_i]]
  optimal_scores <- active_covar_scores[[max_i]]

  #x <- data[, -c(p-1,p)]
  x <- msa$data
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
                                 importance = "permute",    # TRUE uses "anti", better to use "permute"  https://explained.ai/rf-importance/
                                 proximity = TRUE)

    # OOB prediction
    pred <- rf$class.oob
    perf = (sum(pred == optimal_data$outcome) / length(optimal_data$outcome))

    if(perf > best_perf){
      best_perf = perf
      best_rf <- rf
    }

  }

  performance <- data.frame(active_covars,
                            train_performance, train_perf_sd,
                            OOB_performance, OOB_perf_sd,
                            valid_performance, valid_perf_sd)

  # return this object
  structure(list(iterations, byOOB,
                 performance,
                 msa$colLabels,
                 active_covar_names,
                 active_covar_scores,
                 optimal_p, optimal_names, optimal_scores,
                 best_perf,
                 best_rf),
            .Names = c("iterations", "byOOB",
                       "performance",
                       "covar_names",
                       "active_covar_names", "active_covar_scores",
                       "optimal_p", "optimal_covars", "optimal_importance",
                       "optimal_perfomance",
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
  #stopifnot(msa$type == "time to event data")

  # data is a Data frame containing the y-outcome (2 cols at the end) and x-variables. No label column.
  data <- cbind(msa$data, msa$outcome)

  # formula is a symbolic description of the model to be fit. See randomForestSRC
  formula <- as.formula(Surv(time, event) ~ .)
  names(data) <- c(names(msa$data), names(msa$outcome))

  # data will contain the outcome
  current_data <- data
  # x will have just the covariates
  p = dim(data)[2]
  #outcome <- data[, c(p-1,p)]
  #current_x <- data[, -c(p-1,p)]
  outcome <- msa$outcome
  current_x <- msa$data

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
                  importance = "permute",    # TRUE uses "anti", better to use "permute"  https://explained.ai/rf-importance/
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

    # sum scores over all the iterations
    scores <- apply(score, 1, sum)

    # raw scores
    active_covar_scores[[n_repeat]] <- scores
    # normalised scores per run
    #active_covar_scores[[n_repeat]] <- scores / sum(abs(scores))

    # Get ordered list of the current names
    scores_names <- names(current_x)[order(scores)]    # score can be negative, but -ve is always bad

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
  optimal_scores <- active_covar_scores[[max_i]]

  #x <- data[, -c(p-1,p)]
  x <- msa$data
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
                importance = "permute",    # TRUE uses "anti", better to use "permute"  https://explained.ai/rf-importance/
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
                 msa$colLabels,
                 active_covar_names,
                 active_covar_scores,
                 optimal_p, optimal_names, optimal_scores,
                 best_perf,
                 best_rf),
            .Names = c("iterations", "byOOB",
                       "performance",
                       "covar_names",
                       "active_covar_names", "active_covar_scores",
                       "optimal_p", "optimal_covars", "optimal_importance",
                       "optimal_cindex",
                       "trained_random_forest"),
            class = "msaWrapperRandomSurvivalForest")

}


