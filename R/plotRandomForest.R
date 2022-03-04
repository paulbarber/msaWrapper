
#' plotRandomForestPerformance
#' Generic fn.
#' Plot the training performance of a random forest model
#' @param rf The RF object from buildRandomForest().
#' @export
#'
plotRandomForestPerformance <- function(rf) UseMethod("plotRandomForestPerformance")

#' plotRandomForestPerformance.msaWrapperRandomSurvivalForest
#'
#' Plot the training performance of a random forest model.
#' @param rf The RF object from buildRandomForest().
#' @export
#'
plotRandomForestPerformance.msaWrapperRandomSurvivalForest <- function(rf){

  ggp <- ggplot(rf$performance, aes(x = active_covars)) +
    geom_line(aes(y = train_c_index), col = "red") +
    geom_line(aes(y = OOB_c_index), col = "blue") +
    geom_line(aes(y = train_c_index+train_ci_sd), col = "red", linetype = "dashed") +
    geom_line(aes(y = train_c_index-train_ci_sd), col = "red", linetype = "dashed") +
    geom_line(aes(y = OOB_c_index+OOB_ci_sd), col = "blue", linetype = "dashed") +
    geom_line(aes(y = OOB_c_index-OOB_ci_sd), col = "blue", linetype = "dashed")

  print(ggp)
}

#' plotRandomForestPerformance.msaWrapperRFClassifier
#'
#' Plot the training performance of a random forest model.
#' @param rf The RF object from buildRandomForest().
#' @export
#'
plotRandomForestPerformance.msaWrapperRFClassifier <- function(rf){

  ggp <- ggplot(rf$performance, aes(x = active_covars)) +
    geom_line(aes(y = train_performance), col = "red") +
    geom_line(aes(y = OOB_performance), col = "blue") +
    geom_line(aes(y = train_performance+train_perf_sd), col = "red", linetype = "dashed") +
    geom_line(aes(y = train_performance-train_perf_sd), col = "red", linetype = "dashed") +
    geom_line(aes(y = OOB_performance+OOB_perf_sd), col = "blue", linetype = "dashed") +
    geom_line(aes(y = OOB_performance-OOB_perf_sd), col = "blue", linetype = "dashed")

  print(ggp)
}


#' plotRandomForestImportanceSwimmers
#'
#' Plot of importance scores through the optimisation of RF batch regression.#'
#'
#' @param RSF Object returned by buildRandomForest.msaWrapperTte
#' @param show_best_performance_line TRUE/FALSE to show or not this dotted line at the optimal number of covariates.
#' @param title An optional title for the plot.
#' @export
#'
plotRandomForestImportanceSwimmers <- function(RSF, show_best_performance_line = TRUE, title = "", text_size = NA) {

  # Get covariate names
  covar_names <- RSF$covar_names

  # Find the optimal points
  optimum_performance <- RSF$optimal_p

  # Form a data frame with cols: Covar_name Score Number_Active
  data_long <- data.frame(Covariate = character(), Score = numeric(), Active = numeric()) # set this up here, but it gets overwritten!
  # go through list, except last one which is a special case
  for(p in 1:(length(RSF$active_covar_names)-1)){

    c <- RSF$active_covar_names[[p]]
    s <- as.numeric(RSF$active_covar_scores[[p]])

    for(q in 1:length(c)){
      data_long <- rbind(data_long, list(c[q], s[q], length(c)))
    }
  }
  names(data_long) <- c("Covariate", "Score", "Active")
  largest.Score <- max(data_long$Score)
  smallest.Score <- min(data_long$Score)
  # last one, just the last covariate
  data_long <- rbind(data_long, list(RSF$active_covar_names[[length(RSF$active_covar_names)]][1], largest.Score, 1))

  if(is.na(text_size)) text_size <- 256 / length(covar_names)   # by trial and error, this is a good height, if names not too long.

  # reorder factors to maintain original covar order
  data_long$Covariate <- factor(data_long$Covariate, levels = covar_names)

  ggp <- ggplot(data_long, aes(y = Covariate, x = Active, fill = Score)) +
    theme_light() +
    theme(axis.text.y = element_text(size = text_size)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("light blue", "red"),
                         limits = c(smallest.Score, largest.Score),
                         na.value = "transparent") +
    geom_vline(xintercept = optimum_performance, linetype="dashed") +
    ggtitle(title) +
    scale_y_discrete(limits = rev)

  if(show_best_performance_line){
    ggp <- ggp + geom_vline(xintercept = optimum_performance, linetype="dashed")
  }

  print(ggp)

}
