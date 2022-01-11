#' plotDataCorrelations
#'
#' This function plots a correlation matrix of all covars against all others.
#' @param msa The msaWrapper object to work with.
#' @param method Correlation method to use.
#' @param textSize Axes labels text size.
#' @param groups To restrict covars to those in certain groups.
#' @export
plotDataCorrelations <- function(msa,
                                 method = c("kendall", "pearson", "spearman"),
                                 textSize = 5, groups = NA){

  method <- match.arg(method)

  if(!is.na(groups)){
    data <- msa$data[, msa$group %in% groups]
  } else {
    data <- msa$data
  }

  cormat <- cor(data, method = method, use = "pairwise.complete.obs")
  melted_cormat <- reshape2::melt(cormat)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile()+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = textSize, hjust = 1),
          axis.title.x = element_blank())+
    theme(axis.text.y = element_text(angle = 0,
                                     vjust = 1, size = textSize, hjust = 1),
          axis.title.y = element_blank())+
    ggtitle(paste(method, "correlations"))+
    coord_fixed()

}

#' strongDataCorrelations
#'
#' Find strong correlations in the covars
#' @param msa The msaWrapper object to work with.
#' @param threshold Threshold for a correlation to be considered strong.
#' @param method Correlation method to use.
#' @param groups To restrict covars to those in certain groups.
#' @return data frame of strong correlations.
#' @export
strongDataCorrelations <- function(msa, threshold = 0.95,
                                   method = c("kendall", "pearson", "spearman"),
                                   groups = NA){
  method <- match.arg(method)

  if(!is.na(groups)){
    data <- msa$data[, msa$group %in% groups]
  } else {
    data <- msa$data
  }

  cormat <- cor(data, method = method, use = "pairwise.complete.obs")
  melted_cormat <- reshape2::melt(cormat)

  s <- subset(melted_cormat, as.numeric(Var1) > as.numeric(Var2))
  s <- subset(s, abs(value)>threshold)
}

#' plotStrongCorrelations
#'
#' Plot strong correlations in the covars in data frame s.
#' @param msa The msaWrapper object to work with.
#' @param s A data frame with at least 2 cols that have pairs of covar names to plot. like that produced by strongDataCorrelations()
#' @param trans Any of scale_continuous, incl. "log10", "log2" etc.
#'
#' @export
plotStrongCorrelations <- function(msa, s,
                                   method = c("kendall", "pearson", "spearman"),
                                   trans = "identity"){
  method <- match.arg(method)
  data <- msa$data

  for (i in 1:dim(s)[1]){
    X <- as.character(s[i, 1])
    Y <- as.character(s[i, 2])
    print(ggplot(data = data, aes_string(x = X, y = Y)) +
            geom_point() +
            geom_smooth(method = "glm") +
            scale_x_continuous(trans=trans) +
            scale_y_continuous(trans=trans)
    )
    print(cor.test(x = data[,X], y = data[,Y],
                   method = method, use = "pairwise.complete.obs"))
  }
}

#' plotDataCrossCorrelations
#'
#' This function plots a correlation matrix of 2 sets of covars from given groups.
#' @param msa The msaWrapper object to work with.
#' @param method Correlation method to use.
#' @param textSize Axes labels text size.
#' @param group1 Vector of groups that define the first set of covars.
#' @param group2 Vector of groups that define the second set of covars.
#' @export

plotDataCrossCorrelations <- function(msa,
                                      method = c("kendall", "pearson", "spearman"),
                                      textSize = 1, group1, group2){

  method <- match.arg(method)

  X <- msa$data[, msa$group %in% group1]
  Y <- msa$data[, msa$group %in% group2]

  cormat <- cor(X, Y, method = method, use = "pairwise.complete.obs")
  melted_cormat <- reshape2::melt(cormat)
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile()+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = textSize, hjust = 1),
          axis.title.x = element_blank())+
    theme(axis.text.y = element_text(angle = 0,
                                     vjust = 1, size = textSize, hjust = 1),
          axis.title.y = element_blank())+
    ggtitle(paste(method, "correlations"))+
    coord_fixed()

}
