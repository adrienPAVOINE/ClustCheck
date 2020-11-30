# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for variables evaluation
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Plot of t-values
# ------------------------------------------------------------------------- #

#' plottvalue
#'
#' @param obj an object of class NumDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
plottvalue <- function(obj){
  table <- TValueTable.NumDataset(obj)
  p <- ncol(table)
  variables <- rownames(table)
  Tvalue_table <- stack(as.data.frame(table))
  Tvalue_table$variables <- rep(variables,p)
  # Visualisation
  ggplot2::ggplot(Tvalue_table, aes(x=variables, y=values)) +
    geom_col() +
    geom_hline(yintercept = -2, linetype="dashed", size=0.5, color="red") +
    geom_hline(yintercept = 2, linetype="dashed", size=0.5, color="red") +
    coord_flip() +
    facet_wrap(vars(ind))
}

# ------------------------------------------------------------------------- #
# Plot of size effect
# ------------------------------------------------------------------------- #

#' plotsizeeff
#'
#' @param obj an object of class NumDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
plotsizeeff <- function(obj){
  table <- EffectSizeTable.NumDataset(obj)
  p <- ncol(table)
  variables <- rownames(table)
  SE_table <- stack(as.data.frame(table))
  SE_table$variables <- rep(variables,p)
  # Visualisation
  ggplot2::ggplot(SE_table, aes(x=variables, y=values)) +
    geom_col() +
    coord_flip() +
    facet_wrap(vars(ind))
}

# ------------------------------------------------------------------------- #
# Plot of correlations
# ------------------------------------------------------------------------- #

#' plotcorr
#'
#' @param obj an object of class NumDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
plotcorr <- function(obj){
  table <- sort(Corr_ratios.NumDataset(obj), decreasing=T)
  p <- min(12,ncol(table))
  print(p)
  corr <- as.data.frame(table[1:p])
  colnames(corr) <- "values"
  # Visualisation
  ggplot(corr, aes(x=reorder(rownames(corr), -values), y=values)) + geom_col()
}

