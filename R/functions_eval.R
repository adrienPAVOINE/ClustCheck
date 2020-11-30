# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for variables evaluation
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Plot of t-values (numeric variables)
# ------------------------------------------------------------------------- #

#' plottvaluenum
#'
#' @param obj an object of class NumDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
plottvaluenum <- function(obj){
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
    facet_wrap(vars(ind)) +
    labs(title = "t-values")
}

# ------------------------------------------------------------------------- #
# Plot of size effect (numeric variables)
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
    facet_wrap(vars(ind)) +
    labs(title = "Size effect")
}

# ------------------------------------------------------------------------- #
# Plot of correlations (numeric variables)
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
  p <- min(12,length(table))
  corr <- as.data.frame(table[1:p])
  colnames(corr) <- "values"
  # Visualisation
  ggplot(corr, aes(x=reorder(rownames(corr), -values), y=values)) +
    geom_col() +
    labs(title = "Correlations") +
    xlab("Variables") +
    ylab("Value") 
}

# ------------------------------------------------------------------------- #
# Plot of t-values (numeric variables)
# ------------------------------------------------------------------------- #

#' plottvaluefactor
#'
#' @param obj an object of class FactorDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
# a voir comment merger les 2 fonctions plot t values
plottvaluefactor <- function(obj, var){
  table <- TValueTable.FactorDataset(obj, var)
  m <- ncol(table)
  levels <- colnames(table)
  Tvalue_table <- as.data.frame(table)
  Tvalue_table$Freq
  colnames(Tvalue_table) <- c('clusters','levels','values')
  # Visualisation
  ggplot2::ggplot(Tvalue_table, aes(x=levels, y=values)) +
    geom_col() +
    coord_flip() +
    facet_wrap(vars(clusters)) +
    labs(title = "t-values")
}

# ------------------------------------------------------------------------- #
# Plot of Cramer's V (categorical variables)
# ------------------------------------------------------------------------- #

#' plotVCramer
#'
#' @param obj an object of class FactorDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
plotVCramer <- function(obj){
  table <- Vcramer.FactorDataset(obj)
  p <- min(12,length(table))
  print(p)
  VCramer <- as.data.frame(table[1:p,])
  colnames(VCramer) <- "values"
  # Visualisation
  if (p>12){
    title2=cat("Cramer's V values - Top 12 out of",p," variables")
  }
  else
    title2="Cramer's V values"
  ggplot(VCramer, aes(x=reorder(rownames(VCramer), -values), y=values)) + geom_col() +
    labs(title = "Cramer's V values") +
    xlab("Variables") +
    ylab("Value")  
} 


# ------------------------------------------------------------------------- #
# Plot of Phi values (categorical variables) (à terminer)
# ------------------------------------------------------------------------- #

#' plotphi
#'
#' @param obj an object of class FactorDataset
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
#library(ggplot2)
plotphi <- function(obj, var){
  table <- PhiValueTable.FactorDataset(obj, var)
  m <- ncol(table)
  levels <- colnames(table)
  phivalue_table <- as.data.frame(table)
  phivalue_table$Freq
  colnames(phivalue_table) <- c('clusters','levels','values')
  # Visualisation
  ggplot2::ggplot(phivalue_table, aes(x=levels, y=values)) +
    geom_col() +
    coord_flip() +
    facet_wrap(vars(clusters)) +
    labs(title = "phi-values") 
}  






