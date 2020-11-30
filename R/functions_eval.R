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
plottvalue <- function(obj){ # devra appeler un objet de la classe plutot - sera à modifier
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


