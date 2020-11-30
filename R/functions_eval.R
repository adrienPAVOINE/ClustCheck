# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for variables evaluation
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Plot of t-values
# ------------------------------------------------------------------------- #

#' plotvalue
#'
#' @param Tvalue_table a Table of test value
#'
#' @return
#' @export
#' @import ggplot2
#' @examples

plotvalue <- function(Tvalue_table){ # devra appeler un objet de la classe plutot - sera à modifier
  p <- ncol(Tvalue_table)
  print(p)
  variables <- rownames(Tvalue_table)
  Tvalue_table <- stack(as.data.frame(Tvalue_table))
  Tvalue_table$variables <- rep(variables,p)
  # Visualisation
  ggplot2::ggplot(Tvalue_table, aes(x=variables, y=values)) +
    geom_col() +
    geom_hline(yintercept = -2, linetype="dashed", size=0.5, color="red") +
    geom_hline(yintercept = 2, linetype="dashed", size=0.5, color="red") +
    coord_flip() +
    facet_wrap(vars(ind))
}


