# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for variables evaluation
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Plot of t-values
# ------------------------------------------------------------------------- #

#' plot.t
#'
#' @param tvalue a Table of test value
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
plotvalue <- function(tvalue){
  # t values table needs a particular format for the function to operation
  # i.e. 3 columns with variable (variable), t-value (t), cluster name (group)
  ggplot2::ggplot(tvalue, aes(x=variable, y=t)) +
    geom_col() +
    geom_hline(yintercept = -2, linetype="dashed", size=0.5, color="red") +
    geom_hline(yintercept = 2, linetype="dashed", size=0.5, color="red") +
    coord_flip() +
    facet_wrap(vars(group))
}
