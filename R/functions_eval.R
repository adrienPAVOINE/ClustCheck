# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for variables evaluation
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Plot of t-values
# ------------------------------------------------------------------------- #

plot.t <- function(tvalue){
  # t values table needs a particular format for the function to operation
  # i.e. 3 columns with variable (variable), t-value (t), cluster name (group)
  library(ggplot2) # to be put in the main class maybe rather than here
  ggplot(tvalue, aes(x=variable, y=t)) +
    geom_col() +
    geom_hline(yintercept = -2, linetype="dashed", size=0.5, color="red") +
    geom_hline(yintercept = 2, linetype="dashed", size=0.5, color="red") +
    coord_flip() +
    facet_wrap(vars(group))
}
