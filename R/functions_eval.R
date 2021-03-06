# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for variables evaluation
# ------------------------------------------------------------------------- #
#' Plot of t-values
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active variable
#'
#' @return plot of the tests value for all numerical variables and all clusters or plot of tests values by modes of a selected categorical variable for all clusters.
#'
#' @export
#' @import ggplot2
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' plottvalue(obj) #for all the numerical variables
#' plottvalue(obj, BankCustomer$profession) #for only one categorical variable
plottvalue <- function(object, var = NULL) {
  if(object$vartype== "NUM" | object$vartype =="MIX"){
    table <- tvalue_num(object)
    p <- ncol(table)
    variables <- rownames(table)
    Tvalue_table <- stack(as.data.frame(table))
    Tvalue_table$variables <- rep(variables, p)
    # Visualisation
    plot <-ggplot2::ggplot(Tvalue_table, aes(x = variables, y = values)) +
      geom_col() +
      geom_hline(
        yintercept = -2,
        linetype = "dashed",
        size = 0.5,
        color = "red"
      ) +
      geom_hline(
        yintercept = 2,
        linetype = "dashed",
        size = 0.5,
        color = "red"
      ) +
      coord_flip() +
      facet_wrap(vars(ind)) +
      labs(title = "t-values")
    print(plot)
  }
  if(object$vartype== "CAT" | object$vartype=="MIX"){
    if (is.null(var) == FALSE) {
      if (is.character(var) == TRUE | is.factor(var) == TRUE) {
        table <- tvalue_cat(object, var)
        m <- ncol(table)
        levels <- colnames(table)
        Tvalue_table <- as.data.frame(table)
        Tvalue_table$Freq
        colnames(Tvalue_table) <- c('clusters', 'levels', 'values')
        # Visualisation
        ggplot2::ggplot(Tvalue_table, aes(x = levels, y = values)) +
          geom_col() +
          coord_flip() +
          facet_wrap(vars(clusters)) +
          labs(title = "t-values")
      }
    }
    else {
      print("Please enter a categorical vector if you want to observe the t value.")
    }
  }
}
# ------------------------------------------------------------------------- #
# size'effect plot
# ------------------------------------------------------------------------- #
#' Plot of size effect (numerical variables)
#'
#' @param object An object of class ccdata
#'
#' @return A plot of the effect size for all numerical variables by cluster.
#'
#' @export
#' @import ggplot2
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' plotsizeeff(obj)
plotsizeeff <- function(object){
  if(object$vartype=="NUM" | object$vartype=="MIX"){
    table <- effectsize(object)
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
  else {
    cat("Error : Size effect calculations are for numerical variables only.")
  }
}
# ------------------------------------------------------------------------- #
# correlation numeric plot
# ------------------------------------------------------------------------- #
#' Plot of correlations (numerical variables)
#'
#' @param object An object of class ccdata
#' @param limit Number of variables to display by descending value (default=10)
#'
#' @return A bar plot of the correlations between numerical variables and the cluster vector.
#' @export
#' @import ggplot2
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' plotcorr(obj)
plotcorr <- function(object, limit=10) {
  if(object$vartype=="NUM"| object$vartype=="MIX"){
    table <- sort(corr_ratios(object), decreasing = T)
    p <- length(table)
    if (limit<p){
      corr <- as.data.frame(table[1:limit])
      title2=paste("Correlation values - Top",limit,"out of",p,"variables")
    }else{
      corr <- as.data.frame(table[1:p])
      title2=paste("Correlation values - ",p,"variables")
    }
    colnames(corr) <- "values"
    # Visualisation
    ggplot(corr, aes(x = reorder(rownames(corr),-values), y = values)) +
      geom_col() +
      geom_text(aes(label=format(values, digits=3), vjust=-0.5)) +
      labs(title = title2) +
      xlab("Variables") +
      ylab("Value")
  }
  else {
    cat("Error : Correlations calculations are for numerical variables only.")
  }
}
# ------------------------------------------------------------------------- #
# Cramer'V plot
# ------------------------------------------------------------------------- #
#' Plot of Cramer's V (categorical variables)
#'
#' @param object An object of class ccdata
#' @param limit Number of categorical variables to display by descending value (default=10)
#'
#' @return A bar plot with the Cramer values between the categorical variables and the cluster vector.
#' @export
#' @import ggplot2
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' plotVCramer(obj)
plotVCramer <- function(object, limit=10){
  if(object$vartype=="CAT" | object$vartype=="MIX"){
    table <- vcramer(object)
    p <- length(table)
    if (limit<p){
      VCramer <- as.data.frame(table[1:limit,])
      title2=paste("Cramer's V values - Top",limit,"out of",p,"variables")
    }else{
      VCramer <- as.data.frame(table[1:p,])
      title2=paste("Cramer's V values - ",p,"variables")
    }
    colnames(VCramer) <- "values"
    # Visualisation
    ggplot2::ggplot(VCramer, ggplot2::aes(x=reorder(rownames(VCramer), -values), y=values)) +
      ggplot2::geom_col() +
      geom_text(aes(label=format(values, digits=2), vjust=-0.5)) +
      ggplot2::labs(title = title2) +xlab("Variables") +ylab("Value")
  }
  else {
    cat("Error : Cramer's V calculations are for categorical variables only.")
  }
}
# ------------------------------------------------------------------------- #
# phi'values plot
# ------------------------------------------------------------------------- #
#' Plot of Phi values (categorical variables)
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active variable
#'
#' @return A plot for all the phi values between cluster group and modes of the selected variables.
#' @export
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' plotphi(obj, BankCustomer$profession)
plotphi <- function(object, var){
  if(is.factor(var) == TRUE|is.character(var) == TRUE){
    table <- phivalue(object, var)
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
  else {
    cat("Error : Phi values calculations are for categorical variables only.")
  }
}
