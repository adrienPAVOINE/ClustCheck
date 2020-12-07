# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for numerical variables
# ------------------------------------------------------------------------- #
#' Correlation ratios
#'
#' Calculate the correlation ratios,taken individually, in the build-up of the clustering structure
#'
#' @param object an object of class ccdata
#'
#' @return A matrix of correlations. Each column value represents the square of the correlation ratio η² for each variable (individually) with the feature variable (the entered clustering).
#' η² corresponds to the proportion of the variance explained.
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' corr_ratios(obj)
#' @export
corr_ratios <- function(object) {
  if (is.null(object$num_p) == TRUE) {
    stop("Error : Correlations ratios can only be calculated on numerical variables.")
  }
  classes <- object$pred_clusters # clusters
  cl <- object$cluster_names #cluster names
  n_clusters <- length(cl) #numbers of clusters

  #SCT------------
  sct_f <- function(x) {
    m <- mean(x)
    sct <- 0
    for (i in 1:length(x)) {
      #check if x or data
      sct <- (x[i] - m) ** 2 + sct
    }
    return(sct)
  }
  sct_l <- sapply(object$num_data, sct_f)

  #SCE-----------
  sce_f <- function(x) {
    m <- mean(x)
    sce <- 0
    for (i in cl) {
      ind_g <- which(classes == i) #get index of rows of type i cluster
      ng <- length(ind_g) # number of rows of type i cluster
      sce <- sce + (ng * (mean(x[ind_g]) - m) ** 2)
    }
    return(sce)
  }
  sce_l <- sapply(object$num_data, sce_f)

  #SCR------------
  scr_f <- function(x) {
    m <- mean(x)
    scr <- 0
    for (i in cl) {
      ind_g <- which(classes == i)  #get index of rows of type i cluster
      ng <- length(ind_g) # number of rows of type i cluster
      for (j in ind_g) {
        scr <- scr + (x[j] - mean(x[ind_g])) ** 2
      }
    }
    return(scr)
  }
  scr_l <- sapply(object$num_data, scr_f)

  corr <- sce_l / sct_l
  #print("Correlation matrix")
  #print(corr)
  return(corr)
}
#' Test Value for numerical variables
#'
#' Calculates the test values table for each variable per cluster
#'
#' @param object an object of class ccdata
#'
#' @return A matrix consisting of the test values of every variable for each cluster.
#' The rows represent the variables. The columns represent the clusters.
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' tvalue_num(obj)
#' @export
tvalue_num <-function(object){
  if (object$vartype == "CAT") {
    stop("Error : Correlations ratios can only be calculated on numerical variables.")
  }
  data <- object$num_data #numeric data
  classes <- object$pred_clusters #clusters
  n <- object$n #number of rows
  var_names <- object$num_var_names #variable names
  len_col<- object$num_p #number of numeric variables
  cl <- object$cluster_names #cluster names
  n_clusters <- length(cl) #numbers of clusters
  #initialize t_value_table : matrix indexing var_names by row and cluster names by column
  tvalue_table <- matrix(nrow = length(var_names), ncol = n_clusters, dimnames = list(var_names,cl))
  for (x_col in var_names){
    x <- data[[x_col]]  #values of variable x_col
    var_x <- var(x)
    m <- mean(x)
    for(i in cl){
      ind_g <- which(classes==i)  #get index of rows of type i cluster
      ng <-length(ind_g) # number of rows of type i cluster
      mean_g <- mean(x[ind_g])
      var_x <- var(x)
      vt_val <- (mean_g - m) / sqrt(((n-ng)*var_x)/((n-1)*ng))
      tvalue_table[x_col,i]<- vt_val
    }
  }
  #print("t-value table")
  #print(tvalue_table)
  return(tvalue_table)
}
#' Effect Size for numerical variables
#'
#' @param object an object of class ccdata
#'
#' @return An effect size table
#' matrix representing the effect size of every variable for each cluster.
#' The rows represent the variables. The columns represent the clusters.
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' effectsize(obj)
#' @export
effectsize <-function(object){
  if (object$vartype == "CAT") {
    stop("Error : Correlations ratios can only be calculated on numerical variables.")
  }
  data <- object$num_data
  classes <- object$pred_clusters
  n <- object$n
  var_names <- object$num_var_names
  len_col<- object$num_p
  cl <- object$cluster_names
  n_clusters <- length(cl)

  effect_size_table<-matrix(nrow = length(var_names), ncol = n_clusters,dimnames = list(colnames(data),cl))
  for (x_col in var_names){
    x<-data[[x_col]]
    var_x<-var(x)
    m<-mean(x)
    for(i in cl){
      ind_g<-which(classes==i) #get index of rows of type i cluster
      ind_a<-which(classes!=i) #get index of rows of type cluster !=i
      ng<-length(ind_g) # number of rows of type i cluster
      na<-length(ind_a) # number of rows where cluster type != i

      mean_g<-mean(x[ind_g]) #mean of cluster i
      mean_a<-mean(x[ind_a]) #mean of clusters != i

      sg2<-var(x[ind_g])  #variance of cluster i
      sa2<-var(x[ind_a])  #variance of clusters != i

      effect_size<-(mean_g - mean_a)*sqrt((ng+na)/( (ng-1)*sg2 +(na-1)*sa2 ))
      effect_size_table[x_col,i]<- effect_size
    }
  }
  #print("Effect size table")
  #print(effect_size_table)
  return(effect_size_table)
}

