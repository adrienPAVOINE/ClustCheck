# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Class constructor
# ------------------------------------------------------------------------- #

#' Constructor
#'
#' @param data a dataset that contains all the active variables
#' @param pred_clusters a vector of dataset predicted clusters
#' @param true_clusters a vector of dataset actual clusters
#' @return an object returning a list of active data (categorical and numerical data), number of samples, etc.
#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats dist reshape
#' @import questionr
#' @import factoextra
#' @import ggplot2
#' @examples
Dataset <- function(data,pred_clusters,true_clusters=FALSE){
  instance <- list()
  #If the Cluster vector is not a factor
  pred_clusters <- factor(pred_clusters)
  #concatenate the vector and the data
  all_data <- cbind(group = pred_clusters, data)
  #if the vector is already in the data, we remove it
  all_data <- data.frame(all_data[,colnames(unique(as.matrix(all_data), MARGIN=2))])
  #extract all the active variables (1 is always the cluster group)
  active_data <- all_data[,-1]
  if (deparse(substitute(true_clusters)) != FALSE){
    #If the Cluster vector is not a factor
    true_clusters <- factor(true_clusters)
    # #concatenate the vector and the data
    # allactive_data <- cbind(true_clusters = true_clusters, active_data)
    # #if the vector is already in the data, we remove it
    # active_data <- data.frame(allactive_data[,colnames(unique(as.matrix(allactive_data), MARGIN=2))])
    # #extract all the active variables (1 is always the cluster group)
    # active_data <- active_data[,-1]
    instance$true_clusters <- true_clusters
  }

  instance$all_data <- all_data #all the data
  instance$active_data <- active_data  #data of all the active variables
  instance$pred_clusters = pred_clusters #data of the cluster vector
  instance$cluster_names <- unique(pred_clusters) #group names in the cluster vector
  instance$p <- ncol(active_data) #number of variables
  instance$n <- nrow(all_data) #number of samples-
  #controle - data.frame
  ok <- is.data.frame(data) #check if the dataset is a dataframe
  if (!ok){
    stop("The data is not in a dataframe format")
  }
  cat_ind <- sapply(active_data,function(x)is.factor(x)|is.character(x)) #To get all the categorical variables data
  nb_qual <- sum(cat_ind)
  cat_ind1 <- sapply(data,function(x)is.factor(x)|is.character(x)) #To get all the categorical variables including the vector of cluster data
  cat_data1 <- data[ ,cat_ind1]
  #CheckVarQual <- TRUE
  #To know if there is categorical variable (for the metrics functions)

  if (nb_qual < 1 ){
    #print("There are no categorical variables in your dataset")
    #CheckVarQual <- FALSE
  }else{
    instance$cat_data <- active_data[ ,cat_ind] #the data of the categorical variables
    instance$cat_data_cl <- cat_data1 #the data of the categorical variables including the vector of cluster data
    instance$cat_p <- nb_qual #number of categorical variables
    instance$cat_var_names <- names(which(cat_ind == TRUE)) #names of the categorical variables
    instance$cat_ind <- cat_ind
    instance$vcramer <- vcramer(instance) #return the cramer V value for all the category variables
    }
  num_ind = sapply(active_data,function(x)is.numeric(x)|is.double(x))#To get all the numerical variables data
  nb_quanti <- sum(num_ind)
  if (nb_quanti < 1 ){
    #print("There are no numerical variables in your dataset")
  }else{
    num_data <- active_data[ ,num_ind]#the data of the numerical variables
    instance$num_data <- num_data
    instance$num_p <- nb_quanti #number of numerical variables
    instance$num_var_names <- names(num_data) #names of the numerical variables
    instance$num_ind <- num_ind #index of the numerical variables
    instance$corr.ratio <- corr_ratios(instance)#return the corr ratio values for all the numerical variables
  }
  if (nb_qual>0){
    if (nb_quanti>0){
      vartype <- "MIX"
    }else{
        vartype <- "CAT"
        }
  }else{
    vartype <- "NUM"
    }
  instance$vartype <- vartype
  cat("Class correctly instanciated.","\n")
  cat("The dataset contains ",nb_qual," categorical variables and ",nb_quanti," numerical variables.")
  class(instance) <- "ccdata" #Creation of the ccdata class
  return(instance) #returns all instance parameters
}



