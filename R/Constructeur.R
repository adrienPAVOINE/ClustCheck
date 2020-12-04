#' Constructor
#'
#' @param data a dataset that contains all the active variables
#' @param vargroupe a vector corresponding to the dataset clustering results
#' @param TrueCluster if you have the true cluster groups
#' @return an object returning a list of active data (categorical and numerical data), number of samples, etc.
#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats dist reshape
#' @import questionr
#' @import factoextra
#' @import ggplot2
#' @examples
Dataset <- function(data,vargroupe,TrueCluster=FALSE){
  #If the Cluster vector is not a factor
  vargroupe <- factor(vargroupe)
  #concatenate the vector and the data
  allData <- cbind(groupe = vargroupe, data)
  #if the vector is already in the data, we remove it
  data <- data.frame(allData[,colnames(unique(as.matrix(allData), MARGIN=2))])
  #extract all the active variables (1 is always the cluster group)
  dataexp <- data[,-1]
  if (deparse(substitute(TrueCluster)) != FALSE){
    #If the Cluster vector is not a factor
    TrueCluster <- factor(TrueCluster)
    #concatenate the vector and the data
    allDataexp <- cbind(TrueCluster = TrueCluster, dataexp)
    #if the vector is already in the data, we remove it
    dataexp <- data.frame(allDataexp[,colnames(unique(as.matrix(allDataexp), MARGIN=2))])
    #extract all the active variables (1 is always the cluster group)
    dataexp <- dataexp[,-1]
  }
  instance <- list()
  instance$dataexp <- dataexp  #data of all the active variables
  instance$clusters_data = vargroupe #data of the cluster vector
  instance$data <- data #all the data
  instance$p <- ncol(dataexp) #number of variables
  instance$n <- nrow(data) #number of samples-
  instance$cluster_names <- unique(vargroupe) #group names in the cluster vector
  #controle - data.frame
  ok <- is.data.frame(data) #check if the dataset is a dataframe
  if (!ok){
    stop("The data is not in a dataframe format")
  }
  ind.qual <- sapply(dataexp,function(x)is.factor(x)|is.character(x)) #To get all the categorical variables data
  nb_qual <- sum(ind.qual)
  ind.qual1 <- sapply(data,function(x)is.factor(x)|is.character(x)) #To get all the categorical variables including the vector of cluster data
  data.qual1 <- data[ ,ind.qual1]
  #CheckVarQual <- TRUE
  #To know if there is categorical variable (for the metrics functions)

  if (nb_qual < 1 ){
    #print("There are no categorical variables in your dataset")
    #CheckVarQual <- FALSE
  }else{
    data.qual <- dataexp[ ,ind.qual] #the data of the categorical variables
    instance$p.qual <- nb_qual #number of categorical variables
    instance$all.var.qual <- data.qual1 #the data of the categorical variables including the vector of cluster data
    instance$var.qual.names <- names(which(ind.qual == TRUE)) #names of the categorical variables
    instance$data.qual <- data.qual
    instance$ind.qual <- ind.qual
    instance$vcramer <- Vcramer.Data(instance) #return the cramer V value for all the category variables
    }
  ind.quanti = sapply(dataexp,function(x)is.numeric(x)|is.double(x))#To get all the numerical variables data
  nb_quanti <- sum(ind.quanti)
  if (nb_quanti < 1 ){
    #print("There are no numerical variables in your dataset")
  }else{
    data.quanti <- dataexp[ ,ind.quanti]#the data of the numerical variables
    instance$data.quanti <- data.quanti
    instance$p.quanti <- nb_quanti #number of numerical variables
    instance$var.quanti.names <- names(data.quanti) #names of the numerical variables
    instance$ind.quanti <- ind.quanti #index of the numerical variables
    instance$corr.ratio <- Corr_ratios.Data(instance)#return the corr ratio values for all the numerical variables
  }
  if (nb_qual>0){
    if (nb_quanti>0){
      Vartype <- "MIX"
    }else{
        Vartype <- "CAT"
        }
  }else{
    Vartype <- "NUM"
    }
  instance$Vartype <- Vartype
  cat("Class correctly instanciated.","\n")
  cat("The dataset contains ",nb_qual," categorical variables and ",nb_quanti," numerical variables.")
  class(instance) <- "Data" #Creation of the data object
  return(instance) #returns all instance parameters
}



