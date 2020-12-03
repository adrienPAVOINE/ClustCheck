#' Constructeur
#'
#' @param data a dataset (with all your active variables)
#' @param vargroupe the vector of your cluster
#'
#' @return an object which return a list of active data, category and numeric data, number of samples, etc.
#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats dist reshape
#' @import questionr
#' @import factoextra
#' @import ggplot2
#' @examples
Dataset <- function(data,vargroupe){
  #If the Cluster vector is not a factor
  vargroupe <- factor(vargroupe)
  #concate the vector and the data
  allData <- cbind(groupe = vargroupe, data)
  #if the vector is already in the data, we remove it
  data = data.frame(allData[,colnames(unique(as.matrix(allData), MARGIN=2))])
  #exctract all the explain variables (1 is always the cluster group)
  dataexp <- data[,-1]
  instance <- list()
  instance$dataexp <- dataexp  #data of all the active variables
  instance$clusters_data = vargroupe #data of the cluster vector
  instance$data <- data #all the data
  instance$p <- ncol(data) #number of variables
  instance$n <- nrow(data) #number of sample
  clus_names<- unique(vargroupe) #group names in the cluster vector
  instance$cluster_names <- clus_names
  #controle - data.frame
  ok <- is.data.frame(data) #check if the dataset is a dataframe
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  ind.qual = sapply(dataexp,function(x)is.factor(x)|is.character(x)) #To get all the category variables data
  nb_ok <- sum(ind.qual)
  ind.qual1 = sapply(data,function(x)is.factor(x)|is.character(x)) #To get all the category variables including the cluster vector
  data.qual1 <- data[ ,ind.qual1]
  CheckVarQual <- TRUE
  #To know if there is category variable (for the functions metrix)
  if (nb_ok < 1 ){
    print("vous n'avez pas de variables qualitatives")
    CheckVarQual <- FALSE
  }else{
    data.qual <- dataexp[ ,ind.qual] #the data of the category variables
    instance$p.qual <- ncol(data.qual) #number of category variable
    instance$all.var.qual <- data.qual1 #the data of the category variables including the cluster vector's data
    instance$var.qual.names <- names(data.qual) #names of the cateory variables
    instance$data.qual = data.qual
    instance$ind.qual = ind.qual
    instance$vcramer <- Vcramer.Data(instance) #return the cramer value for all the category variables
  }
  ind.quanti = sapply(dataexp,function(x)is.numeric(x)|is.double(x))#To get all the numeric variables data
  nb_quanti <- sum(ind.quanti)
  if (nb_quanti < 1 ){
    print("There are no quantitative variables in your dataset")
  }else{
    data.quanti<- dataexp[ ,ind.quanti]#the data of the numeric variables
    instance$data.quanti <- data.quanti
    instance$p.quanti <- ncol(data.quanti) #number of numeric variables
    instance$var.quanti.names <- names(data.quanti) #names of the numeric variables
    instance$ind.quanti = ind.quanti #index of the numeric variables
    instance$corr.ratio <- Corr_ratios.Data(instance)#return the corr ratio value for all the numeric variables
  }

  instance$CheckVarQual <- CheckVarQual
  class(instance) <- "Data" #Creation of the data object
  return(instance) #return of all the information present in the instance
}



