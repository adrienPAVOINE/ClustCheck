#' Constructeur
#'
#' @param data a dataset with factor variables
#' @param vargroupe the variable's name of the cluster
#'
#' @return a Factordataset object
#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats dist reshape
#' @import questionr
#' @import factoextra
#' @import ggplot2
#' @examples
Dataset <- function(data,vargroupe){
  idc <- which(names(data)==vargroupe)
  dataexp <- data[,-idc]
  instance <- list()
  instance$vargroupe = vargroupe
  instance$data <- data
  instance$p <- ncol(data)
  instance$n <- nrow(data)
  instance$clusters_data <- data[[vargroupe]]
  clus_names<- unique(data[[vargroupe]])
  instance$cluster_names <- clus_names
  #controle - data.frame
  ok <- is.data.frame(data)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  ind.qual = sapply(dataexp,function(x)is.factor(x)|is.character(x))
  nb_ok <- sum(ind.qual)
  ind.qual1 = sapply(data,function(x)is.factor(x)|is.character(x))
  data.qual1 <- data[ ,ind.qual1]
  CheckVarQual <- TRUE

  if (nb_ok < 1 ){
    print("vous n'avez pas de variables qualitatives")
    CheckVarQual <- FALSE

  }else{
    data.qual <- dataexp[ ,ind.qual]
    instance$p.qual <- ncol(data.qual)
    instance$all.var.qual <- data.qual1
    instance$var.qual.names <- names(data.qual)
    instance$data.qual = data.qual
    instance$ind.qual = ind.qual
    instance$vcramer <- Vcramer.Data(instance)
  }
  ind.quanti = sapply(data,function(x)is.numeric(x))
  nb_quanti <- sum(ind.quanti)
  if (nb_quanti < 1 ){
    print("There are no quantitative variables in your dataset")
  }else{
    data.quanti<- data[ ,ind.quanti]
    instance$data.quanti <- data.quanti
    instance$p.quanti <- ncol(data.quanti)
    instance$var.quanti.names <- names(data.quanti)
    instance$ind.quanti = ind.quanti
    instance$corr.ratio <- Corr_ratios.Data(instance)
  }

  instance$CheckVarQual <- CheckVarQual
  class(instance) <- "Data"
  return(instance)
}



