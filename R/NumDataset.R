#' NumDataset
#'
#' @param data a dataset
#' @param clusters_data a Numdataset object
#'
#' @return
#' @export
#' @importFrom stats var
#' @examples
NumDataset <- function(data,clusters_data){
  if (! is.data.frame(data)){
    stop("Data must be a dataframe")
  }

  if (is.data.frame(clusters_data)){
    clus_names<- unique(clusters_data[[1]])
  }else{
    clus_names<- unique(clusters_data)
  }

  ind.quanti = sapply(data,function(x)is.numeric(x))
  nb_quanti <- sum(ind.quanti)
  if (nb_quanti < 1 ){
    stop("There are no quantitative variables in your dataset")
  }
  data.quanti<- data[ ,ind.quanti]
  instance <- list()
  instance$data <- data
  instance$p <- ncol(data)
  instance$n <- nrow(data)
  instance$data.quanti <- data.quanti
  instance$p.quanti <- ncol(data.quanti)
  instance$var.quanti.names <- names(data.quanti)
  instance$clusters_data <- clusters_data
  instance$cluster_names <- clus_names
  class(instance) <- "NumDataset"
  return(instance)
}


#' Corr_ratios.NumDataset
#'
#' @param object a NumDataset object
#'
#' @return
#' @export
#'
#' @examples
Corr_ratios.NumDataset <-function(object){
  classes <- object$clusters_data
  cl <- object$cluster_names
  n_clusters <- length(cl)

  #SCT------------
  sct_f<- function(x){
    m<-mean(x)
    sct<-0
    for (i in 1:length(x)){#check if x or data
      sct<-(x[i]- m)**2 + sct
    }
    return(sct)
  }
  sct_l<-sapply(object$data.quanti,sct_f)

  #SCE-----------
  sce_f<- function(x){
    m <- mean(x)
    sce <- 0
    for (i in cl){
      ind_g<-which(classes==i)
      ng<-length(ind_g)
      sce<-sce + (ng*(mean(x[ind_g])-m)**2)
    }
    return(sce)
  }
  sce_l<-sapply(object$data.quanti,sce_f)

  #SCR------------
  scr_f<- function(x){
    m<-mean(x)
    scr<-0
    for (i in cl){
      ind_g <- which(classes==i)
      ng <- length(ind_g)
      for (j in ind_g){
        scr<-scr+ (x[j]-mean(x[ind_g]))**2
      }
    }
    return(scr)
  }
  scr_l<-sapply(object$data.quanti,scr_f)

  corr <- sce_l/sct_l
  print("Correlation matrix")
  print(corr)
}


#' TValueTable.NumDataset
#'
#' @param object a object NumData
#'
#' @return
#' @export
#'
#' @examples
TValueTable.NumDataset <-function(object){
  data <- object$data.quanti
  classes <- object$clusters_data
  n <- object$n
  var_names <- object$var.quanti.names
  len_col<- object$p.quanti
  cl <- object$cluster_names
  n_clusters <- length(cl)
  tvalue_table <- matrix(nrow = length(var_names), ncol = n_clusters, dimnames = list(var_names,cl))
  for (x_col in var_names){
    x <- data[[x_col]]
    var_x <- var(x)
    m <- mean(x)
    for(i in cl){
      ind_g <- which(classes==i)
      ng <-length(ind_g)
      mean_g <- mean(x[ind_g])
      var_x <- var(x)
      vt_val <- (mean_g - m) / sqrt(((n-ng)*var_x)/((n-1)*ng))
      tvalue_table[x_col,i]<- vt_val
    }
  }
  print("t-value table")
  print(tvalue_table)
}

#' EffectSizeTable.NumDataset
#'
#' @param object a NumDataset object
#'
#' @return
#' @export
#'
#' @examples
EffectSizeTable.NumDataset <-function(object){
  data <- object$data.quanti
  classes <- object$clusters_data
  n <- object$n
  var_names <- object$var.quanti.names
  len_col<- object$p.quanti
  cl <- object$cluster_names
  n_clusters <- length(cl)

  effect_size_table<-matrix(nrow = length(var_names), ncol = n_clusters,dimnames = list(colnames(data),cl))
  for (x_col in var_names){
    x<-data[[x_col]]
    var_x<-var(x)
    m<-mean(x)
    for(i in cl){
      ind_g<-which(classes==i)
      ind_a<-which(classes!=i)
      ng<-length(ind_g)
      na<-length(ind_a)

      mean_g<-mean(x[ind_g])
      mean_a<-mean(x[ind_a])

      sg2<-var(x[ind_g])
      sa2<-var(x[ind_a])

      effect_size<-(mean_g - mean_a)*sqrt((ng+na)/( (ng-1)*sg2 +(na-1)*sa2 ))
      effect_size_table[x_col,i]<- effect_size
    }
  }
  print("Effect size table")
  print(effect_size_table)
}
