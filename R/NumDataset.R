

NumDataset <- function(data,clusters_data){
  if (! is.data.frame(data) | ! is.data.frame(clusters_data) ){
    stop("Les données doivent être sous forme de data frame")
  }
  ind.quanti = sapply(data,function(x)is.numeric(x))
  nb_quanti <- sum(ind.quanti)
  if (nb_quanti < 1 ){
    stop("Il n'y a pas de variables quantitatives dans votre dataset")
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
  instance$cluster_names <- unique(clusters_data[[1]])
  class(instance) <- "NumDataset"
  return(instance)
}


TValueTable.NumDataset <-function(object){
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
  print("Tableau des valeurs tests")
  print(tvalue_table)
}


#TValueTable.NumDataset <-function(object){
EffectSizeTable.NumDataset <- function(data){
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
  print("Tableau des tailles d'effet")
  print(effect_size_table)
}