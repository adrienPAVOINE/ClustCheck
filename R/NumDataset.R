
#' Corr_ratios.Data
#'
#' @param object a NumDataset object
#'
#' @return
#' @export
#'
#' @examples
Corr_ratios.Data <-function(object){
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
  return(corr)
}




#' TValueTable.NumDataset
#'
#' @param object a object object
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
  return(tvalue_table)
}

#' EffectSizeTable.NumDataset
#'
#' @param object a NumDataset object
#'
#' @return
#' @export
#'
#' @examples
EffectSizeTable.Data <-function(object){
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
  return(effect_size_table)
}



#' Get_PCA.Data
#'
#' @param object a data object
#' @param index_names optional vector of the index to use
#'
#' @return
#' @export
#' @import PerformanceAnalytics
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @import corrplot
#' @import ggpubr
#' @examples
Get_PCA.Data<- function(object,index_names){
  data.quanti <-object$data.quanti
  varname_classes <- "Clusters"
  classes <- object$clusters_data
  
  
  label_to_show<-"var"
  # #if index specified, use index for labels in plots
  # if(missing(index_names)) {
  #   label_to_show<-"var"
  # } else {
  #   rownames(data.quanti) <- index_names 
  #   label_to_show<-"all"
  # }
  
  #Variable correlation
  correlation_chart <- PerformanceAnalytics::chart.Correlation(data.quanti, histogram=FALSE, pch=19)
  
  #PCA
  res.pca <- FactoMineR::PCA(data.quanti, graph = FALSE)
  
  eig.val <- factoextra::get_eigenvalue(res.pca)
  eig_plot <- factoextra::fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
  
  #keep values where eigenvalue > 1
  # dims<-which(eig.val[,1]>1)
  # n_dim <- length(dims)
  # eig.val.kept <- eig.val[dims,]
  
  #keep values explaining 85% of total variance
  dims<-c(which(eig.val[,3]<85),which(eig.val[,3]>=85)[1])
  n_dim <- length(dims)
  eig.val.kept <- eig.val[dims,]
  
  #keep values where eigenvalue > 1
  if(n_dim==1){
    n_dim<-2
  }
  
  for (i in seq(1,n_dim, by=2)){
    if(i==n_dim){
      dim_axes<-c(i-1,i)
    }else{
      dim_axes<-c(i,i+1)
    }
    
    #correlation circle
    corr_circle <- factoextra::fviz_pca_var(res.pca, col.var = "cos2", axes=dim_axes,
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE)
    
    #biplot simple des individus et des variables selon les clusters
    biplot <- factoextra::fviz_pca_biplot (res.pca, axes=dim_axes,
                               col.ind = classes, palette = "jco",
                               addEllipses = TRUE,
                               label = label_to_show,
                               ellipse.type = "convex",
                               #ellipse.type = "confidence",
                               col.var = "black", repel = TRUE,
                               legend.title = varname_classes, mean.point = FALSE)
    
    
    plt <- ggpubr::ggarrange(corr_circle, biplot,labels = c('b', 'a'), widths = c(1,2),ncol = 2, nrow = 1)
    
    print(plt)
  }
}