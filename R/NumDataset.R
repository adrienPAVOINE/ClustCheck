# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for numerical variables
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Correlations ratios
# ------------------------------------------------------------------------- #

#' Correlation ratios
#'
#' Calculate the correlation ratios,taken individually, in the construction of the clustering structure
#'
#' @param object an object of class ccdata
#'
#' @return A matrix of correlations. Each column value represents the square of the correlation ratio η² for each variable (individually) with the characterizing variable (the entered clustering).
#' η² corresponds to the proportion of the variance explained.
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' corr_ratios(obj)
#' @export
corr_ratios <-function(object){
  classes <- object$pred_clusters
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
  sct_l<-sapply(object$num_data,sct_f)

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
  sce_l<-sapply(object$num_data,sce_f)

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
  scr_l<-sapply(object$num_data,scr_f)

  corr <- sce_l/sct_l
  print("Correlation matrix")
  print(corr)
  return(corr)
}

# ------------------------------------------------------------------------- #
# t-values for numerical variables
# ------------------------------------------------------------------------- #

#' Test Value for numerical variables
#'
#' Calculates the test value table for each variable per cluster
#'
#' @param object an object of class ccdata
#'
#' @return A matrix representing the value test of every variable for each cluster.
#' The rows represent the variables. The columns represent the clusters.
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' tvalue_num(obj)
#' @export
tvalue_num <-function(object){
  data <- object$num_data
  classes <- object$pred_clusters
  n <- object$n
  var_names <- object$num_var_names
  len_col<- object$num_p
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

# ------------------------------------------------------------------------- #
# effect size
# ------------------------------------------------------------------------- #

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

# ------------------------------------------------------------------------- #
# PCA
# ------------------------------------------------------------------------- #


#' PCA visualisation
#'
#' Applying a principal component analysis (PCA) to the data variables
#'
#' @param object an object of class ccdata
#'
#' @return Display three graphs to illustrate allows a synthetic view of the data, ideally in a two
#' First: Represents the correlation chart between all the variables
#' Second: Is split into two graphs. On the right; the correlation circle and on the right; the biplot of the individuals and variables grouped by their clusters
#'
#' @export
#' @import PerformanceAnalytics
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @import corrplot
#' @import ggpubr
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' get_PCA(obj)
get_PCA <- function(object){
  num_data <-object$num_data
  varname_classes <- "Clusters"
  classes <- object$pred_clusters
  nb_quanti <- object$num_p

  label_to_show<-"var"


  #Variable correlation
  correlation_chart <- PerformanceAnalytics::chart.Correlation(num_data, histogram=FALSE, pch=19)
  correlation_chart
  #PCA
  res.pca <- FactoMineR::PCA(num_data, graph = FALSE)

  eig.val <- factoextra::get_eigenvalue(res.pca)
  eig_plot <- factoextra::fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

  #Kaiser Rule
  I<- sum(eig.val[1:nb_quanti,1])
  dims<-c(which(eig.val[,1]>(I/nb_quanti)))
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

    #biplot individuals and variables grouped by clusters
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
