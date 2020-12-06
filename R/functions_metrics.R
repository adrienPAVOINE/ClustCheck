# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for clustering evalation metrics
# ------------------------------------------------------------------------- #
#' transformdata
#'
#'Data tranformation for categorical and mixed variables
#'
#' @param object An object of class ccdata
#'
#' @return A new dataset for the metrics function, created with the new coordinates based on a FAMD.
#' @export
#' @import FactoMineR
#' @import factoextra
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' transformdata(obj)
transformdata <- function(object){
  if(object$vartype=="NUM"){
    stop("Error : the variables are numerical and don't need factorial transformation")
  }
  res.famd <- FactoMineR::FAMD(object$active_data, graph = FALSE)
  ind <- factoextra::get_famd_ind(res.famd)
  return(as.data.frame(ind$coord))
}
#' Silhouette coefficient
#'
#' @param object An object of class ccdata
#' @param clusters A vector corresponding to the dataset clustering results (predicted clusters with ccdata class object by default)
#'
#' @return The silouhette value for all the cluster groups and a mean silouhette.
#' @export
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' silhouetteC(obj)
silhouetteC <- function(object, clusters=object$pred_clusters) {
  if(object$vartype!= "NUM"){
    data <- transformdata(object)
  }else{
    data <- object$active_data
  }
  # a: The mean distance between a sample and all other points in the same class.
  # b: The mean distance between a sample and all other points in the next nearest cluster.
  # if (nrow(object)!=length(clusters)){
  #   stop("Feature data and cluster data don't have the same length")
  # }
  d <- as.matrix(dist(data))
  n <- ncol(d)
  a <- NULL; b <- NULL
  for (col in 1:n){
    cluster <- clusters[col]
    # calculation for a
    same_class <- which(clusters==cluster) # identification of the class samples
    same_class_wo_sample <- same_class[which(same_class!=col)] # we remove the sample here
    a <- c(a,mean(d[same_class_wo_sample,col]))
    # calculation for b
    all_different_class <- which(clusters!=cluster) # identification of all the other samples
    w <- as.integer(names(which.min(d[all_different_class,col]))) # identification of the closest sample in an other sample
    nearest_cluster <- clusters[w] # identification of the next nearest cluster
    different_class <- which(clusters==nearest_cluster)
    b <- c(b,mean(d[different_class,col]))
  }
  s <- (b - a)/pmax(a,b) # silhouette formula

  # Cluster silhouette
  sk <- NULL
  for (k in unique(clusters)){
    ind = which(clusters == k)
    nbk <- sum(clusters == k)
    #sk <- 1/nbk * (sum(s[ind]))
    #cat("Silhouette for cluster" , k )
    sk <- c(sk, 1/nbk * (sum(s[ind])))
  }
  return(list(cluster_silhouette=sk, mean_silhouette=mean(s)))
}
#' Davies-Bouldin Index
#'
#' @param object An object of class ccdata
#' @param clusters A vector corresponding to the dataset clustering results (predicted clusters with ccdata class object by default)
#'
#' @return The Davies-Bouldin index for all the cluster groups.
#' @export
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' davies_bouldinC(obj)
davies_bouldinC <- function(object, clusters=object$pred_clusters) {
  # s : the average distance between each point of cluster and the centroid of that cluster – also know as cluster diameter
  # d : the distance between cluster centroids
  if(object$vartype!= "NUM"){
    data <- transformdata(object)
  }else{
    data <- object$active_data
  }
  k <- length(unique(clusters))
  p <- ncol(data)
  centroids <- matrix(nrow=k, ncol=p)
  d <- matrix(nrow=k, ncol=k)
  s <- matrix(nrow=1, ncol=k)
  R <- matrix(nrow=k, ncol=k)
  maxR <- matrix(nrow=1, ncol=k)
  # Centroids calculation
  i=1
  for (g in unique(clusters)){
    k_data <- data[which(clusters==g),]
    centroids[i,] <- sapply(k_data, mean, na.rm=T)
    s[i] <- sqrt(mean(rowSums(apply(k_data, 2, function(y) (y - mean(y))^2))))
    i = i+1
  }
  # R and d calculation
  for (i in 1:k){
    for (j in 1:k){
      d[i,j] <- sqrt(sum((centroids[i,] - centroids[j,])^2))
      R[i,j] <- (s[i]+s[j])/d[i,j]
    }
  }
  # Index calculation
  for (i in 1:k){
    maxR[i] <- max(R[i,][is.finite(R[i,])])
  }
  DB <- sum(maxR)/k
  return(DB)
}
#' Dunn Index
#'
#' @param object An object of class ccdata
#' @param clusters A vector corresponding to the dataset clustering results (predicted clusters with ccdata class object by default)
#'
#' @return The Dunn Index for all the cluster groups
#' @export
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' dunn_indexC(obj)
dunn_indexC <- function(object, clusters=object$pred_clusters) {
  # Calculated using the following:
  # d1 : distance of samples to their centroids
  # d2 : distance betwewen centroids
  # Dunn Index is the ratio between the d2 min and the d1 max
  if(object$vartype!= "NUM"){
    data <- transformdata(object)
  }else{
    data <- object$active_data
  }
  k <- length(unique(clusters))
  p <- ncol(data)
  centroids <- matrix(nrow=k, ncol=p)
  d1 <- matrix(nrow=1, ncol=k)
  d2 <- matrix(nrow=k, ncol=k)
  # Centroids calculation
  i=1
  for (g in unique(clusters)){
    k_data <- data[which(clusters==g),]
    centroids[i,] <- sapply(k_data, mean, na.rm=T)
    i = i+1
  }
  # d2 calculation
  for (i in 1:k){
    for (j in 1:k){
      d2[i,j] <- sqrt(sum((centroids[i,] - centroids[j,])^2))
    }
  }
  # d1 calculation
  i=1
  for (g in unique(clusters)){
    k_data <- data[which(clusters==g),]
    d1[i] <- sqrt(mean(rowSums(apply(k_data, 2, function(y) (y - mean(y))^2))))
    i = i+1
  }
  DI <- min(d2[d2>0])/max(d1)
  return(DI)
}
#' Evaluates the performance of the classifier by comparing predicted vs true clusters (true clusters required as input)
#'
#' @param object An object of class ccdata
#'
#' @param true_clusters Vector of the true clusters (true clusters with ccdata class object by default)
#'
#'
#' @return Confusion matrix, error rate, recall and precision. A matrix of recall and precision for all the cluster groups.
#' @export
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' validation(obj, BankCustomer$Cluster)
validation <- function(object, true_clusters = object$true_clusters) {
  if (is.null(true_clusters) == FALSE) {
    table <- contingency(object, true_clusters)
    tab <- table[[1]]
    ConfMat <- table[[2]]
    nli <- table[[5]]
    nco <- table[[4]]
    n <- object$n
    if (nli != nco) {
      stop("Error : you don't have the same numbers of clusters between predicted and true values")
    } else{
      if (nli == 2) {
        Errorrate <- 1 - ((ConfMat[1, 2] + ConfMat[2, 1]) / n)
        Recall <- ConfMat[1, 1] / (ConfMat[1, 3])
        Precision <- ConfMat[1, 1] / ConfMat[3, 1]
        matrix = matrix(
          Errorrate,
          Recall,
          Precision,
          nrow = 3,
          ncol = 1,
          dimnames = list(c(
            "Errorrate", "Recall", "Precision"
          ), c("Valeurs"))
        )
        print(matrix)
      } else if (nli > 2) {
        print(ConfMat)
        Errorrate <- 1 - ((sum(diag(tab))) / n)
        cat("Error Rate :", Errorrate, "\n")
        for (i in 1:nli) {
          group <- object$cluster_names[i]
          cat("cluster:", group, "\n")
          Recall <- ConfMat[i, i] / ConfMat[i, nli + 1]
          Precision <- ConfMat[i, i] / ConfMat[nli + 1, i]
          matrix = matrix(
            c(Recall, Precision),
            nrow = 2,
            ncol = 1,
            dimnames = list(c("Recall", "Precision"), c("Valeurs"))
          )
          print(matrix)
        }
      }

    }
  } else{
    stop("Error : you didn't enter a true cluster vector")
  }
}
#' Statistical Test
#'
#' @param object An object of class ccdata
#' @param var The string of the variable name
#'
#' @return A sentence indicating whether the two variables are significantly different
#' @export
#' @importFrom stats aov reorder t.test var
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' statistical_test(obj,"profession") #To know if the cluster have significantly the same job or not
#' statistical_test(obj,"revenu") #To know if the cluster have significantly the same salary
statistical_test <- function(object, var){
  k <- length(object$cluster_names)
  if(is.numeric(object$all_data[[var]])){
    if (k == 1){
      stop("Error : you have only one cluster group")
    }else if(k == 2){
      groupe <- unique(object$pred_clusters)
      cluster1 <- data[object$pred_clusters==groupe[1],]
      moy1 <- mean(cluster1[[var]])
      cluster2 <- data[object$pred_clusters==groupe[2],]
      moy2 <- mean(cluster2[[var]])
      if (moy1>moy2){
        test <- t.test(cluster1[[var]], cluster2[[var]], alternative = "greater")
        if (test$p.value < 0.05){
          cat("Mean of", var, "is signifantly higher in", groupe[1], "than in", groupe[2])
        }else{
          test <- t.test(cluster1[[var]], cluster2[[var]])
          if (test$p.value < 0.05){
            cat("Mean of", var, "is significantly different between the two cluster groups")
          }}
      }else{
        test <- t.test(cluster2[[var]], cluster1[[var]], alternative = "greater")
        if (test$p.value < 0.05){
          cat("Mean of", var, "is signifantly higher in", groupe[2], "than in", groupe[])
        }else{
          test <- t.test(cluster1[[var]], cluster2[[var]])
          if (test$p.value < 0.05){
            cat("Mean of", var, "is significantly different between the two cluster groups")
          }
        }
      }
    }else{
      boxplot(object$all_data[[var]]~object$pred_clusters)
      mod=aov(object$all_data[[var]]~object$pred_clusters)
      p_value <- (summary(mod)[[1]][[1,"Pr(>F)"]])
      if (p_value < 0.05){
        cat("The cluster group has a significant impact on", var)
      }else{
        cat("There are not significant impact of the cluster on", var)
      }
    }
  }else{
    khi2 <- chisq.test(table(object$pred_clusters, object$all_data[[var]]))
    if (khi2$p.value < 0.05){
      cat("The cluster significantly doesn't have the same ", var)
    }else
      cat("There are not significant impact of the cluster on", var)
  }
}
