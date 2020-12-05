# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for clustering evalation metrics
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Data tranformation for categorical and mixed variables
# ------------------------------------------------------------------------- #

#' transformdata
#'
#' @param object an object of class ccdata
#'
#' @return
#' @export
#' @import FactoMineR
#' @import factoextra
#'
#' @examples
transformdata <- function(object){
  if(object$vartype=="NUM"){
    stop("The variables are numerical and don't need factorial transformation")
  }
  res.famd <- FactoMineR::FAMD(object$active_data, graph = FALSE)
  ind <- factoextra::get_famd_ind(res.famd)
  return(as.data.frame(ind$coord))
}

# ------------------------------------------------------------------------- #
# Silhouette
# ------------------------------------------------------------------------- #

#' silhouette
#'
#' @param object an object of class ccdata
#' @param clusters a vector corresponding to the dataset clustering results
#'
#' @return
#' @export
#'
#' @examples
silhouette <- function(object, clusters=object$pred_clusters) {
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


# ------------------------------------------------------------------------- #
# Davies-Bouldin Index
# ------------------------------------------------------------------------- #

#' davies_bouldin
#'
#' @param object an object of class ccdata
#' @param clusters a vector corresponding to the dataset clustering results
#'
#' @return
#' @export
#'
#' @examples

# s : the average distance between each point of cluster and the centroid of that cluster – also know as cluster diameter
# d : the distance between cluster centroids

davies_bouldin <- function(object, clusters=object$pred_clusters) {
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


# ------------------------------------------------------------------------- #
# Dunn Index
# ------------------------------------------------------------------------- #

#' dunn_index
#'
#' @param object an object of class ccdata
#' @param clusters a vector corresponding to the dataset clustering results
#'
#' @return
#' @export
#'
#' @examples

# Calculated using the following:
# d1 : distance of samples to their centroids
# d2 : distance betwewen centroids
# Dunn Index is the ratio between the d2 min and the d1 max


dunn_index <- function(object, clusters=object$pred_clusters) {
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

# ------------------------------------------------------------------------- #
# Validation
# ------------------------------------------------------------------------- #

#' validation
#'
#' @param object an object of class ccdata
#'
#' @return
#' @export
#'
#' @examples
validation <-function(object){
  table <- contingency(object, object$true_clusters)
  tab <- table[[1]]
  ConfMat <- table[[2]]
  nli <- table[[5]]
  nco <- table[[4]]
  n <- object$n
  if(nli != nco){
    stop("You don't have the same numbers of clustes between predicted and true values")
  }else{
    if(nli ==2){
    Errorrate <- 1-((ConfMat[1,2]+ConfMat[2,1])/n)
    Recall <- ConfMat[1,1]/(ConfMat[1,3])
    Precision <- ConfMat[1,1]/ConfMat[3,1]
    matrix = matrix(Errorrate,Recall,Precision, nrow=3,ncol=1, dimnames = list(c("Errorrate","Recall","Precision"), c("Valeurs")))
    print(matrix)
    }else if(nli>2){
      print(ConfMat)
      Errorrate <- 1-((sum(diag(tab)))/n)
      print(sum(diag(tab)))
      cat("Error Rate :", Errorrate,"\n")
      print(nli)
      for(i in 1:nli){
        group <- object$cluster_names[i]
        cat("cluster:", group,"\n")
        Recall <- ConfMat[i,i]/ConfMat[i,nli+1]
        Precision <- ConfMat[i,i]/ConfMat[nli+1,i]
        matrix = matrix(c(Recall,Precision), nrow=2,ncol=1, dimnames = list(c("Recall","Precision"), c( "Valeurs")))
        print(matrix)
      }
    }

  }
}

# ------------------------------------------------------------------------- #
# Statistical Test
# ------------------------------------------------------------------------- #

#' statistical_test
#'
#' @param object an object of class ccdata
#' @param var a data vector of an active variable
#'
#' @return
#' @export
#' @importFrom stats aov reorder t.test var
#' @examples
statistical_test <- function(object, var){
  k <- length(object$cluster_names)
  if(is.numeric(object$all_data[[var]])){
    if (k == 1){
      stop("Vous n'avez qu'un seul groupe")
    }else if(k == 2){
      groupe <- unique(object$pred_clusters)
      cluster1 <- data[object$pred_clusters==groupe[1],]
      moy1 <- mean(cluster1[[var]])
      cluster2 <- data[object$pred_clusters==groupe[2],]
      moy2 <- mean(cluster2[[var]])
      if (moy1>moy2){
        test <- t.test(cluster1[[var]], cluster2[[var]], alternative = "greater")
        if (test$p.value < 0.05){
          cat("Le moyenne du", var, "est superieur chez le groupe", groupe[1], "que chez le groupe", groupe[2])
        }else{
          test <- t.test(cluster1[[var]], cluster2[[var]])
          if (test$p.value < 0.05){
            cat("La moyenne du", var, "est significativement differente entre les deux groupes")
          }}
      }else{
        test <- t.test(cluster2[[var]], cluster1[[var]], alternative = "greater")
        if (test$p.value < 0.05){
          cat("Le moyenne du", var, "est superieur chez le groupe", groupe[2], "que chez le groupe", groupe[])
        }else{
          test <- t.test(cluster1[[var]], cluster2[[var]])
          if (test$p.value < 0.05){
            cat("Le moyenne du", var, "est significativement differente entre les deux groupes")
          }
        }
      }

    }else{
      boxplot(object$all_data[[var]]~object$pred_clusters)
      mod=aov(object$all_data[[var]]~object$pred_clusters)
      p_value <- (summary(mod)[[1]][[1,"Pr(>F)"]])
      if (p_value < 0.05){
        cat("Le groupe a un lien significatif sur", var)
      }else{
        cat("il n'y a pas de lien significatif entre groupe et", var)
      }

    }
  }else{
    khi2 <- chisq.test(table(object$clusters_data, object$all_data[[var]]))
    if (khi2$p.value < 0.05){
      #cat("Les groupes n'ont significativement pas la ou le meme", var)
    }else
      cat("Il n'y a pas de lien significatif entre le groupe et", var)
  }
}


