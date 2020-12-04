#' Transformdata.Data
#'
#' @param object a data object
#'
#' @return
#' @export
#' @import FactoMineR
#' @import factoextra
#'
#' @examples
Transformdata.Data <- function(object){
  # #fonction pour centrage-réduction
  # CR <- function(x){
  #   n <- length(x)
  #   m <- mean(x)
  #   v <- (n-1)/n*var(x)
  #   return((x-m)/sqrt(v))
  # }
  # #appliquer la fonction sur les variables continues
  # varcont <- data.frame(lapply(subset(object$dataexp,select=object$ind.quanti),CR))
  # #codage disjonctif complet
  # #library(ade4)
  # varquali <- ade4::acm.disjonctif(subset(object$dataexp,select=object$ind.qual))
  # #fonction pour pondération des indicatrices
  # PF <- function(x){
  #   m <- mean(x)
  #   return(x/sqrt(m))
  # }
  # #appliquer la pondération sur les indicatrices
  # varquali.pond <- data.frame(lapply(varquali,PF))
  # #données transformées envoyées ?l'ACP
  # data.pour.acp <- cbind(varcont,varquali.pond)
  # nbcol.tot <- ncol(data.pour.acp)
  # print(nbcol.tot)
  # rownames(data.pour.acp) <- rownames(object$data)
  res.famd <- FactoMineR::FAMD(object$dataexp, graph = FALSE)
  ind <- factoextra::get_famd_ind(res.famd)
  return(ind$coord)

  #
  #
  # acp.data <- ade4::dudi.pca(data.pour.acp,center=T,scale=F,scannf=F, nf=nbcol.tot)
  # coordind = round(acp.data$li[,])
  # ind <-cbind(coordind)



}


# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for clustering evalation metrics
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Silhouette
# ------------------------------------------------------------------------- #
#' silhouette.Data
#'
#' @param object a datset object
#' @param Clusters a vector cluster
#'
#' @return
#' @export
#'
#' @examples
silhouette.Data <- function(object, Clusters) {
  print(object$Vartype)
  if(object$Vartype== "CAT" | object$Vartype == "MIX"){
    data <- Transformdata.Data(object)
  }else{
    data <- object$dataexp
  }
  print(data)
  # a: The mean distance between a sample and all other points in the same class.
  # b: The mean distance between a sample and all other points in the next nearest cluster.
  if (nrow(data)!=length(Clusters)){
    stop("Feature data and cluster data don't have the same length")
  }
  d <- as.matrix(dist(data))
  n <- ncol(d)
  a <- NULL; b <- NULL
  for (col in 1:n){
    cluster <- Clusters[col]
    # calculation for a
    same_class <- which(Clusters==cluster) # identification of the class samples
    same_class_wo_sample <- same_class[which(same_class!=col)] # we remove the sample here
    a <- c(a,mean(d[same_class_wo_sample,col]))
    # calculation for b
    all_different_class <- which(Clusters!=cluster) # identification of all the other samples
    w <- as.integer(names(which.min(d[all_different_class,col]))) # identification of the closest sample in an other sample
    nearest_cluster <- Clusters[w] # identification of the next nearest cluster
    different_class <- which(Clusters==nearest_cluster)
    b <- c(b,mean(d[different_class,col]))
  }
  s <- (b - a)/pmax(a,b) # silhouette formula


  for (k in unique(Clusters)){
    ind = which(Clusters == k)
    nbk <- sum(Clusters == k)
    sk <- 1/nbk * (sum(s[ind]))
    cat("Silhouette du groupe" , k )
    print(sk)
  }
  return(mean(s))
}





# ------------------------------------------------------------------------- #
# Davies-Bouldin Index
# ------------------------------------------------------------------------- #

# s : the average distance between each point of cluster and the centroid of that cluster – also know as cluster diameter
# d : the distance between cluster centroids

davies_bouldin <- function(data, clusters) {
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
#
# Calculated using the following:
# d1 : distance of samples to their centroids
# d2 : distance betwewen centroids
# Dunn Index is the ratio between the d2 min and the d1 max


dunn_index <- function(data, clusters) {
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







#' TestStatistique.Data
#'
#' @param object a data object
#' @param varexp a string
#'
#' @return
#' @export
#' @importFrom stats aov reorder t.test var
#' @examples
TestStatistique.Data <- function(object, varexp){
  k <- length(object$cluster_names)
  if(is.numeric(object$data[[varexp]])){
    if (k == 1){
      stop("Vous n'avez qu'un seul groupe")
    }else if(k == 2){
      groupe <- unique(object$clusters_data)
      cluster1 <- data[object$clusters_data==groupe[1],]
      moy1 <- mean(cluster1[[varexp]])
      cluster2 <- data[object$clusters_data==groupe[2],]
      moy2 <- mean(cluster2[[varexp]])
      if (moy1>moy2){
        test <- t.test(cluster1[[varexp]], cluster2[[varexp]], alternative = "greater")
        if (test$p.value < 0.05){
          cat("Le moyenne du", varexp, "est superieur chez le groupe", groupe[1], "que chez le groupe", groupe[2])
        }else{
          test <- t.test(cluster1[[varexp]], cluster2[[varexp]])
          if (test$p.value < 0.05){
            cat("La moyenne du", varexp, "est significativement differente entre les deux groupes")
          }}
      }else{
        test <- t.test(cluster2[[varexp]], cluster1[[varexp]], alternative = "greater")
        if (test$p.value < 0.05){
          cat("Le moyenne du", varexp, "est superieur chez le groupe", groupe[2], "que chez le groupe", groupe[])
        }else{
          test <- t.test(cluster1[[varexp]], cluster2[[varexp]])
          if (test$p.value < 0.05){
            cat("Le moyenne du", varexp, "est significativement differente entre les deux groupes")
          }
        }
      }

    }else{
      boxplot(object$data[[varexp]]~object$clusters_data)
      mod=aov(object$data[[varexp]]~object$clusters_data)
      p_value <- (summary(mod)[[1]][[1,"Pr(>F)"]])
      if (p_value < 0.05){
        cat("Le groupe a un lien significatif sur", varexp)
      }else{
        cat("il n'y a pas de lien significatif entre groupe et", varexp)
      }

    }
  }else{
    khi2 <- chisq.test(table(object$clusters_data, object$data[[varexp]]))
    if (khi2$p.value < 0.05){
      #cat("Les groupes n'ont significativement pas la ou le meme", varexp)
    }else
      cat("Il n'y a pas de lien significatif entre le groupe et", varexp)
  }
}


