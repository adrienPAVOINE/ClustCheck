# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for clustering evalation metrics
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Silhouette
# ------------------------------------------------------------------------- #

silhouette <- function(data, clusters) {
  # a: The mean distance between a sample and all other points in the same class.
  # b: The mean distance between a sample and all other points in the next nearest cluster.
  if (nrow(data)!=length(clusters)){
    stop("Feature data and cluster data don't have the same length")
  }
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
  return(mean(s))
}


# ------------------------------------------------------------------------- #
# Davies-Bouldin Index
# ------------------------------------------------------------------------- #

# s : the average distance between each point of cluster and the centroid of that cluster – also know as cluster diameter
# d_centroids : the distance between cluster centroids

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