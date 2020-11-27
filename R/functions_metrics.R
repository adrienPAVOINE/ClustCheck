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