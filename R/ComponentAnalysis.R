# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for multivariate analysis
# ------------------------------------------------------------------------- #
#' PCA visualisation
#'
#' Applying a principal component analysis (PCA) to the feature variables
#'
#' @param object an object of class ccdata
#'
#' @return Displays three graphs to allow a synthetic view of the data
#' A. Represents the correlation chart between all the variables
#' B. split into two graphs. On the left: the correlation circle and on the right: the biplot of the individuals and variables grouped by their clusters
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
  if(object$vartype=="NUM" |object$vartype=="MIX"){
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
  else {
    cat("Error : PCA is intended to numerical variables only.")
  }

}
#' Multiple Component Analysis
#' Show two graphs to illustrate the cluster with all the categorical variables of your dataset.
#'
#' @param object An object of class ccdata
#'
#'
#' @return A first plot with all the modes of the categorical variables placed by importance of contribution with the two first axes.
#' A biplot whho illustrate the cluster group with all the modes of categorical variables.
#' @export
#' @import ExPosition
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @import corrplot
#' @import ggpubr
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' get_MCA(obj)
get_MCA <- function(object){
  if(object$vartype=="CAT" | object$vartype =="MIX"){
    data.quali <-object$cat_data
    varname_classes <- "Clusters"
    classes <- object$pred_clusters
    label_to_show<-"var"
    #MCA
    #We use the Benzecri correction
    res.mca = ExPosition::epMCA(data.quali, graphs = FALSE, correction = "b")
    eig.val <- factoextra::get_eigenvalue(res.mca)
    eig_plot <- factoextra::fviz_eig(res.mca, addlabels = TRUE, ylim = c(0, 50))
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
      #Importance of the variable
      VarPlot <- factoextra::fviz_mca_var (res.mca,select.var = list(cos2=0.85), repel = TRUE)
      print(dim_axes)
      #simple biplot between sample and active variables
      biplot <- factoextra::fviz_mca_biplot(res.mca, axes=dim_axes,
                                            col.ind = classes, palette = "jco",
                                            addEllipses = TRUE,
                                            label = label_to_show,
                                            ellipse.type = "convex",
                                            #ellipse.type = "confidence",
                                            col.var = "black", repel = TRUE,
                                            legend.title = varname_classes, mean.point = FALSE)
      plt <- ggpubr::ggarrange(VarPlot, biplot,labels = c('b', 'a'), widths = c(1,2),ncol = 2, nrow = 1)
      print(plt)
    }
  }
  else {
    cat("Error : MCA is intended to categorical variables only.")
  }

}
#' Factorial Analysis of Mixed Data
#'
#' @param object An object of class ccdata
#'
#'
#' @return three graphs to illustrate the cluster with all the variables of your dataset.
#' A. Plot with all the individuals colored by their cluster group and illustrated by categorical and numerical variables
#' B. Plot with all the modes of the categorical variables placed by importance of contribution with the first two axes.
#' C. Correlation circle with all the numerical variables
#' @export
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @import corrplot
#' @import ggpubr
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' get_FAMD(obj)
get_FAMD <- function(object){
  if(object$vartype=="MIX"){
    #loop to concate name of variable + modality (to avoid error in the AFDM)
    nb = 0
    for(i in object$cat_data) {
      nb = nb+1
      names = object$cat_var_names[nb]
      i <- paste(names,i)
      object$cat_data[nb] <- i

    }
    #first data with new exp variables
    data <- cbind(object$num_data, object$cat_data)
    #data with active var and cluster
    all.data <- cbind(object$pred_clusters,object$num_data, object$cat_data)
    #creation on the FAMD
    res.famd <- FactoMineR::FAMD(data, graph = FALSE)
    #to get variable
    var <- factoextra::get_famd_var(res.famd)

    #Contrib of the categorical variables
    qual <- factoextra::fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
                                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
    )

    #contrib of the numeric variables
    quanti <- factoextra::fviz_famd_var(res.famd, "quanti.var", col.var = "contrib",
                                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                        repel = TRUE)
    #the ind plot with color by cluster group, we only use the two firsts axes
    ind<- factoextra::fviz_famd_ind(
      res.famd,
      axes = c(1, 2),
      repel = FALSE,
      habillage = all.data[,1],
      palette = NULL,
      addEllipses = TRUE,
      col.ind = "blue",
      col.ind.sup = "darkblue",
      col.quali.var = "black",
      select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
      gradient.cols = NULL
    )
    #split of the screen
    plt <- ggpubr::ggarrange(ind,         # First row with ind plot
                             ggpubr::ggarrange(qual, quanti, ncol = 2, labels = c("B", "C")), # Second row with var quanti and qual plot
                             nrow = 2,
                             labels = "A"       # Labels of the ind plot
    )
    print(plt)
  }
  else {
    cat("Error : this factorial analysis function is intended to mixed data only.")
  }
}
