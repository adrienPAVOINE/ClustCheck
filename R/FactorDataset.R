#' FactoDataset
#'
#' @param data a dataset with factor variables
#' @param vargroupe the variable's name of the cluster
#'
#' @return a Factordataset object
#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats dist reshape
#' @import questionr
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @examples
FactoDataset <- function(data,vargroupe){
  #controle - data.frame
  ok <- is.data.frame(data)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  ind.qual = sapply(data,function(x)is.factor(x)|is.character(x))
  data.qual1 <- data[ ,ind.qual]
  nb_ok <- sum(ind.qual)
  if (nb_ok <= 1 ){
    stop("vous n'avez pas de variables qualitatives autre que ", vargroupe, " dans votre dataset")
  }
  data.qual <- data.qual1[ , !(names(data.qual1) %in% vargroupe)]
  instance <- list()
  instance$data <- data
  instance$p <- ncol(data)
  instance$n <- nrow(data)
  instance$p.qual <- ncol(data.qual)
  instance$all.var.qual <- data.qual1
  instance$var.qual.names <- names(data.qual)
  instance$data.qual = data.qual
  instance$vargroupe = vargroupe
  instance$vcramer <- Vcramer.FactorDataset(instance)
  class(instance) <- "FactorDataset"
  return(instance)
}
#' Vcramer.FactorDataset
#'
#' @param object a object
#' @param var if you just want the Cramer for one varible
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
Vcramer.FactorDataset <- function(object, var = FALSE){
  if(var == FALSE){
    l<-c()
    for (i in object$var.qual.names){
      tableau <- table(object$data[[i]],object$data[[object$vargroupe]])
      nli <- nrow(tableau)
      nco <- ncol(tableau)
      khi2 = chisq.test(tableau)$statistic
      cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1)))))
      l <- c(l,cramer)
    }
    matrice = matrix(l,nrow=object$p.qual,ncol=1, dimnames = list(colnames(object$data.qual),"Cramer"))
    print(matrice)
    #listemax <- c()
    #ind = head(sort(matrice[,1], decreasing = TRUE), 3)
    #listemax <- c(listemax, ind)
    #print(listemax)
    #rownames(matrice)[ind]
  }else{
    tableau <- table(object$data[[var]],object$data[[object$vargroupe]])
    nli <- nrow(tableau)
    nco <- ncol(tableau)
    khi2 = chisq.test(tableau)$statistic
    cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1)))))
    cat("cramer entre la var groupe et ", var," = ", cramer)
  }
}
#' PhiValueTable.FactorDataset
#'
#' @param object a object
#' @param nomvarqual a string
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
PhiValueTable.FactorDataset <- function(object, nomvarqual){
  tableau <- table(object$data[[object$vargroupe]],object$data[[nomvarqual]])
  nli <- nrow(tableau)
  nco = ncol(tableau)
  eff = addmargins(tableau)
  pourc = addmargins(prop.table(addmargins(tableau,1),1),2)
  tab_phi <- tableau
  for (i in 1:nli){
    for (j in 1:nco){
      taille = eff[i,j]/eff[i,nco+1]
      liste <- c(eff[i,j],(eff[nli+1,j]-eff[i,j]), (eff[i,nco+1]-eff[i,j]),eff[nli+1,nco+1]-(eff[i,j]+(eff[nli+1]-eff[i,j])+(eff[i,nco+1]-eff[i,j])) )
      matri <- matrix(liste,2,2)
      khi2 <- chisq.test(matri)
      pla = (matri[2,1])/(matri[2,1]+matri[2,2])
      tab_phi[i,j] <- round(sign(taille-pla) * sqrt(khi2$statistic/eff[nli+1,nco+1]),4)
    }
  }
  print(tab_phi)
}
#' TValueTable.FactorDataset
#'
#' @param object a object
#' @param nomvarqual name of var
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
TValueTable.FactorDataset <-function(object, nomvarqual){
  tableau <- table(object$data[[object$vargroupe]],object$data[[nomvarqual]])
  nli <- nrow(tableau)
  nco = ncol(tableau)
  eff = addmargins(tableau)
  pourc = addmargins(prop.table(addmargins(tableau,1),1),2)
  tab_vtest <- tableau
  for (i in 1:nli){
    for (j in 1:nco){
      v = (sqrt(eff[nli+1,j]))*((pourc[i,j] - pourc[nli+1,j])/(sqrt(((eff[nli+1,nco+1]-eff[nli+1,j])/(eff[nli+1,nco+1] - 1))*pourc[nli+1,j]* (1-pourc[nli+1,j]))))
      tab_vtest[i,j] <- v
    }
  }
  print("ci dessous tableau des valeurs tests")
  print(tab_vtest)

}
#' VisualisationACM.FactorDataset
#'
#' @param object a object
#'
#' @return a graph
#' @export
#' @import questionr
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @importFrom graphics  barplot mosaicplot
#' @importFrom stats addmargins chisq.test
#' @examples
VisualisationACM.FactorDataset <- function(object){
  res.mca <- MCA(object$all.var.qual ,graph = FALSE)
  fviz_mca_var(res.mca, repel = TRUE,col.var = "contrib", ggtheme= theme_minimal())
}

#' VisualisationAC.FactorDataset
#'
#' @param object a object
#' @param nomvarqual a string
#'
#' @return
#' @export
#' @import questionr
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @importFrom graphics  barplot mosaicplot
#' @importFrom stats addmargins chisq.test
#' @examples
VisualisationAC.FactorDataset <-function(object, nomvarqual){
  tableau <- table(object$data[[object$vargroupe]],object$data[[nomvarqual]])
  lprop(tableau, digits=1)
  cprop(tableau, digits=2)
  colors <- c("chartreuse4", "chartreuse1", "orange","green")
  barplot(tableau, col=colors, main = "heureux par libert? sur internet", ylab="nombre ")
  mosaicplot(tableau, col = colors)
  res.ca <- CA(tableau, graph = TRUE)
}


