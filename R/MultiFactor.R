#' MultiFactorConst
#'
#' @param data a dataset
#' @param vargroupe a string
#'
#' @return a object
#' @export
#' @importFrom grDevices rainbow
#' @importFrom stats dist reshape
#' @import questionr
#' @examples
MultiFactorConstr <- function(data,vargroupe){
  #contr?le - data.frame
  ok <- is.data.frame(data)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  ind.qual = sapply(data,function(x)is.factor(x)|is.character(x))
  data.qual <- data[ ,ind.qual]
  print(data.qual)
  data.qual = data.qual[ , !(names(data.qual) %in% vargroupe)]
  print(data.qual)
  instance <- list()
  instance$data = data
  instance$data.qual = data.qual
  instance$vargroupe = vargroupe
  instance$vcramer <- Vcramer.MultiFactor(instance)
  class(instance) <- "MultiFactor"
  return(instance)
}
obj = MultiFactorConstr(data,"groupe")

#' Vcramer.UnivFactor
#'
#' @param object an UnivFator object
#'
#' @return a matrice
#' @export
#'
#' @examples
Vcramer.MultiFactor <- function(object){
  nb_col <- ncol(object$data.qual)
  l<-c()
  for (i in 1:nb_col){
    colname <- colnames(object$data.qual)[i]
    tableau <- table(object$data[[colname]],object$data[[object$vargroupe]])
    nli <- nrow(tableau)
    nco <- ncol(tableau)
    khi2 = chisq.test(tableau)$statistic
    cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1)))))
    l <- c(l,cramer)

  }
  print(matrix(l,nrow=nb_col,ncol=1, dimnames = list(colnames(object$data.qual),"Cramer")))
  }

#' PhiValueTable.MultiFactor
#'
#' @param object a MultiFactor object
#' @param nomvarquali a string
#'
#' @return
#' @export
#'
#' @examples
PhiValueTable.MultiFactor <- function(object, nomvarquali){
  tableau <- table(object$data[[object$vargroupe]],object$data[[nomvarquali]])
  nli <- nrow(tableau)
  nco = ncol(tableau)
  eff = addmargins(tableau)
  pourc = addmargins(prop.table(addmargins(tableau,1),1),2)
  tab_phi <- tableau
  for (i in 1:nli){
    for (j in 1:nco){
      taille = eff[i,j]/eff[i,nco+1]
      liste <- c(eff[i,j],(eff[nli+1]-eff[i,j]), (eff[i,nco+1]-eff[i,j]),eff[nli+1,nco+1]-(eff[i,j]+(eff[nli+1]-eff[i,j])+(eff[i,nco+1]-eff[i,j])) )
      matri <- matrix(liste,2,2)
      khi2 <- chisq.test(matri)
      pla = (matri[2,1])/(matri[2,1]+matri[2,2])
      tab_phi[i,j] <- round(sign(taille-pla) * sqrt(khi2$statistic/eff[nli+1,nco+1]),4)
    }
  }
  print(tab_phi)
}


#' VisualisationAFC.MultiFactor
#'
#' @param object a object
#'
#' @return a graph
#' @export
#'
#' @examples
VisualisationAFC.MultiFactor <- function(object){
  res.mca <- MCA (object$data.qual,graph = FALSE)  #calcul de l'ACM, r???sultats mis dans une variable
  #print(res.mca) #Variable resultat contient tous les elements suivants

  fviz_mca_var(res.mca, repel = TRUE,col.var = "contrib",
               ggtheme= theme_minimal())
}
