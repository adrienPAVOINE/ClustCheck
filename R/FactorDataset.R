#' CalcTable.Data
#'
#' @param object a object
#' @param varqual the other variable to cross
#'
#' @return
#' @export
#' @importFrom stats addmargins
#' @examples
CalcTable.Data <- function(object, varqual){
  tableau <- table(object$clusters_data,varqual)
  nli <- nrow(tableau)
  nco <- ncol(tableau)
  eff <- addmargins(tableau)
  pourc <- addmargins(prop.table(addmargins(tableau,1),1),2)
  return(list(tableau, eff, pourc, nco, nli))
}


#' Vcramer.Data
#'
#' @param object a object
#' @param var if you just want the Cramer for one varible
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
Vcramer.Data <- function(object, var = FALSE){
  nameVar <- deparse(substitute(var))
  if(nameVar == FALSE){
    l<-c()
    for (i in object$var.qual.names){
      tableau <- table(object$clusters_data,object$data[[i]])
      nli <- nrow(tableau)
      nco <- ncol(tableau)
      khi2 = chisq.test(tableau)$statistic
      cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1)))))
      l <- c(l,cramer)
    }
    matrice = matrix(l,nrow=object$p.qual,ncol=1, dimnames = list(colnames(object$data.qual),"Cramer"))
    #print(matrice)
    return(matrice)
    #listemax <- c()
    #ind = head(sort(matrice[,1], decreasing = TRUE), 3)
    #listemax <- c(listemax, ind)
    #print(listemax)
    #rownames(matrice)[ind]
  }else{
    table <- CalcTable.Data(object, var)
    tableau <- table[[1]]
    nli <- table[[5]]
    nco <- table[[4]]
    khi2 = chisq.test(tableau)$statistic
    cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1)))))
    cat("cramer entre la var groupe",deparse(substitute(var))," et  = ", cramer)

  }

}
#' PhiValueTable.Data
#'
#' @param object a object
#' @param nomvarqual a string
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
PhiValueTable.Data <- function(object, nomvarqual){
  table <- CalcTable.Data(object, nomvarqual)
  tableau <- table[[1]]
  nli <- table[[5]]
  nco <- table[[4]]
  eff = table[[2]]
  pourc = table[[3]]
  tab_phi <- tableau
  for (i in 1:nli){
    for (j in 1:nco){
      taille = eff[i,j]/eff[i,nco+1]
      liste <- c(eff[i,j],(eff[nli+1,j]-eff[i,j]), (eff[i,nco+1]-eff[i,j]),eff[nli+1,nco+1]-(eff[i,j]+(eff[nli+1]-eff[i,j])+(eff[i,nco+1]-eff[i,j])) )
      matri <- matrix(liste,2,2)
      suppressWarnings(khi2 <- chisq.test(matri))
      pla = (matri[2,1])/(matri[2,1]+matri[2,2])
      tab_phi[i,j] <- round(sign(taille-pla) * sqrt(khi2$statistic/eff[nli+1,nco+1]),4)
    }
  }
  #print(tab_phi)
  return(tab_phi)
}



#' TValueTable.Data
#'
#' @param object a data object
#' @param nomvarqual string of the variable
#'
#' @return
#' @export
#'
#' @examples
TValueTable.Data <-function(object, nomvarqual){
  table <- CalcTable.Data(object, nomvarqual)
  tableau <- table[[1]]
  nli <- table[[5]]
  nco <- table[[4]]
  eff = table[[2]]
  pourc = table[[3]]
  tab_vtest <- tableau
  for (i in 1:nli){
    for (j in 1:nco){
      v = (sqrt(eff[nli+1,j]))*((pourc[i,j] - pourc[nli+1,j])/(sqrt(((eff[nli+1,nco+1]-eff[nli+1,j])/(eff[nli+1,nco+1] - 1))*pourc[nli+1,j]* (1-pourc[nli+1,j]))))
      tab_vtest[i,j] <- v
    }
  }
  print("ci dessous tableau des valeurs tests")
  #print(tab_vtest)
  return(tab_vtest)

}
#' VisualisationACM.Data
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
VisualisationACM.Data <- function(object){
  res.mca <- FactoMineR::MCA(object$all.var.qual ,graph = FALSE)
  factoextra::fviz_mca_var(res.mca, repel = TRUE,col.var = "contrib")
}

#' VisualisationAC.Data
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
#' @import dplyr
#' @importFrom graphics  barplot mosaicplot
#' @importFrom stats addmargins chisq.test
#' @examples
VisualisationAC.Data <-function(object, nomvarqual){
  tableau <- table(object$clusters_data,nomvarqual)
  questionr::lprop(tableau, digits=1)
  questionr::cprop(tableau, digits=2)
  mydf <- as.data.frame(tableau)
  ggplot2::ggplot(mydf, ggplot2::aes(fill=nomvarqual, y=Freq, x=Var1)) + ggplot2::geom_bar(position="stack", stat="identity")
  #data$groupe = factor(data$groupe)
  #ggplot2::ggplot(data = data) + geom_mosaic(aes(x = product(groupe, région), fill=groupe), na.rm=TRUE) +
   # labs(x = "Is it rude recline? ", title='f(DoYouRecline | RudeToRecline) f(RudeToRecline)')
  res.ca <- FactoMineR::CA(tableau, graph = TRUE)
}

