#' UnivConst
#'
#' @param data a data frame
#' @param varqual1 a name variable
#' @param vargroupe a name variable
#'
#' @return a matrice
#' @export
#' @importFrom stats addmargins chisq.test
#' @import questionr
#' @examples


UnivConst <- function(data,varqual1,vargroupe){
  #contr?le - data.frame
  ok <- is.data.frame(data)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  instance <- list()
  instance$data = data
  instance$tableau <- table(data[[vargroupe]],data[[varqual1]])
  instance$nli <- nrow(instance$tableau)
  instance$nco = ncol(instance$tableau)
  instance$eff = addmargins(instance$tableau)
  instance$pourc = addmargins(prop.table(addmargins(instance$tableau,1),1),2)
  class(instance) <- "UnivFactor"
  return(instance)
}

#' Vcramer.UnivFactor
#'
#' @param object an UnivFator object
#'
#' @return
#' @export
#'
#' @examples
Vcramer.UnivFactor <- function(object){
  khi2 = chisq.test(object$tableau)$statistic
  khi2
  cramer = sqrt((khi2)/(nrow(object$data)*(min((object$nco-1),(object$nli-1)))))
  cramer
}



#' TValueTable
#'
#' @param object a UnivFactor object
#'
#' @return a matrice
#' @export
#'
#' @examples
#'
TValueTable.UnivFactor <-function(object){
  tab_vtest <- object$tableau
  for (i in 1:object$nli){
    for (j in 1:object$nco){
      v = (sqrt(object$eff[object$nli+1,j]))*((object$pourc[i,j] - object$pourc[object$nli+1,j])/(sqrt(((object$eff[object$nli+1,object$nco+1]-object$eff[object$nli+1,j])/(object$eff[object$nli+1,object$nco+1] - 1))*object$pourc[object$nli+1,j]* (1-object$pourc[object$nli+1,j]))))
      tab_vtest[i,j] <- v
    }
  }
  print("ci dessous tableau des valeurs tests")
  print(tab_vtest)
  tab <- as.data.frame.matrix(tab_vtest)
  # CONVERT TO DATA FRAME
  df <- transform(data.frame(tab), y=row.names(tab))
  # RESHAPE LONG
  long_df <- reshape(df, varying = colnames(tab), times = colnames(tab),
                     timevar="x", v.names="value", direction="long")
  # ORDER VALUES
  long_df <- with(long_df, long_df[order(value),])
  long_df$xy <- with(long_df, paste(x, y, sep=":"))
  # CONVERT TO MATRIX
  new_mat <- matrix(long_df$value, dimnames=list(long_df$xy, "value"))
  new_mat
  # BASE R barplot:
  barplot(new_mat, beside = TRUE, horiz = TRUE,
          main = "Horizontal Bar Graph Descending Order",
          names.arg = row.names(new_mat),
          las=1, col = rainbow(nrow(new_mat)))

}



#' Visualisation.UnivFactor
#'
#' @param object a UnivFactor object
#'
#' @return some plot
#' @export
#' @import questionr
#' @import FactoMineR
#' @import factoextra
#' @importFrom graphics  barplot mosaicplot
#' @importFrom stats addmargins chisq.test
#' @import ggplot2
#'
#' @examples
VisualisationACM.UnivFactor <-function(object){
  #r?alisation des profils ligne et colonne
  lprop(object$tableau, digits=1)#la distribution de la r?gion parmis ceux heureux || 4 profils lignes
  #ensemnle = profil moyen
  cprop(object$tableau, digits=2)
  #on s'uppose la d?pendance car les profils sont distincts
  # on test l'ind?pendance
  chisq = chisq.test(object$tableau)
  print(chisq)
  #aphiques
  colors <- c("chartreuse4", "chartreuse1", "orange","green")
  barplot(object$tableau, col=colors, main = "heureux par libert? sur internet", ylab="nombre ")
  mosaicplot(object$tableau, col = colors)
  res.ca <- CA(object$tableau, graph = TRUE)
}





