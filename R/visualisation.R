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
Visualisation.UnivFactor <-function(object){
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
