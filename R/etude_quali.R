#' Title
#'
#' @param data a data frame
#' @param varqual1 a name variable
#' @param vargroupe a name variable
#'
#' @return a matrice
#' @export
#' @importFrom stats addmargins chisq.test
#' @importFrom graphics  barplot mosaicplot
#' @import questionr
#' @import FactoMineR
#' @import factoextra
#' @examples

etude_quali <- function(data,varqual1, vargroupe){
  #contr?le - data.frame
  ok <- is.data.frame(data)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  tableau <- table(data[[vargroupe]],data[[varqual1]])
  nli = nrow(tableau)
  nco = ncol(tableau)
  eff = addmargins(tableau)
  pourc = addmargins(prop.table(addmargins(tableau,1),1),2)
  tab_vtest <- table(data[[vargroupe]],data[[varqual1]])
  for (i in 1:nli){
    for (j in 1:nco){
      v = (sqrt(eff[nli+1,j]))*((pourc[i,j] - pourc[nli+1,j])/(sqrt(((eff[nli+1,nco+1]-eff[nli+1,j])/(eff[nli+1,nco+1] - 1))*pourc[nli+1,j]* (1-pourc[nli+1,j]))))
      tab_vtest[i,j] <- v
    }
  }
  print("ci dessous tableau des valeurs tests")
  print(tab_vtest)

  tab_taille <- table(data[[vargroupe]],data[[varqual1]])
  for (i in 1:nli){
    for (j in 1:nco){
      taille = eff[i,j]/eff[i,nco+1]
      tab_taille[i,j] <- taille
    }

  }
  print(" ci dessous le tableau des tailles")
  print(tab_taille)
  #r?alisation des profils ligne et colonne
  lprop(tableau, digits=1)#la distribution de la r?gion parmis ceux heureux || 4 profils lignes
  #ensemnle = profil moyen
  cprop(tableau, digits=2)
  #on s'uppose la d?pendance car les profils sont distincts
  # on test l'ind?pendance
  chisq = chisq.test(tableau)
  print(chisq)
  #aphiques
  colors <- c("chartreuse4", "chartreuse1", "orange","green")
  barplot(tableau, col=colors, main = "heureux par libert? sur internet", ylab="nombre ")
  mosaicplot(tableau, col = colors)
  res.ca <- CA(tableau, graph = TRUE)
  #print(res.ca)

}

