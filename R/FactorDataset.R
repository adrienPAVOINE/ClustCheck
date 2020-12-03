#' CalcTable.Data
#'
#' @param object a Dataset object
#' @param varqual the other variable to cross
#'
#' @return
#' @export
#' @importFrom stats addmargins
#' @examples
CalcTable.Data <- function(object, varqual){
  #creation of the cross table between the cluster data and an other category variable
  tableau <- table(object$clusters_data,varqual)
  nli <- nrow(tableau) #number of cluster
  nco <- ncol(tableau) #number of modality in the other category variable
  eff <- addmargins(tableau) #to add Sum and names of the modalities
  pourc <- addmargins(prop.table(addmargins(tableau,1),1),2) #prop.table to have the percentage by lines
  return(list(tableau, eff, pourc, nco, nli))
}
#' Vcramer.Data
#'
#' @param object a dataset object
#' @param var If you want the cramer value of an unique category variable
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
Vcramer.Data <- function(object, var = FALSE){
  nameVar <- deparse(substitute(var)) #to get the name of the vector
  if(nameVar == FALSE){ #if we want all the cramer values for all the category variables
    l<-c() #a list to the futur cramer values
    for (i in object$var.qual.names){
      table <- CalcTable.Data(object, object$data[[i]]) #call the CalcTable function to get the cross table
      tableau <- table[[1]]
      nli <- table[[5]]
      nco <- table[[4]]
      khi2 = chisq.test(tableau, simulate.p.value = TRUE)$statistic #we use chisq.test to get the khi2 statistic
      cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1))))) #calcul of Vcramer
      l <- c(l,cramer) #put the cramer value into the list
    }
    #use of a matrix to print the variable name and the Vcramer
    matrice = matrix(l,nrow=object$p.qual,ncol=1, dimnames = list(colnames(object$data.qual),"Cramer"))
    #print(matrice)
    return(matrice)
    #listemax <- c()
    #ind = head(sort(matrice[,1], decreasing = TRUE), 3)
    #listemax <- c(listemax, ind)
    #print(listemax)
    #rownames(matrice)[ind]
  }else{
    table <- CalcTable.Data(object, var) #call the CalcTable function to get cross table
    tableau <- table[[1]]
    nli <- table[[5]]
    nco <- table[[4]]
    khi2 = chisq.test(tableau)$statistic #same things for the khi2 value
    cramer = sqrt((khi2)/(nrow(object$data)*(min((nco-1),(nli-1)))))
    cat("cramer entre la var groupe",deparse(substitute(var))," et  = ", cramer)#we only print a clause

  }

}
#' PhiValueTable.Data
#'
#' @param object a dataset object
#' @param nomvarqual vector of a category variable
#'
#' @return
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
PhiValueTable.Data <- function(object, nomvarqual){
  table <- CalcTable.Data(object, nomvarqual) #call the CalcTable to get cross table between the two variables
  tableau <- table[[1]]
  nli <- table[[5]]
  nco <- table[[4]] #we get all the information (effective table, nli, nco and the line percentage)
  eff = table[[2]]
  pourc = table[[3]]
  tab_phi <- tableau #creation of the same table where we will put the news informations
  for (i in 1:nli){
    for (j in 1:nco){
      plg = eff[i,j]/eff[i,nco+1] #number of sample in the i cluster and the j modality (of the other variable) / number of total sample in the i cluster
      #creation of a i*j matrix : matrix to cross all the cluster group by all the modalities -> use to calculate the pla (proportion in the others groups)
      liste <- c(eff[i,j],(eff[nli+1,j]-eff[i,j]), (eff[i,nco+1]-eff[i,j]),eff[nli+1,nco+1]-(eff[i,j]+(eff[nli+1]-eff[i,j])+(eff[i,nco+1]-eff[i,j])) )
      matri <- matrix(liste,2,2)
      #for all the group*modalitiy we calcule the Khi2
      suppressWarnings(khi2 <- chisq.test(matri)) #remove the warnings wich tell us that n is to small
      pla = (matri[2,1])/(matri[2,1]+matri[2,2]) #pla calculate for all the group*modalities (important to see the proportion of the modality in the others cluster group)
      tab_phi[i,j] <- round(sign(plg-pla) * sqrt(khi2$statistic/eff[nli+1,nco+1]),4) #get all the phi values in one table
      #get the signed phi value with (plg-pla)
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
  table <- CalcTable.Data(object, nomvarqual) #call the CalcTable to get the info of the cross table
  tableau <- table[[1]]
  nli <- table[[5]]
  nco <- table[[4]]
  eff = table[[2]]
  pourc = table[[3]]
  tab_vtest <- tableau #creation of a table to put the futur values
  for (i in 1:nli){
    for (j in 1:nco){
      #for all the group and all the modalities
      v = (sqrt(eff[nli+1,j]))*((pourc[i,j] - pourc[nli+1,j])/(sqrt(((eff[nli+1,nco+1]-eff[nli+1,j])/(eff[nli+1,nco+1] - 1))*pourc[nli+1,j]* (1-pourc[nli+1,j]))))
      #calcul of v
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
#' @importFrom utils data stack
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
