# ------------------------------------------------------------------------- #
# CLUSTCHECK
# Functions for categorical variables (univariate)
# ------------------------------------------------------------------------- #
#' Contingency table
#'
#' Create a cross table between a cluster variable and a selected categorical variable.
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active categorical variable
#'
#' @return A cross table between two variables.
#' The same table with margins and sum
#' The table with percentages by lines
#' Number of lines (i.e. number of clusters)
#' Number of columns (i.e. number of modes in the other variables)
#'
#' @importFrom stats addmargins
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' table <- contingency(obj, BankCustomer$profession)
#' effectiveTable <- table[[2]]
#' @export
contingency <- function(object, var){
  #creation of the cross table between the cluster data and an other categorical variable
  tableau <- table(object$pred_clusters,var)
  nli <- nrow(tableau) #number of cluster
  nco <- ncol(tableau) #number of modality in the other categorical variable
  eff <- addmargins(tableau) #to add Sum and names of the modes
  pourc <- addmargins(prop.table(addmargins(tableau,1),1),2) #prop.table to have the percentage by lines
  return(list(tableau, eff, pourc, nco, nli))
}
# ------------------------------------------------------------------------- #
# Cramer'V table
# ------------------------------------------------------------------------- #
#' Cramer Values
#'
#' Calulates the cramer's V values between two categorical variables
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active categorical variable
#'
#' @return table of Cramer'V values for all categorical variables or only selected one
#'
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' vcramer(obj) #for all the cramer value by variables
#' vcramer(obj, BankCustomer$profession) #For one categorical variable
vcramer <- function(object, var = FALSE){
  if(is.null(object$cat_p) == FALSE){
    nameVar <- deparse(substitute(var)) #to get the name of the vector
    if(nameVar == FALSE){ #if we want all the cramer values for all the categorical variables
      l<-c() #a list to the futur cramer values
      for (i in object$cat_var_names){
        table <- contingency(object, object$all_data[[i]]) #call the CalcTable function to get the cross table
        tableau <- table[[1]]
        nli <- table[[5]]
        nco <- table[[4]]
        khi2 = chisq.test(tableau, simulate.p.value = TRUE)$statistic #we use chisq.test to get the khi2 statistic
        cramer = sqrt((khi2)/(nrow(object$all_data)*(min((nco-1),(nli-1))))) #calcul of Vcramer
        l <- c(l,cramer) #put the cramer value into the list
      }
      #use of a matrix to print the variable name and the Vcramer
      matrice = matrix(l,nrow=object$cat_p,ncol=1, dimnames = list(object$cat_var_names,"Cramer"))
      return(matrice)
      #listemax <- c()
      #ind = head(sort(matrice[,1], decreasing = TRUE), 3)
      #listemax <- c(listemax, ind)
      #print(listemax)
      #rownames(matrice)[ind]
    }else{
      table <- contingency(object, var) #call the CalcTable function to get cross table
      tableau <- table[[1]]
      nli <- table[[5]]
      nco <- table[[4]]
      khi2 = chisq.test(tableau, simulate.p.value = TRUE)$statistic #same things for the khi2 value
      cramer = sqrt((khi2)/(nrow(object$all_data)*(min((nco-1),(nli-1)))))
      cat("Cramer value between the cluster vector and  ",deparse(substitute(var))," = ", cramer) #we only print a clause
    }
  }
  else {
    cat("Error : Cramer's V calculations are for categorical variables only.")
  }
}
# ------------------------------------------------------------------------- #
# Phi's values table
# ------------------------------------------------------------------------- #
#' Value Phi
#'
#'Calculate the Phi value for all cluster*mode of one selected variable.
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active categorical variable
#'
#' @return A cross table between the cluster variable and the selected variable with phi values for all cluster*mode pairs.
#' @export
#' @importFrom stats addmargins chisq.test
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' phivalue(obj, BankCustomer$profession)
phivalue <- function(object, var) {
  if (object$vartype == "CAT" | object$vartype == "MIX") {
    if (is.factor(var) == TRUE | is.character(var) == TRUE) {
      table <-
        contingency(object, var) #call the CalcTable to get cross table between the two variables
      tableau <- table[[1]]
      nli <- table[[5]]
      nco <-
        table[[4]] #we get all the information (effective table, nli, nco and the line percentage)
      eff = table[[2]]
      pourc = table[[3]]
      tab_phi <-
        tableau #creation of the same table where we will put the news informations
      for (i in 1:nli) {
        for (j in 1:nco) {
          plg = eff[i, j] / eff[i, nco + 1] #number of sample in the i cluster and the j modality (of the other variable) / number of total sample in the i cluster
          #creation of a i*j matrix : matrix to cross all the cluster group by all the modes -> use to calculate the pla (proportion in the others groups)
          liste <-
            c(eff[i, j], (eff[nli + 1, j] - eff[i, j]), (eff[i, nco + 1] - eff[i, j]), eff[nli +
                                                                                             1, nco + 1] - (eff[i, j] + (eff[nli + 1] - eff[i, j]) + (eff[i, nco + 1] -
                                                                                                                                                        eff[i, j])))
          matri <- matrix(liste, 2, 2)
          #for all the group*modalitiy we calcule the Khi2
          suppressWarnings(khi2 <-
                             chisq.test(matri)) #remove the warnings wich tell us that n is to small
          pla = (matri[2, 1]) / (matri[2, 1] + matri[2, 2]) #pla calculate for all the group*modes (important to see the proportion of the modality in the others cluster group)
          tab_phi[i, j] <-
            round(sign(plg - pla) * sqrt(khi2$statistic / eff[nli + 1, nco + 1]), 4) #get all the phi values in one table
          #get the signed phi value with (plg-pla)
        }
      }
      return(tab_phi)
    } else{
      stop("Error : var must be a categorical variable")
    }
  } else {
    stop("Error : There are no categorical variable in your dataset.")
  }
}
# ------------------------------------------------------------------------- #
# T-values categorical table
# ------------------------------------------------------------------------- #
#' t-value for categorical variables
#'
#'Calculates the test values for all cluster*modes of one selected variable.
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active categorical variable
#'
#' @return A cross table between the cluster variable and the selected variable with test values for all cluster*modality pairs.
#' @export
#'
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' tvalue_cat(obj, BankCustomer$profession)
tvalue_cat <- function(object, var) {
  if (is.factor(var) == TRUE | is.character(var) == TRUE) {
    table <-
      contingency(object, var) #call the CalcTable to get the info of the cross table
    tableau <- table[[1]]
    nli <- table[[5]]
    nco <- table[[4]]
    eff = table[[2]]
    pourc = table[[3]]
    tab_vtest <- tableau #creation of a table to put the futur values
    for (i in 1:nli) {
      for (j in 1:nco) {
        #for all the group and all the modes
        v = (sqrt(eff[nli + 1, j])) * ((pourc[i, j] - pourc[nli + 1, j]) /
                                         (sqrt(((eff[nli + 1, nco + 1] - eff[nli + 1, j]) / (eff[nli + 1, nco + 1] - 1)
                                         ) * pourc[nli + 1, j] * (1 - pourc[nli + 1, j]))))
        #calcul of v
        tab_vtest[i, j] <- v
      }
    }
    #print("ci dessous tableau des valeurs tests")
    #print(tab_vtest)
    return(tab_vtest)
  } else{
    stop("Error : var must be a categorical variable")
  }
}
# ------------------------------------------------------------------------- #
# AFC
# ------------------------------------------------------------------------- #
#' Correspondence Analysis visualisation
#' (two graphs to illustrate the cluster with a specific categorical selected variable)
#'
#' @param object An object of class ccdata
#' @param var A data vector of an active variable
#'
#' @return A stack barplot with cluster in abscissa and modes in stack and a CA factor map which compare the cluster class and the modes of the selected variable.
#'
#' @export
#' @import questionr
#' @import FactoMineR
#' @import factoextra
#' @import ggplot2
#' @import dplyr
#' @importFrom graphics  barplot mosaicplot
#' @importFrom stats addmargins chisq.test
#' @importFrom utils data stack
#' @importFrom graphics boxplot
#' @import ggpubr
#' @examples
#' data(BankCustomer)
#' obj <- Dataset(BankCustomer, BankCustomer$Cluster)
#' vizAFC(obj, BankCustomer$profession)
vizAFC <- function(object, var) {
  if(object$vartype=="CAT" | object$vartype == "MIX"){
    if(is.factor(var) == TRUE|is.character(var) == TRUE){
    tableau <- table(object$pred_clusters, var)
    questionr::lprop(tableau, digits = 1)
    questionr::cprop(tableau, digits = 2)
    mydf <- as.data.frame(tableau)
    plotbar <-
      ggplot2::ggplot(mydf, aes(fill = var, y = Freq, x = Var1)) + geom_bar(position =
                                                                              "stack", stat = "identity")
    if (length(unique(var)) > 2) {
    res.ca <- FactoMineR::CA(tableau, graph = FALSE)
    biplot <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE)
    plt <-
      ggpubr::ggarrange(
        plotbar,
        biplot,
        labels = c('b', 'a'),
        widths = c(1, 2),
        ncol = 2,
        nrow = 1
      )
      print(plt)
    } else{
      print(plotbar)
    }
    }else{
      stop("Error : var must be a categorical variable")
    }
  }
  else {
    cat("Error : AFC is intended to categorical variables.")
  }

}
