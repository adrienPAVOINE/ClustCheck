# Corr_ratios
# data a data frame
# classes dataframe
# returns  matrix

Corr_ratios <- function(data,classes){
  if (! is.data.frame(data) | ! is.data.frame(classes) ){
    stop("Les données doivent être sous forme d'un data frame")
  }
  
  #SCT------------
  sct<- function(x){
    m<-mean(x)
    sct<-0
    for (i in 1:length(x)){#check if x or data
      sct<-(x[i]- m)**2 + sct
    }
    return(sct)
  }
  sct_l<-sapply(data,sct)
  
  #SCE-----------
  cl<-unique(classes[[1]])
  ng<-length(cl)
  sce<- function(x){
    m<-mean(x)
    sce<-0
    for (i in cl){
      ind_g<-which(classes==i)
      ng<-length(ind_g)
      sce<-sce + (ng*(mean(x[ind_g])-m)**2)
    }
    return(sce)
  }  
  sce_l<-sapply(data,sce)
  
  #SCR------------
  scr<- function(x){
    m<-mean(x)
    scr<-0
    for (i in cl){
      ind_g<-which(classes==i)
      ng<-length(ind_g)
      for (j in ind_g){
        scr<-scr+ (x[j]-mean(x[ind_g]))**2
      }
    }
    return(scr)
  }  
  scr_l<-sapply(data,scr)
  corr<- sce_l/sct_l
  
  instance<-list()
  instance$sct_l<-sct_l
  instance$sce_l<-sce_l
  instance$scr_l<-scr_l
  instance$corr<-corr
  return(instance)
}


# Corr_ratios
# data a data frame
# classes dataframe
# returns  matrix

Tvalue_table_fct <- function(data,classes){
  if (! is.data.frame(data) | ! is.data.frame(classes) ){
    stop("Les données doivent être sous forme d'un data frame")
  }
  n<-nrow(data)
  var_names<-colnames(data)
  len_col<-length(var_names)
  cl<-unique(classes[[1]])
  ng<-length(cl)
  tvalue_table<-matrix(nrow = length(var_names), ncol = ng,dimnames = list(colnames(data),cl))
  for (x_col in colnames(data)){
    x<-data[[x_col]]
    var_x<-var(x)
    m<-mean(x)
    for(i in cl){
      ind_g<-which(classes==i)
      ng<-length(ind_g)
      mean_g<-mean(ind_g)
      var_x<-var(x)
      vt_val<-(mean_g - m) / sqrt(((n-ng)*var_x)/((n-1)*ng))
      tvalue_table[x_col,i]<- vt_val
    }
  }
  return(tvalue_table)
}
