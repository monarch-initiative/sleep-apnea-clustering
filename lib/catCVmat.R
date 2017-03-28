library(gmodels)
source("lib/cvtest.R")
catCVmat <- function (df){
  if ("obf_pptid" %in% colnames(df)){
    mat=df %>%
      select(-obf_pptid) %>%
      as.matrix()
  } else {
    mat <- df %>%
      as.matrix()
  }
  
  cvmat<-matrix(nrow=dim(mat)[2], ncol=dim(mat)[2])
  rownames(cvmat)<-colnames(mat)
  colnames(cvmat)<-colnames(mat)
  
  for (i in 1:(dim(mat)[2]-1)){
    for (j in 2:dim(mat)[2]){
      if (i<j){
        cola=mat[,i]
        colb=mat[,j]
        
        cols_to_select=names(df)[c(i, j)]
        t<-df %>%
          select(one_of(cols_to_select)) %>%
          na.omit %>%
          group_by_(cols_to_select[1]) %>%
          summarize(count=n())
        s<-df %>%
          select(one_of(cols_to_select)) %>%
          na.omit %>%
          group_by_(cols_to_select[2]) %>%
          summarize(count=n())
        
        if (nrow(t)<2 | nrow(s)<2){
          print("One of the features has only 1 level.")
          cvmat[i,j]<-NA
        } else{
          cv<-cv.test(cola, colb)
          cvmat[i,j]<-cv
        }
        
        #out<- capture.output(CrossTable(cola, colb, chisq=TRUE))
        #cat(colnames(mat)[i], " x ", colnames(mat)[j], out,
        #    file=filename, sep="\n", append=TRUE)
      }
    }
  }
  
  return(cvmat)
}