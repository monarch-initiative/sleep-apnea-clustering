library(gmodels)

catCrossTabs <- function (df, filename){
  if ("obf_pptid" %in% colnames(df)){
    mat=df %>%
      select(-obf_pptid) %>%
      as.matrix()
  } else {
    mat <- df %>%
      as.matrix()
  }
  
  
  for (i in 1:(dim(mat)[2]-1)){
    for (j in 2:dim(mat)[2]){
      if (i<j){
        cola=mat[,i]
        colb=mat[,j]
        out<- capture.output(CrossTable(cola, colb, chisq=TRUE))
        cat(colnames(mat)[i], " x ", colnames(mat)[j], out,
            file=filename, sep="\n", append=TRUE)
      }
    }
  }
  return()
}