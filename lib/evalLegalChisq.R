evalLegalChisq<-function(df){
  
  mat<-matrix(nrow=dim(df)[2], ncol=dim(df)[2])
  rownames(mat)<-colnames(df)
  colnames(mat)<-colnames(df)
  
  for(i in 1:(dim(mat)[2])){
    for (j in 1:dim(mat)[2]){
      cols_to_select=names(df)[c(i, j)]
      t<-df %>%
        select(one_of(cols_to_select)) %>%
        na.omit %>%
        group_by_(cols_to_select[1]) %>%
        summarize(count=n())
      
      if (nrow(t)<2){
        print("One of the features has only 1 level.")
        mat[i,j]<-1
      } else{
        mat[i,j]<-0
      }
    }
  }
  
  return(mat)
}