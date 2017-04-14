etaAnovaMat<-function (df, cat_terms, cont_terms){
  #this function takes a data frame,
  #list of categorical terms
  #list of continuous terms
  #returns eta value of ANOVA, which is analogous to R value of correlation
  
  mat<-matrix(nrow=length(cat_terms), ncol=length(cont_terms))
  rownames(mat)<-cat_terms
  colnames(mat)<-cont_terms
  for (i in 1:length(cat_terms)){
    for (j in 1:length(cont_terms)){
      model <- lm(paste(cont_terms[j], "~", cat_terms[i]), data=df)
      rsq<-summary(model)$r.squared
      mat[i,j]<-sqrt(rsq)
      #note that this is linear regression, but it is equivalent to ANOVA
      #and r = eta
    }
  }
  return (mat)
}