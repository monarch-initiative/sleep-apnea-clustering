corrNASpear<- function (df) {
  correlated<-df %>%
    na.omit %>%
    cor (method="spearman")
  
  return(correlated)
}