removeCorrelated<-function(df){
  resp_corr<-corrNASpear(df %>% select (-obf_pptid))
  melted<-melt(resp_corr)
  
  melteddesc<-melted %>%
    arrange(desc(value)) %>%
    filter(abs(value)>.90) %>%
    filter(Var1 != Var2)
  
  #oahi
  #rdi3p
  terms_to_remove<-unique(pull(melteddesc, Var1))
  terms_to_remove1<-sapply(terms_to_remove, as.character)
  correlated<-df %>%
    select(one_of(terms_to_remove1))
  
  nas<-sapply(correlated, na_count)
  data.frame(nas)
  
  
  correlated.nona<-removeCols(correlated, names(which(nas>0)))
  
  corrdf<-sapply(correlated.nona, IQR, na.rm=TRUE)
  
  rangedf<-sapply(correlated.nona, range, na.rm=TRUE)
  variation<-corrdf/(rangedf[2, ]-rangedf[1, ])
  
  keeplist<-c(names(which.max(variation)), 
              names(which.max(rangedf[2,]-rangedf[1,])), 
              names(which.max(corrdf)))
  setdiff(terms_to_remove1, keeplist)
  df.nocorr<-removeCols(df, setdiff(terms_to_remove1, keeplist))
  
  return(df.nocorr)
}
