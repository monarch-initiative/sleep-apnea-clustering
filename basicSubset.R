basicSubset<-function(datadict, dataset, string){
  df <- lookUpFolderWithString(datadict, string)
  terms<-pullTerms(df, dataset)
  terms<-c("obf_pptid", terms)
  sub_data<-subsetCols(dataset, terms)
  return(sub_data)
}
