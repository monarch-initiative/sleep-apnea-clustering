pruneSHHS<-function (dataset, datadict){
  #Function that removes unwanted columns from SHHS1 and SHHS2 datasets
  #Dependencies: tidyverse, stringr
  #Input: dataset (SHHS1 or SHHS2), datadict (SHHS data dictionary variables)
  #Output: SHHS dataset w/ cols removed
  
  shhs.lower<-colsToLower(dataset)
  df.admin <- lookUpFolderWithString(datadict, "Administrative")
  #df.admin<-datadict %>%
  #  filter(str_detect(folder, "Administrative"))
  terms_admin<-pullTerms(df.admin, shhs.lower)
  shhs.noadmin<-removeCols(shhs.lower, terms_admin)
  
  df.sq <- lookUpFolderWithString(datadict, "Signal Quality")
  terms_nosq<-pullTerms(df.sq, shhs.noadmin)
  shhs.nosq<-removeCols(shhs.noadmin, terms_nosq)
  
  df.specanal <- lookUpFolderWithString (datadict, "Spectral Analysis")
  terms_nospecanal<-pullTerms(df.specanal, shhs.nosq)
  shhs.nospecanal<-removeCols(shhs.nosq, terms_nospecanal)
  
  drop.cols<-c("pm217", "pm227", "ecgdate", "medalert", "lang15", "date10", 
               "date25", "date02", "formdate_hi", "hi217", "formdate_ms", 
               "formdate_ql", "formdate_sh")
  dataset.pruned<-removeCols(shhs.nosq, drop.cols)
  return (dataset.pruned)
}