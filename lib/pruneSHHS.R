pruneSHHS<-function (dataset, datadict){
  #Function that removes unwanted columns from SHHS1 and SHHS2 datasets
  #Dependencies: tidyverse, stringr
  #Input: dataset (SHHS1 or SHHS2), datadict (SHHS data dictionary variables)
  #Output: SHHS dataset w/ cols removed
  
  shhs.lower<-colsToLower(dataset)
  df.admin<-datadict %>%
    filter(str_detect(folder, "Administrative"))
  terms_admin<-pullTerms(df.admin, shhs.lower)
  shhs.noadmin<-removeCols(shhs.lower, terms_admin)
  
  df.sq<-datadict %>%
    filter(str_detect(folder, "Signal Quality"))
  terms_nosq<-pullTerms(df.sq, shhs.noadmin)
  shhs.nosq<-removeCols(shhs.noadmin, terms_nosq)
  
  drop.cols<-c("pm217", "pm227", "ecgdate")
  dataset.pruned<-removeCols(shhs.nosq, drop.cols)
  return (dataset.pruned)
}