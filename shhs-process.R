library(tidyverse)
library(stringr)

pull <- function(x,y) {
  x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
}

colsToLower <- function(df) {
  names(df) <- tolower(names(df))
  return(df)
}


pullTerms <-function(category_tibble, dataset){
  #if (is.name(substitute(cat_str))){
  #  cat=deparse(substitute(cat_str))
  #}
  #else{
  #    cat=cat_str
  #}
  
  terms_to_remove<-pull(category_tibble, id)
  pulled_cols<-filter(category_tibble, assertthat::has_name(dataset, terms_to_remove)) %>%
    select (id) %>% 
    collect %>% 
    .[["id"]]
  
  return (pulled_cols)
  }

removeCols<-function(dataset, cols_to_remove){
  dataset.cols.removed<-dataset %>%
    select(-one_of(cols_to_remove))
  return (dataset.cols.removed)
}

subsetCols<-function(dataset, cols_to_subset){
  dataset.cols.subsetted<-dataset %>%
    select(one_of(cols_to_subset))
  return(dataset.cols.subsetted)
}


#load data
setwd("~/Documents/MSthesis/datasets")
shhs1<-read_csv("shhs1-dataset-0.11.0.csv")
shhs2<-read_csv("shhs2-dataset-0.11.0.csv")

datadict<-read_csv("shhs-data-dictionary-0.11.0-variables.csv")

#process
pruneSHHS<-function (dataset, ddict){
  shhs.lower<-colsToLower(dataset)
  terms_admin<-pullTerms(ddict, shhs.lower, "Administrative")
  shhs.noadmin<-removeCols(shhs.lower, terms_admin)
  terms_nosq<-pullTermsToRemove(ddict, shhs.noadmin, "Signal Quality")
  shhs.nosq<-removeCols(shhs.noadmin, terms_nosq)
  
  drop.cols<-c("pm217", "pm227", "ecgdate")
  dataset.pruned<-removeCols(shhs.nosq, drop.cols)
  return (dataset.pruned)
}

shhs1.pruned<-pruneSHHS(shhs1, datadict)
shhs2.pruned<-pruneSHHS(shhs2, datadict)








