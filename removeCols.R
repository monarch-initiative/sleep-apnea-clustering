removeCols<-function(dataset, cols_to_remove){
  #removes selected columns in given dataset
  #Requirements: dplyr, tidyverse
  #Input: dataset is tibble, cols_to_remove is list of strings (column names)
  #Output: dataset.cols.removed (tibble with columns removed)
  
  term_isthere<-has_name(dataset, cols_to_remove)
  dataset.cols.removed<-dataset %>%
    
    select(-one_of(cols_to_remove[term_isthere]))
  return (dataset.cols.removed)
}