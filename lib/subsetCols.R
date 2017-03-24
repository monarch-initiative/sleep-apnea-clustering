subsetCols<-function(dataset, cols_to_subset){
  #Same as removeCols, but selecting FOR columns named in cols_to_subset
  #Requirements: dplyr, tidyverse
  #Input: dataset is tibble, cols_to_subset is list of strings (column names)
  #Output: dataset.cols.subsetted (tibble with only selected columns)
  
  #Error
  #wrong name for dataset, wrong name for cols_to_subset
  #
  term_isthere<-has_name(dataset, cols_to_subset)
  dataset.cols.subsetted<-dataset %>%
    dplyr::select(one_of(cols_to_subset[term_isthere]))
  
  return(dataset.cols.subsetted)
}