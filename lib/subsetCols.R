subsetCols<-function(dataset, cols_to_subset){
  #Same as removeCols, but selecting FOR columns named in cols_to_subset
  #Requirements: dplyr, tidyverse
  #Input: dataset is tibble, cols_to_subset is list of strings (column names)
  #Output: dataset.cols.subsetted (tibble with only selected columns)
  dataset.cols.subsetted<-dataset %>%
    dplyr::select(one_of(cols_to_subset))
  return(dataset.cols.subsetted)
}