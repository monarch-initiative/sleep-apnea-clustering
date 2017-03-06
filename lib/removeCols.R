removeCols<-function(dataset, cols_to_remove){
  #removes selected columns in given dataset
  #Requirements: dplyr, tidyverse
  #Input: dataset is tibble, cols_to_remove is list of strings (column names)
  #Output: dataset.cols.removed (tibble with columns removed)
  dataset.cols.removed<-dataset %>%
    select(-one_of(cols_to_remove))
  return (dataset.cols.removed)
}