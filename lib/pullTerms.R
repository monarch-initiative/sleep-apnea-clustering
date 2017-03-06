pullTerms <-function(category_tibble, dataset){
  
  
  #This function serves as a way to translate data dictionary row filters
  #into shhs dataset column selections
  #Requirements: tidyverse, dplyr
  #Input: category_tibble is output of filter operation on data dictionary (rows
  #fitting conditions, e.g. all rows w/Category="Administrative" and Description
  #containing "Heart Rate"), dataset to be operated on (e.g. SHHS1, SHHS2, etc.)
  #Output: list of strings corresponding to column names in dataset
  terms_to_remove<-pull(category_tibble, id)
  pulled_cols<-filter(category_tibble, assertthat::has_name(dataset, terms_to_remove)) %>%
    select (id) %>% 
    collect %>% 
    .[["id"]]
  #assertthat::has_name makes sure terms_to_remove are actually in dataset
  
  return (pulled_cols)
}