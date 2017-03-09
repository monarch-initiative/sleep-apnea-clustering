lookUpFolderWithString <- function (ddict, string){
  #This function will look at the column "folder" in ddict
  #and will return a tibble of all rows which contain the string
  #Dependencies: tidyverse, dplyr, stringr
  #Input: ddict (data dictionary csv), string (any string)
  #Output: df (tibble)
  
  df<-ddict %>%
    filter(str_detect(folder, string))
  #put in error catching here
  return(df)
}