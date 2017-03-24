lookUpDomain <- function (ddict, string){
  #This function will look at the column "folder" in ddict
  #and will return a tibble of all rows which contain the string
  #Dependencies: tidyverse, dplyr, stringr
  #Input: ddict (data dictionary csv), string (any string)
  #Output: df (tibble)
  
  df<-ddict %>%
    filter(str_detect(domain, string))
  #put in error catching here
   #string isn't recognized
   #string doesn't exist in ddict
  return(df)
}