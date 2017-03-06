colsToLower <- function(df) {
  #renames all column names in df to lowercase
  names(df) <- tolower(names(df))
  return(df)
}