pull <- function(x,y) {
  #pull out all values from column y in df/tibble x as a list of strings
  #from stackexchange
  x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]
  #error checking
}
