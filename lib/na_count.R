na_count <- function (y) {
  z<-sum(length(which(is.na(y))))
  return(z)
}