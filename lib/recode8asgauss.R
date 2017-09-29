recode8asGauss<-function(x){
  if (is.na(x)){
    y=NA
  } else
    if (x==8){
      y <- sample(1:4, 1)
    } else
      y=x
    return (y)
}