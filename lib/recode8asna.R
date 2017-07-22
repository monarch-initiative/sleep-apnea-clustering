recode8asNA<-function(x){
  if (is.na(x)){
    y=NA
  } else
    if (x==8){
      y=NA
    } else
      y=x
    return (y)
}