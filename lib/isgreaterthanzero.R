is.greaterThanZero<-function(x){
  if (is.na(x)){
    y=NA
  } else
    if (x>0){
      y=1
    } else{
      y=0
    }
  return (y)
}