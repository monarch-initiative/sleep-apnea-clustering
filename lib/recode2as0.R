recode2as0<-function(x){
  if (is.na(x)){
    x=NA
  } else
    if (x==2){
      x=0
    }
  return (x)
}