cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  #p = chisq.test(x, y, correct=FALSE)$p.value
  print.noquote("Cramér V / Phi:")
  #CVlist=c(as.numeric(CV), as.numeric(p))
  return(as.numeric(CV))
}