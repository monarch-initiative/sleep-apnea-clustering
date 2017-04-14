source("lib/lookUpDomain.R")

getYNterms <-function (datadict, df){
  noyes.tab<-lookUpDomain(datadict, "yes")
  check.tab<-lookUpDomain(datadict, "check")
  terms_noyes<-pullTerms(noyes.tab, df)
  terms_noyes<-c(terms_noyes)
  #check if there is a check.tab
  terms_check<-pullTerms(check.tab, df)
  terms_yn<-c(terms_noyes, terms_check)
  
  return(terms_yn)
}

