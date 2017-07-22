cvd.pruned %>% 
  mutate_each(funs(as.numeric)) %>%
  
  summary
#vital, any_chd, any_cvd

terms_cvd_yn<-getYNterms(datadict, cvd.pruned)
terms_cvd_cat<-c("gender", "race", "vital", terms_cvd_yn)
cvd.cont<-removeCols(cvd.pruned, terms_cvd_cat)
cvd.cont.mut<-cvd.cont %>% 
  mutate_each(funs(as.numeric)) %>% 
  select(which(colMeans(is.na(.)) < 0.5)) %>%
  summary
corMat<-corrNASpear(cvd.cont.mut %>% select(-obf_pptid, -pptid))
plotMeltedCorr(corMat, "cont")

melted<-melt(corMat)
melteddesc<-melted %>%
  arrange(desc(value)) %>%
  filter(abs(value)>.80) %>%
  filter(Var1 != Var2)

cvd.cat<-subsetCols(cvd.pruned, terms_cvd_cat)
cvd.cat.clean<-cvd.cat %>%
  mutate_each(funs(factor)) %>% 
  select(-mi_death)
summary(cvd.cat)

corMat<-catCVmat(cvd.cat.clean)
plotMeltedCorr(anovaresults, "cat")

source("lib/etaAnovaMat.R")
contnames<-cvd.cont.mut %>% select(-obf_pptid, -pptid) %>% colnames
cvd.cat.clean %>% colnames
catnames<-c("gender", "race", "any_chd", "any_cvd")
anovaresults<-etaAnovaMat(cvd.pruned, catnames, contnames)