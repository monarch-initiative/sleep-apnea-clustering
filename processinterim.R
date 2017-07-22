df.interim<-interim.pruned %>% select(-starts_with("diastolic")) %>%
  select(-starts_with("systolic")) %>%
  select(-wtprotocol, -bptime, -bpprotocol, -pptstatus) 

terms_yn_interim<-getYNterms(datadict, df.interim)
df.interim.cont<-removeCols(df.interim, terms_yn_interim)
df.interim.cont<-df.interim.cont %>% select(which(colMeans(is.na(.)) < 0.5))

summary(df.interim.cont)
corMat<-corrNASpear(df.interim.cont %>% select(-obf_pptid, -gender, -race))
plotMeltedCorr(corMat, "cont")

melted<-melt(cvmat)
melteddesc<-melted %>%
  arrange(desc(value)) %>%
  filter(abs(value)>.10) %>%
  filter(Var1 != Var2)

sampsize=nrow(df.interim.cont)
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring", 
                     threshold = "bonferroni",
                     sampleSize = sampsize, 
                     alpha = 0.05)

Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.25,
                       sampleSize = sampsize, labels=colnames(corMat))

df.interim.cat<-subsetCols(df.interim, terms_yn_interim)
df.interim.cat<-df.interim.cat %>% select(which(colMeans(is.na(.)) < 0.5))
cvmat<-catCVmat(df.interim.cat)
plotMeltedCorr(cvmat, "cat")

df.interim.cat %>%
  mutate_each(funs(factor)) %>%
  summary

library("bootnet")

Data_binary <- as.matrix(na.omit(df.interim.cat))
for (i in 1:ncol(Data_binary)){
  Data_binary[,i] <- 1 * (Data_binary[,i] > median(Data_binary[,i]))
}

library("IsingSampler")
library(qgraph)
Res <- EstimateIsing(as.matrix(Data_binary), method = "uni")
Graph_Ising1 <- qgraph(Res$graph, layout = "spring", 
                       labels = colnames(Data_binary))

library("IsingFit")
Res <- IsingFit(as.matrix(Data_binary),gamma = 0.25, plot=FALSE)
Graph_Ising2 <- qgraph(Res$weiadj, layout = "spring")

catnames<-colnames(df.interim.cat)
catnames<-c("gender", "race", catnames)
contnames<-colnames(df.interim.cont %>% select(-obf_pptid, -gender, -race))
anovaMat<-etaAnovaMat(df.interim, catnames, contnames)
plotMeltedCorr(anovaMat, "cat")

library(polycor)
hetcor(df.anthro.cat) 

library(ggm)
pcor(c(1, 2), df.anthro.cat %>% select(-obf_pptid) %>% na.omit)