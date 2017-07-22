library(cluster)

recode2as0<-function(x){
  if (is.na(x)){
    x=NA
  } else
    if (x==2){
      x=0
    }
  return (x)
}

source("lib/recode8asna.R")

df<-interim.pruned %>% 
  
  select(-wtprotocol, -bpprotocol, -systolic1, -diastolic1, -systolic2, -diastolic2, 
         -freqothersnearby, -bptime, -race, -pptstatus, -yrsdrive, 
         -drive, -oxytherapy, -snoredever, -carotidend, -chf) %>%
  mutate(gender=unlist(lapply(gender, recode2as0))) %>%
  mutate(#snoredever=unlist(lapply(snoredever, recode8asNA)),
         treatedsnoring=unlist(lapply(treatedsnoring, recode8asNA)),
         stopbreathing=unlist(lapply(stopbreathing, recode8asNA)),
         sleepapnea=unlist(lapply(sleepapnea, recode8asNA)),
         sleepdisorder=unlist(lapply(sleepdisorder, recode8asNA)),
         bpmeds=unlist(lapply(bpmeds, recode8asNA))) %>% 
  select(which(colMeans(is.na(.)) < 0.5)) %>%
  mutate(mi_priorexam=as.factor(mi_priorexam), stroketia=as.factor(stroketia), 
         #chf=as.factor(chf), 
         cabgptca=as.factor(cabgptca), #carotidend=as.factor(carotidend),
         gender=as.factor(gender), #drive=as.factor(drive), 
         bpmeds=as.factor(bpmeds),
         sleepdisorder=as.factor(sleepdisorder), 
         sleepapnea=as.factor(sleepapnea),
         stopbreathing=as.factor(stopbreathing), treatedsnoring=as.factor(treatedsnoring)
         #snoredever=as.factor(snoredever), oxytherapy=as.factor(oxytherapy), 
         #carotidend=as.factor(carotidend)
         ) %>%
  select(-mi_priorexam, -cabgptca, -sleepdisorder, -obf_pptid) %>%
  na.omit %>%
  summary


gower_dist <- daisy(as.data.frame(df %>% select(-obf_pptid)),
                    metric = "gower", 
                    type=list(symm=c(18,31,38), asymm=c(3,4,5,17,19,20), 
                              ordratio=c(9,10,11,12,13,14,15,16, 21,22,23,24,25,26,
                                         27,28,29,30,32)))

hier=hclust(gower_dist, method="complete")

clusth<-cutree(hier, k=5)
df.wclust<-df %>%
  na.omit%>%
  mutate(cluster=clusth)

silw=rep_len(NA, 10)
for (i in 2:10){
  clusth<-cutree(hier, k=i)
  sil<-silhouette(clusth, gower_dist)
  silw[i]=mean(sil[,3])
}

plot(1:10, silw, main="Average silhouette width", xlab="number of clusters",
     ylab="silhouette width")

sil<-silhouette(clusth, gower_dist)
summary(sil)

source("lib/catCVmat.R")
source("lib/plotMeltedCorr.R")
df.wclust.cat<-df.wclust %>%
  select(mi_priorexam, stroketia, cabgptca, treatedsnoring, stopbreathing, sleepapnea,
         bpmeds, sleepdisorder,
         gender, cluster)

library(qgraph)
library("IsingSampler")
Data_binary <- as.matrix(df.wclust.cat %>% select(-cluster) %>%
                           mutate_each(funs(as.integer)))
for (i in 1:ncol(Data_binary)){
  Data_binary[,i] <- 1 * (Data_binary[,i] > median(Data_binary[,i]))
}

Res <- EstimateIsing(as.matrix(Data_binary), method = "uni")

categories.cat=list()
categories.cat$cardiac = c(1:3, 7)
categories.cat$sleep = c(4:6, 8)
categories.cat$gender = 9
Graph_Ising1 <- qgraph(Res$graph, layout = "spring", 
                       labels = colnames(Data_binary),
                       groups=categories.cat,
                       color=rainbow(length(categories.cat))
                       )


library("IsingFit")
Res <- IsingFit(as.matrix(Data_binary),gamma = 0.25, plot=FALSE)
Graph_Ising2 <- qgraph(Res$weiadj, layout = "spring", 
                       labels = colnames(Data_binary), groups=categories)

df.wclust.cont<-df.wclust %>%
  select(-mi_priorexam, -stroketia, -cabgptca, -treatedsnoring, -stopbreathing, -sleepapnea,
         -bpmeds, -obf_pptid, -sleepdisorder,
         -gender)

corMat <- cor_auto(df.wclust.cont %>% select(-cluster))
sampsize=nrow(df.wclust.cont)
Graph_pcor <- qgraph(corMat, graph = "pcor", layout = "spring", 
                     threshold = "bonferroni",
                     sampleSize = sampsize, 
                     alpha = 0.05, labels=colnames(corMat),
                     groups=categories,
                     color=rainbow(length(categories)))

categories<-list()
categories$BP<-c(1,2)
categories$sleep_habits <- c(3:10)
categories$snoring <- c(12:13)
categories$fall_asleep_while <-c(14:23)
categories$medication <- c(11,24)
categories$driving <- c(25:29)
categories$age <- 30
Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", tuning = 0.90,
                      groups=categories, 
                      sampleSize = sampsize, 
                      #labels=colnames(corMat),
                      color=rainbow(length(categories)))


source("lib/etaAnovaMat.R")
etaAnovaMat(df %>% select(-obf_pptid), [treatedsnoring, stopbreathing, sleepapnea,
            bpmeds, sleepdisorder,
            gender], )

data<-df.wclust.cat %>%
  select(-cabgptca, -mi_priorexam, -sleepdisorder, -stroketia, -treatedsnoring)

df.working<-df.wclust.cont %>%
  select(1:2, age_s1, unrested, notenoughsleep, stoppedcar, whiledriving, freqdrive, 12:20, cluster)

dend<-as.dendrogram(hier)
dend <- color_branches(dend, k=5)
branchcolors<-get_leaves_branches_col(dend)
plot(dend, main="Hierarchical clustering of interim dataset", ylab="height")
legend("topright", legend=c(1,2,3,4,5), fill=newbranchcolors)

newbranchcolors=c("#009232", "#CC476B", "#917600", "#008FB7", "#A352D1")
df.racediv<-interim.pruned %>%
  select(which(colMeans(is.na(.)) < 0.5)) %>%
  mutate(race=as.factor(race)) %>%
  group_by(race) %>%
  na.omit %>%
  summarize_each(funs(mean))
  
hc<-hetcor(as.data.frame(df %>% select(-obf_pptid, -mi_priorexam)))

library(corrplot)
corrplot(hc$correlations, order = "hclust", tl.col='black', tl.cex=.75) 