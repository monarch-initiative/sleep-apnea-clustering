library(clValid)

getAvgSil<-function(clustering, gower_mat){
  sil_val<-silhouette(clustering, gower_mat)
  sum.sil<-summary(sil_val)
  avgsil<-sum.sil$avg.width
  
  return(avgsil)
}

dunn.index.h=c(NA)
dunn.index.hq=c(NA)
dunn.index.hm=c(NA)
dunn.index.hs=c(NA)
dunn.index.hw=c(NA)
dunn.index.ha=c(NA)

dunn.index.d=c(NA)
connie.h=c(NA)
connie.ha<-c(NA)
connie.hq<-c(NA)
connie.hm<-c(NA)
connie.hs<-c(NA)
connie.hw<-c(NA)
connie.d<-c(NA)

avgsil.h<-c(NA)
avgsil.d<-c(NA)
avgsil.ha<-c(NA)
avgsil.hq<-c(NA)
avgsil.hm<-c(NA)
avgsil.hs<-c(NA)
avgsil.hw<-c(NA)

for(i in 2:10){
  clusth<-cutree(hier, k=i)
  clustha<-cutree(hier.avg, k=i)
  clusthmc <-cutree(hier.mcq, k=i)
  clusthmed <-cutree(hier.med, k=i)
  clusthsing <-cutree(hier.sing, k=i)
  clusthw <- cutree(hier.ward, k=i)
  clustd<-cutree(dirty, k=i)
  dunn.index.h[i]=dunn(gower_mat, clusth)
  dunn.index.hq[i]=dunn(gower_mat, clusthmc)
  dunn.index.hm[i]=dunn(gower_mat, clusthmed)
  dunn.index.hs[i]=dunn(gower_mat, clusthsing)
  dunn.index.hw[i]=dunn(gower_mat, clusthw)
  dunn.index.ha[i]=dunn(gower_mat, clustha)
  
  dunn.index.d[i]=dunn(gower_mat, clustd)
  
  connie.h[i]<-connectivity(distance=gower_mat, clusth)
  connie.ha[i]<-connectivity(distance=gower_mat, clustha)
  connie.hq[i]<-connectivity(distance=gower_mat, clusthmc)
  connie.hm[i]<-connectivity(distance=gower_mat, clusthmed)
  connie.hs[i]<-connectivity(distance=gower_mat, clusthsing)
  connie.hw[i]<-connectivity(distance=gower_mat, clusthw)
  
  connie.d[i]<-connectivity(distance=gower_mat, clustd)
  
  avgsil.d[i]<-getAvgSil(clustd, gower_mat)
  avgsil.h[i]<-getAvgSil(clusth, gower_mat)
  avgsil.ha[i]<-getAvgSil(clustha, gower_mat)
  avgsil.hq[i]<-getAvgSil(clusthmc, gower_mat)
  avgsil.hm[i]<-getAvgSil(clusthmed, gower_mat)
  avgsil.hs[i]<-getAvgSil(clusthsing, gower_mat)
  avgsil.hw[i]<-getAvgSil(clusthw, gower_mat)
  
}

sil_width <- c(NA)
dunn.index.p<-c(NA)
connie.p<-c(NA)
for(i in 2:10){
  
  pam_fit <- pam(na.omit(gower_dist),
                 diss = TRUE,
                 k = i)
  dunn.index.p[i]<-dunn(gower_mat, pam_fit$clustering)
  connie.p[i]<-connectivity(gower_mat, pam_fit$clustering)
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
#complete dunn (average much worse) median about same, mcquitty to 7, single to 8 ward to 4
#silh complete average worse at 6, better at 8, median same, mcquitty same, single to 8, ward bad
#conn complete, average to 8, median to 8, mcquitty to 6, single to8, ward bad
#consensus is 6 clusters before measures get worse, of those mcquitty seems best

dunn.index<-as.data.frame(cbind(dunn.index.h, dunn.index.ha, 
                                 dunn.index.hs,
                                dunn.index.p, dunn.index.hm, dunn.index.hq,
                                dunn.index.hw, dunn.index.d))
colnames(dunn.index)<-c("complete", "average",  "single",
                        "pam", "median", "mcquitty", "ward", "diana")

conn<-as.data.frame(cbind(connie.h, connie.ha,  connie.hs,
                           connie.p, connie.hm, connie.hq, connie.hw, connie.d))
colnames(conn)<-c("complete", "average",  "single",
                  "pam", "median", "mcquitty", "ward", "diana")

silh<-as.data.frame(cbind(avgsil.h, avgsil.ha,  avgsil.hs,
                           sil_width, avgsil.hm, avgsil.hq, avgsil.hw, avgsil.d))
colnames(silh)<-c("complete", "average",  "single",
                  "pam", "median", "mcquitty", "ward", "diana")

num_clust<-c(1:10)
tableau<- cbind(num_clust, silh)
table_long <- tableau %>%
  gather(clustering, value, -num_clust) # convert to long format

ggplot(data=table_long,
       aes(x=num_clust, y=value, colour=clustering)) +
  geom_line() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), labels=c(1,2,3,4,5,6,7,8,9,10)) +
  ggtitle("Comparison of different cluster methods - Silhouette width") +
  xlab("Number of clusters") +
  ylab("avg silhouette width") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

