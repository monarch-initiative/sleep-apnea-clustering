library(clValid)

source("lib/getAvgSil.R")

dunn.index.h=c(NA)
dunn.index.hs=c(NA)
dunn.index.ha=c(NA)
dunn.index.d=c(NA)

connie.h=c(NA)
connie.hs<-c(NA)
connie.ha<-c(NA)
connie.d<-c(NA)

avgsil.h<-c(NA)
avgsil.ha<-c(NA)
avgsil.hs<-c(NA)
avgsil.d<-c(NA)



for(i in 2:10){
  clusth<-cutree(hier, k=i)
  clustha<-cutree(hier.avg, k=i)
  clusthsing <-cutree(hier.sing, k=i)
  clustd<-cutree(hier.di, k=i)
  
  dunn.index.h[i]=dunn(gower_mat, clusth)
  dunn.index.hs[i]=dunn(gower_mat, clusthsing)
  dunn.index.ha[i]=dunn(gower_mat, clustha)
  dunn.index.d[i]=dunn(gower_mat, clustd)
  
  connie.h[i]<-connectivity(distance=gower_mat, clusth)
  connie.ha[i]<-connectivity(distance=gower_mat, clustha)
  connie.hs[i]<-connectivity(distance=gower_mat, clusthsing)
  connie.d[i]<-connectivity(distance=gower_mat, clustd)
  
  avgsil.d[i]<-getAvgSil(clustd, gower_mat)
  avgsil.h[i]<-getAvgSil(clusth, gower_mat)
  avgsil.ha[i]<-getAvgSil(clustha, gower_mat)
  avgsil.hs[i]<-getAvgSil(clusthsing, gower_mat)
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

dunn.index<-as.data.frame(cbind(dunn.index.h, dunn.index.ha, 
                                 dunn.index.hs,
                                dunn.index.p,
                                dunn.index.d))
colnames(dunn.index)<-c("complete", "average",  "single",
                        "pam", "diana")

conn<-as.data.frame(cbind(connie.h, connie.ha,  connie.hs,
                           connie.p, connie.d))
colnames(conn)<-c("complete", "average",  "single",
                  "pam", "diana")

silh<-as.data.frame(cbind(avgsil.h, avgsil.ha,  avgsil.hs,
                           sil_width, avgsil.d))
colnames(silh)<-c("complete", "average",  "single",
                  "pam", "diana")

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

ggplot(data=table_long,
       aes(x=num_clust, y=value, colour=clustering)) +
  geom_line() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), labels=c(1,2,3,4,5,6,7,8,9,10)) +
  ggtitle("Comparison of different cluster methods - Dunn index") +
  xlab("Number of clusters") +
  ylab("Dunn index") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

