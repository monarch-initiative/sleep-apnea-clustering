gower_dist <- daisy(as.data.frame(df), 
                    metric = "gower", 
                    type=list(symm=c(2), 
                              asymm=c(4,5)))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

hier=hclust(gower_dist, method="complete") #complete linkage
hier.avg<-hclust(gower_dist, method="average") #average linkage

hier.sing<-hclust(gower_dist, method="single") #single linkage

#takes a long time
hier.di<-diana(gower_dist)
