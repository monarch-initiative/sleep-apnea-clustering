gower_dist <- daisy(as.data.frame(df.choose), 
                    metric = "gower", 
                    type=list(symm=c(2), 
                              asymm=c(1, 3)))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

hier=hclust(gower_dist, method="complete") #complete linkage
hier.avg<-hclust(gower_dist, method="average") #average linkage
hier.mcq<-hclust(gower_dist, method="mcquitty")
hier.med<-hclust(gower_dist, method="median")
hier.sing<-hclust(gower_dist, method="single")
hier.ward<-hclust(gower_dist, method="ward.D")
dirty<-diana(gower_dist)