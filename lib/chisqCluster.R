#chisqCluster

require(gmodels)

chisqCluster<-function (data, feat){
  csframe<-data %>% gather(feature, value, -cluster) %>%
    group_by(cluster, feature, value) %>%
    tally %>% 
    spread(value, n, fill = 0)
  
  csdat<-as.data.frame(csframe %>% filter(feature==feat)) %>% 
    select(-cluster, -feature)
  
  xtab<-CrossTable(as.matrix(csdat), chisq=TRUE)
  
  return(xtab$chisq$p.value)
  
}


