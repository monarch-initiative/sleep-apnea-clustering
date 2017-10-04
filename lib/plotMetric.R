plotMetric<-function(metric){
  num_clust<-c(1:10)
  tableau<- cbind(num_clust, metric)
  table_long <- tableau %>%
    gather(clustering, value, -num_clust) # convert to long format
  
  p<-ggplot(data=table_long,
         aes(x=num_clust, y=value, colour=clustering)) +
    geom_line() +
    scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), labels=c(1,2,3,4,5,6,7,8,9,10)) +
    ggtitle("Comparison of different cluster methods") +
    xlab("Number of clusters") +
    ylab("metric") +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
  
  return(p)
}



