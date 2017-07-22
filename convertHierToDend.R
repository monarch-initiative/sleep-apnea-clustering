
convertHierToDend <- function(hier){
  dend<-as.dendrogram(hier)
  dendcolor<-dend %>% set("branches_k_color", 
                          value = c("blue", "orange", "green", "purple", "red", 
                                    "cyan3"), k = 6) %>%
    #plot(xlab="", sub="")
  return(dendcolor)

}

gghier<-as.ggdend(dend, type="rectangle")
ggplot(gghier, segments = TRUE, labels = FALSE, nodes = TRUE, 
       horiz = FALSE, theme = theme_dendro()) 