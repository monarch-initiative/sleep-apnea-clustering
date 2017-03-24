plotMeltedCorr<-function (correlated) {
  melted<-melt(correlated)
  qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(melted)
}
