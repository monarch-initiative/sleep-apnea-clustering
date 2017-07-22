categ<-c( "survey q's", "interim","clin. meas.","med hist",  
          "anthro.", "PSG","repeats", "spec anal.", "meds" ,"signal qual.", 
          "sleep arch.", 
          "admin."," ", "demog.")
piechartno<-data.frame(
  categ=c("PSG","repeats", "spectral analysis", "medications" ,"signal quality", 
          "sleep architecture", 
          "administrative","other", "demographics"),
  values=c(850, 224,166, 125, 82, 69, 50, 33, 10),
  include=rep_len("no", 9)
)
piechartyes<-data.frame(
  categ=c("survey questions", "interim","clinical measurements","medical history",  
          "anthropometry"),
  values=c(155, 134, 62, 31, 10),
  include=rep_len("yes", 5)
)
piechart<-rbind(piechartno, piechartyes)

piechart$categ<-factor(piechart$categ, levels=piechart$categ[order(piechart$include)])

bp<- ggplot(piechart, aes(x="", y=values, fill=include))+
  geom_bar(width = 1, stat = "identity", color="black") +
  guides(fill=guide_legend(override.aes=list(colour=NA))) +
  guides(fill=guide_legend(title="Included"))

pie <- bp + coord_polar(theta="y", start=0)

y.breaks <- cumsum(values) - values/2
y.breaks  

p <- pie +
  theme(axis.ticks=element_blank(),  # the axis ticks
        axis.title=element_blank(),  # the axis labels
        axis.text.y=element_blank())+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # prettiness: make the labels black
  theme(axis.text.x=element_text(color='black')) +
  scale_y_continuous(
    breaks=y.breaks,   # where to place the labels
    labels=categ# the labels
  )
values<-c(155,134,62,31,10,850,224, 166, 125, 82, 69,50, 33, 10)

ggplot(df.wclust, aes(x=factor(cluster), y=pcstahda, fill=factor(htnderv_s1))) +
  geom_boxplot()