
#anthro dataset
df.anthro1 <-basicSubset(datadict, shhs1.pruned, "Anthropometry")
#add bmi category
df.anthro.cat<-df.anthro1 %>% 
  mutate(bmicat = cut(bmi_s1, breaks=c(0, 18.5, 25, 30, Inf),
                      labels=c(1, 2, 3, 4),
                      #include.lowest=TRUE, 
                      right=TRUE))
#fix mistaken entry
df.anthro.cat$hip[2716]=110.4
df.anthro.cat<-df.anthro.cat %>%
  mutate(bmicat=as.integer(bmicat))

#blood presuure
df.bp <- basicSubset(datadict, shhs1.pruned, "Blood Pressure")
drop.cols<-c("aai", "ankbp", "armbp", "dias120", "dias220", "dias320", "syst220",
             "syst120", "syst320")
df.bp<-removeCols(df.bp, drop.cols)

#bloods
df.blood <- basicSubset(datadict, shhs1.pruned, "Bloods")
df.bloods.joined<-left_join(df.blood, df.bp)

#lung function
df.lung <- basicSubset(datadict, shhs1.pruned, "Lung Function")

#respiratory
df.resp <- basicSubset(datadict, shhs1.pruned, "Respiratory Events")
df.oxysat <- basicSubset(datadict, shhs1.pruned, "Oxygen Saturation")

df.hr <- basicSubset(datadict, shhs1.pruned, "Heart Rate")
df.arousal <-basicSubset(datadict, shhs1.pruned, "Arousals")

#sleep architecture
df.sleeparch<-basicSubset(datadict, shhs1.pruned, "Sleep Architecture")
#ESS
df.ess<- basicSubset(datadict, shhs1.pruned, "Epworth Sleepiness Scale")
#SF-36
df.sf36<-basicSubset(datadict, shhs1.pruned, "SF-36")

#join
df.joined<-left_join(df.anthro.cat, df.bloods.joined)
df.joined<-left_join(df.joined, df.lung)
df.joined<-left_join(df.joined, df.arousal)
df.joined<-left_join(df.joined, df.resp)
df.joined<-left_join(df.joined, df.oxysat)
df.joined<-left_join(df.joined, df.hr)
df.joined<-left_join(df.joined, df.ess)
df.joined<-left_join(df.joined, df.sf36)
df.joined<-left_join(df.joined, df.sleeparch)

