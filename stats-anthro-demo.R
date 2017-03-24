library(reshape2)
library(gmodels)

#anthro dataset
df.anthro <-basicSubset(datadict, shhs1.pruned, "Anthropometry")
#add bmi category
df.anthro.cat<-df.anthro %>% 
  mutate(bmicat = cut(bmi_s1, breaks=c(0, 18.5, 25, 30, Inf),
                      labels=c("underweight", "normal", "overweight", "obese"),
                      #include.lowest=TRUE, 
                      right=TRUE))
#fix mistaken entry
df.anthro.cat$hip[2716]=110.4

#boxplots and scatterplots
ggplot(df.anthro %>% 
         select(-obf_pptid, -bmi_s1, -weight, -weight20))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=neck20, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=hip, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=waist, x=bmicat))+geom_boxplot()
ggplot(data=df.anthro, aes(x=bmi_s1, y=waist))+geom_point()
ggplot(data=df.anthro.cat, aes(x=bmi_s1, y=hip))+geom_point()
ggplot(data=df.anthro, aes(x=bmi_s1, y=neck20))+geom_point()

#correlation graph
melted<-melt(cor(na.omit(df.anthro.cat%>%select(-obf_pptid))))

qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

#Add demographics
df.demo<-basicSubset(datadict, shhs1.pruned, "Demographics")
joined.demo.anthro<-left_join(df.demo, df.anthro.cat)

sampledat<-joined.demo.anthro %>%
  group_by(bmicat, age_category_s1)

ggplot(data=sampledat, aes(y=count, x=as.factor(age_category_s1), 
                           fill=bmicat))+geom_bar(stat="identity", 
                                                  position=position_dodge())

sampledat<-joined.demo.anthro %>%
  #  select(gender, race, bmi_s1) %>%
  group_by(gender, race)

ggplot(data=sampledat, aes(y=neck20, x=as.factor(gender), 
                           fill=as.factor(race))) + geom_boxplot()

CrossTable(joined.demo.anthro$gender, joined.demo.anthro$bmicat, chisq=TRUE)

sampledat<-joined.demo.anthro %>%
  group_by(age_category_s1)

ggplot(data=sampledat, aes(y=neck20, 
                           x=as.factor(age_category_s1))) + geom_boxplot()

ggplot(data=joined.demo.anthro, aes(y=bmi_s1, x=age_s1)) + geom_point()

sampledat<-joined.demo.anthro %>%
  group_by(race, ethnicity)

ggplot(data=sampledata, aes(y=neck20, x=as.factor(race), 
                            fill=as.factor(ethnicity))) + geom_boxplot()

joined.demo.anthro %>%
  + group_by(race, ethnicity) %>%
  + summarize(count=n())