
df.anthro <-basicSubset(datadict, shhs1.pruned, "Anthropometry")
df.anthro.cat<-df.anthro %>% mutate(bmicat = cut(bmi_s1, 
                                  breaks=c(0, 18.5, 25, 30, Inf),
                                  labels=c("underweight", "normal", "overweight", 
                                           "obese"),
                                  #include.lowest=TRUE, 
                                  right=TRUE))
ljoined<-left_join(df.anthro.cat, df.oxysat)
qplot(x=Var1, y=Var2, data=melt(cor(df.anthro)), fill=value, geom="tile")
df.select<-ljoined %>% select(bmicat, avdrop5)
ggplot(ljoined, aes(y=avdnbp5, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=neck20, x=bmicat))+geom_boxplot()

df.bp <- lookUpFolderWithString(datadict, "Blood Pressure")
terms_bp<-pullTerms(df.bp, shhs1.pruned)
terms_bp<-c("obf_pptid", terms_bp)
drop.cols<-c("aai", "ankbp", "armbp", "dias120", "dias220", "dias320", "syst220",
             "syst120", "syst320")
shhs1.bp<-subsetCols(shhs1.pruned, terms_bp)
shhs1.bp<-removeCols(shhs1.bp, drop.cols)

df.blood <- lookUpFolderWithString(datadict, "Bloods")
terms_blood<-pullTerms(df.blood, shhs1.pruned)
shhs1.blood<-subsetCols(shhs1.pruned, terms_blood)

df.ecg <- lookUpFolderWithString(datadict, "ECG")
terms_ecg<-pullTerms(df.ecg, shhs1.pruned)
terms_ecg<-c("obf_pptid", terms_ecg)
shhs1.ecg<-subsetCols(shhs1.pruned, terms_ecg)

cols=c("ilbbb", "lah", "lvh3_3", "qrs", "truposmi", "ventrate", "rtrial")
shhs1.ecg.fct<-shhs1.ecg %>%
  mutate_each_(funs(factor(.)),cols)

df.arousal <- basicSubset(datadict, shhs1.pruned, "Arousals")

na.omit(shhs1.bp) %>% summarize(mean(diasbp), sd(diasbp), mean(systbp), sd(systbp))
summary(shhs1.bp)

xtabs(~lvh3_1+lvh3_3, data=shhs1.ecg.fct)

library(gmodels)
CrossTable(shhs1.ecg.fct$afib, shhs1.ecg.fct$apbs, prop.t=TRUE, prop.r=FALSE, 
           prop.c=FALSE, fisher=TRUE)
#chisq=TRUE

CrossTable(joined.demo.anthro[,2], joined.demo.anthro[,3], prop.t=TRUE, 
           prop.r=FALSE, prop.c=FALSE, chisq=TRUE)

library(Hmisc)
describe(mydata)

library(pastecs)
stat.desc(mydata)

library(psych)
describe(shhs1.bp)

long.shhs.bp<-shhs1.bp %>% 
  select(-obf_pptid) %>% 
  gather(BP, mmhg, diasbp:systbp)

long.shhs.blood<-shhs1.blood %>% 
  #select(-obf_pptid) %>% 
  gather(Blood, mgdl, chol:trig)

shhs1.arousals<-df.arousal %>% select(matches ("ar"))
boxplot(df.arousal %>% select(-obf_pptid))
p<-ggplot(long.shhs.blood, aes(factor(Blood), mgdl))
p + geom_boxplot()

boxplot(shhs1.blood %>% select(-trig))
par(mfrow=c(2,1), mar=c(2.5, 2.1, 2.1, 1.0))
hist(shhs1.bp$diasbp)
hist(shhs1.bp$systbp)

par(mfrow=c(1,1), mar=c(5.1, 2.1, 2.1, 4.1))
#now make your lovely plot
p<-ggplot(long.shhs.blood, aes(mgdl, fill = Blood)) 
p + geom_density(alpha = 0.2)
p + geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity')
#can fill in y=..density..

qqplot(x=shhs1.bp$diasbp, y=shhs1.bp$systbp)
qqplot(x=shhs1.blood$chol, y=shhs1.blood$hdl)

ggplot(data=shhs1.blood, aes(x=chol, y=hdl)) + geom_point()
ggplot(data=shhs1.blood, aes(x=chol, y=trig)) + geom_point()

ggplot(data=df.arousal, aes(x=ahnrembp, y=ai_nrem)) + geom_point()

df.hr<-basicSubset(datadict, shhs1.pruned, "Heart Rate")
min_terms<-lookUpDispnameWithString(datadict, "Minimum")
terms_to_drop<-pullTerms(min_terms, df.hr)
df.hr.nomin<-removeCols(df.hr, terms_to_drop)
max_terms<-lookUpDispnameWithString(datadict, "Maximum")
terms_to_drop<-pullTerms(max_terms, df.hr.nomin)
df.hr.nomax<-removeCols(df.hr.nomin, terms_to_drop)
threepercent<-lookUpDispnameWithString(datadict, "3%")
terms_3pc<-pullTerms(threepercent, df.hr.nomax)
df.hr.3pc<-subsetCols(df.hr.nomax, terms_3pc)

df.medalert<-basicSubset(datadict, shhs1.pruned, "Medical Alert")

df.oxysat<-basicSubset(datadict, shhs1.pruned, "Oxygen Saturation")
lookUpDispname(datadict, "arousals")

df.resp<-basicSubset(datadict, shhs1.pruned, "Respiratory Events")
lookUpFolderWithString(datadict, "Indexes")

df.anthro.cat$hip[2716]=110.4
ggplot(df.anthro %>% 
         select(-obf_pptid, -bmi_s1, -weight, -weight20))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=neck20, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=hip, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=waist, x=bmicat))+geom_boxplot()
ggplot(data=df.anthro, aes(x=bmi_s1, y=waist))+geom_point()
ggplot(data=df.anthro, aes(x=bmi_s1, y=hip))+geom_point()
ggplot(data=df.anthro, aes(x=bmi_s1, y=neck20))+geom_point()

library(reshape2)
melted<-melt(cor(na.omit(df.anthro%>%select(-obf_pptid))))
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile")
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

melted <- melt(cor(na.omit(shhs1.avg.noarous%>%select(-obf_pptid))))

boxplot(df.anthro %>% select (-obf_pptid, -height, -hip, -neck20, -waist, -bmi_s1))

df.demo<-basicSubset(datadict, shhs1.pruned, "Demographics")
joined.demo.anthro<-left_join(df.demo, df.anthro.cat)

sampledat<-joined.demo.anthro %>% 
  group_by(age_category_s1, bmicat) %>%
  summarize(count=n())

sampledat<-joined.demo.anthro %>%
  group_by(bmicat, age_category_s1) %>%
  summarize(count=n())

ggplot(data=sampledat, aes(y=count, x=as.factor(age_category_s1), 
                           fill=bmicat))+geom_bar(stat="identity", 
                                                  position=position_dodge())

sampledat<-joined.demo.anthro %>%
#  select(gender, race, bmi_s1) %>%
  group_by(gender, race)

ggplot(data=sampledat, aes(y=neck20, x=as.factor(gender), 
                           fill=as.factor(race))) + geom_boxplot()

CrossTable(joined.demo.anthro$gender, joined.demo.anthro$bmicat, chisq=TRUE)



df.sleephabit <-basicSubset(datadict, shhs1.pruned, "Sleep Habits")
cola=df.sleephabit[,2]
colb=df.sleephabit[,4]
CrossTable(cola, colb, chisq=TRUE)