
ljoined<-left_join(df.anthro.cat, df.oxysat)
qplot(x=Var1, y=Var2, data=melt(cor(df.anthro)), fill=value, geom="tile")
df.select<-ljoined %>% select(bmicat, avdrop5)
ggplot(ljoined, aes(y=avdnbp5, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=neck20, x=bmicat))+geom_boxplot()

df.bp <- basicSubset(datadict, shhs1.pruned, "Blood Pressure")
drop.cols<-c("aai", "ankbp", "armbp", "dias120", "dias220", "dias320", "syst220",
             "syst120", "syst320")
shhs1.bp<-removeCols(df.bp, drop.cols)
bp.corr<-corrNASpear(df.bp)
source("lib/plotMeltedCorr.R")
plotMeltedCorr(bp.corr)

df.blood <- basicSubset(datadict, shhs1.pruned, "Bloods")
blood.corr<-corrNASpear(df.bloods.joined)
plotMeltedCorr(blood.corr)
df.bloods.joined<-left_join(df.blood, df.bp)

df.ecg <- basicSubset(datadict, shhs1.pruned, "ECG")

cols=c("ilbbb", "lah", "lvh3_3",  "truposmi", "rtrial")
intcols=c("qrs","ventrate")
shhs1.ecg.fct<-df.ecg %>%
  mutate_each_(funs(factor(.)),cols)
df.ecg.fct<-shhs1.ecg.fct %>%
  select(-qrs, -ventrate, -obf_pptid) %>%
  mutate_each(funs(as.factor))

df.ecg.fct.no<-df.ecg.fct %>%
  select(-part2deg, -mob2, -wpw)

summy<-df.ecg.fct %>%
  select(rvh, rtrial) %>%
  group_by(rvh, rtrial) %>%
  summarize(count=n())

summy.nona <- summy %>%
  filter(!(is.na(rvh) | is.na(rtrial)))

s<-sapply(summy.nona, function(x) length(unique(x)))

sapply(df.ecg.fct, function(col) length(count(col)))
catCrossTabs(df.ecg.fct.no, "crosstabs-ecg.txt")

df.ecg.num<-shhs1.ecg.fct %>%
  select(ventrate, qrs) %>%
  mutate_each(funs(as.integer))
#qrs, ventrate
catCVmat(df.ecg.fct %>% select(-mob2, -part2deg, -wpw))
df.ecg.fct %>%
  group_by(rtrial, rvh) %>%
  summarize(count=n())

  select(-mob2, -part2deg, -wpw) %>%
#  summary
  catCVmat

df.ecg.fct %>%
  group_by(afib, vpbs) %>%
  summarize(count=n())
sapply(df.ecg.fct, group_by, col)

df.ecg %>% 
  group_by(obf_pptid) %>%
  summarize(count=n())

dfsummy<-as.data.frame(summy)
dfsummy %>%
  group_by(Var2) %>%
  summarize(count=n())

names(df.ecg.fct)
cols_to_select=names(df.ecg.fct)[c(25, 24)]
t<-df.ecg.fct %>%
  select(one_of(cols_to_select)) %>%
  na.omit %>%
  group_by_(cols_to_select[1]) %>%
  summarize(count=n())

if (nrow(t)<2){
  print("One of the features has only 1 level.")
  foo<-c(foo, names(df.ecg.fct)[i])
}

# Columns you want to group by
grp_cols <- names(df.ecg.fct)

# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)

# Perform frequency counts
df.ecg.fct %>%
  group_by_(.dots=dots) %>%
  summarise(count = n())

ecg.corr<-catCVmat(df.ecg.fct.no)
source("lib/plotMeltedCorr.R")
plotMeltedCorr(ecg.corr)
melted<-melt(ecg.corr)
p <- qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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


df.medalert<-basicSubset(datadict, shhs1.pruned, "Medical Alert")
source("lib/catCrossTabs.R")

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
dim(df.sleephabit)

mat.sleephabit=df.sleephabit %>%
  select(-obf_pptid, -starts_with("hrs"), -starts_with("mi"), -naps02, -starts_with("tfaw"), 
         -starts_with("twuw"), -starts_with("yrs")) %>%
  as.matrix()
colb=df.sleephabit %>% pull(2)
cola=df.sleephabit %>% pull(3)


for (i in 1:(dim(mat.sleephabit)[2]-1)){
  for (j in 2:dim(mat.sleephabit)[2]){
    if (i<j){
      cola=mat.sleephabit[,i]
      colb=mat.sleephabit[,j]
      out<- capture.output(CrossTable(cola, colb, chisq=TRUE))
      cat(colnames(mat.sleephabit)[i], " x ", colnames(mat.sleephabit)[j], out,
          file="crosstabs-sleephabit.txt", sep="\n", append=TRUE)
      #print (colnames(mat.sleephabit)[i])
      #print (colnames(mat.sleephabit)[j])
    }
  }
}

#summary
df.sleephabit %>%
  select(-obf_pptid, -starts_with("hrs"), -starts_with("mi"), -naps02, 
         -starts_with("tfaw"), -starts_with("twuw"), -starts_with("yrs")) %>%
  mutate_each(funs(factor)) %>%
  summary

sleephab.num<-df.sleephabit %>%
  select(starts_with("hrs"), starts_with("mi"), naps02, 
         starts_with("yrs"), 
         -obf_pptid)
summary(sleephab.num)
boxplot(sleephab.num)
cor(na.omit(sleephab.num))

noyes.tab<-lookUpDomain(datadict, "yes")
terms_noyes<-pullTerms(noyes.tab, df.sleephabit)
df.sleephabit.noyes<-subsetCols(df.sleephabit, terms_noyes)
catCrossTabs(df.sleephabit.noyes, "crosstabs-sleephab-noyes.txt")

df.sleephab.no<-removeCols(df.sleephabit, terms_noyes)
df.sleephab.no %>%
  select(-starts_with("hrs"), -starts_with("mi"), -naps02, 
         -starts_with("tfaw"), -starts_with("twuw"), -starts_with("yrs")) %>%
  na.omit %>%
  cor(method="spearman")

melted<-df.sleephab.no %>%
  select(-obf_pptid, -starts_with("tfaw"), -starts_with("twuw"),-starts_with("mi")) %>%
  na.omit %>%
  cor (method="spearman") %>%
  melt

#tidysf36 <- sf36.grped %>%
#  gather(key, value, -gender, -race)
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
joined.demo.anthro.sf36<-left_join(joined.demo.anthro, df.sf36)
out <- capture.output(summary(my_very_time_consuming_regression))

cat("My title", out, file="summary_of_my_very_time_consuming_regression.txt", 
    sep="n", append=TRUE)
cola<-sapply(df.sleephabit, pull((2:dim(df.sleephabit)[2])))

df.sf36<-basicSubset(datadict, shhs1.pruned, "SF-36")
summary(df.sf36)
boxplot(df.sf36 %>%
          select(starts_with("raw"), -obf_pptid))

ggplot(joined.demo.anthro.sf36, aes(y=bp_s1, x=bmi_s1)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)

sf36.grped<-joined.demo.anthro.sf36 %>%
  group_by(gender, race) %>%
  select(one_of(sf36.scores)) 

sf36.scores<-df.sf36 %>%
  select(-starts_with("raw"), -obf_pptid) %>%
  colnames


tidysf36 <- sf36.grped %>%
  gather(key, value, -gender, -race)

ggplot(tidysf36, aes(y=value, 
                x=as.factor(gender), 
                fill=as.factor(race))) + geom_boxplot() + facet_wrap(~key)

df.med<-basicSubset(datadict, shhs1.pruned, "Medications")

df.med %>%
  select(-obf_pptid) %>%
  mutate_each(funs(factor)) %>%
  summary

mat.med=df.med %>%
  select(-obf_pptid) %>%
  as.matrix()

for (i in 1:(dim(mat.med)[2]-1)){
  for (j in 2:dim(mat.med)[2]){
    if (i<j){
      cola=mat.med[,i]
      colb=mat.med[,j]
      out<- capture.output(CrossTable(cola, colb, chisq=TRUE))
      cat(colnames(mat.med)[i], " x ", colnames(mat.med)[j], out,
          file="crosstabs-meds.txt", sep="\n", append=TRUE)
      #print (colnames(mat.sleephabit)[i])
      #print (colnames(mat.sleephabit)[j])
    }
  }
}

df.qual <- basicSubset (datadict, shhs1.pruned, "Quality Of Life")
catCrossTabs(df.qual.yes, "crosstabs-qual-yesno.txt")

terms.noyes.qual<-pullTerms(noyes.tab, df.qual)
df.qual.yes<-subsetCols(df.qual, terms.noyes.qual)
df.qual.num<-removeCols(df.qual, terms.noyes.qual)

melted<-df.qual.num %>%
  select(-obf_pptid) %>% #, -starts_with("tfaw"), -starts_with("twuw"),-starts_with("mi")) %>%
  na.omit %>%
  cor (method="spearman") %>%
  melt

qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df.medhist <- basicSubset(datadict, shhs1.pruned, "Medical History")
df.medhist.num<-df.medhist %>% 
  select(which(sapply(.,is.double)))
cor(na.omit(df.medhist.num))

df.medhist.cat<-df.medhist %>% 
  select(-which(sapply(.,is.double)))

catCrossTabs(df.medhist.cat, "crosstabs-medhist.txt")

df.adverse <- basicSubset(datadict, shhs1.pruned, "Adverse Events")
df.adverse %>%
  select(-obf_pptid) %>%
  mutate_each(funs(factor)) %>%
  summary

df.adverse.no0<-df.adverse %>%
  select(-imhihrae, -imlohrae, -imosatae, -urosatae)

cola<-df.adverse$imdbpae
colb<-df.adverse$imsbpae
crossout<-CrossTable(cola, colb, chisq=TRUE)
crossout$chisq$p.value
cv.test(cola, colb)
adverse.corr<-catCVmat(df.adverse.no0)

melted<-melt(adverse.corr)

catCrossTabs(df.adverse.no0, "crosstabs-adverse.txt")

df.ess<- basicSubset(datadict, shhs1.pruned, "Epworth Sleepiness Scale")

df.ess %>% select (-obf_pptid) %>% na.omit %>% cor(method="spearman")

df.morning <- basicSubset(datadict, shhs1.pruned, "Morning Survey")

#diffa10, meds10
df.morning %>% 
  select(-diffa10, -meds10, -obf_pptid) %>%
  na.omit %>%
  cor(method="spearman")

summary(df.morning)

df.health <- basicSubset(datadict, shhs1.pruned, "Health Interview")

#asa15, evsmok15, nitro15, ns1yr15, slpill15, smknow15, 

df.health %>%
  select(-obf_pptid, -asa15, -evsmok15, -nitro15, -ns1yr15, -slpill15, 
         -smknow15) %>%
  na.omit %>%
  cor(method="spearman")

df.health.yesno<-df.health %>%
  select(obf_pptid, asa15, evsmok15, nitro15, ns1yr15, slpill15, smknow15)

catCrossTabs(df.health.yesno, "crosstabs-healthint.txt")

df.sleeparch<-basicSubset(datadict, shhs1.pruned, "Sleep Architecture")
#stage shifts, minutes, percent, latencygrade (domain)
stagesh.tab<-lookUpUnits(datadict, "stage shifts")
terms_stagesh<-pullTerms(stagesh.tab, df.sleeparch)
minutes.tab<-lookUpUnits(datadict, "minutes")
terms_minutes<-pullTerms(minutes.tab, df.sleeparch)
df.sa.stagesh<-subsetCols(df.sleeparch, terms_stagesh)
df.sa.min<-subsetCols(df.sleeparch, terms_minutes)

minutes_corr<-corrNASpear(df.sa.min %>% select(-slptime))
melted<-melt(minutes_corr)
melteddesc<-melted %>%
  arrange(desc(value)) %>%
  filter(abs(value)>.5)
qplot(x=Var1, y=Var2, data=melteddesc, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#assocstats in vcd package Cramer's V
#ICC in psych package or ICC package
#eta (sqrt of eta squared) from ANOVA

model <- lm(waist ~ bmicat, data=na.omit(df.anthro.cat))
rsq<-summary(model)$r.squared
sqrt(rsq)
boxplot(waist ~ bmicat, na.omit(df.anthro.cat))

library(psych)
library(vcd)

source("lib/corrNaSpear.R")
df.lung<-basicSubset(datadict, shhs1.pruned, "Lung Function")
corrNASpear(df.lung)