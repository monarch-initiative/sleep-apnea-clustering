#This file is deprecated
ljoined<-left_join(df.anthro.cat, df.oxysat)
qplot(x=Var1, y=Var2, data=melt(cor(df.anthro)), fill=value, geom="tile")
df.select<-ljoined %>% select(bmicat, avdrop5)
ggplot(ljoined, aes(y=avdnbp5, x=bmicat))+geom_boxplot()
ggplot(df.anthro.cat, aes(y=neck20, x=bmicat))+geom_boxplot()

df.bp <- basicSubset(datadict, shhs1.pruned, "Blood Pressure")
df.bp2 <- basicSubset(datadict, shhs2.pruned, "Blood Pressure")
drop.cols<-c("aai", "ankbp", "armbp", "dias120", "dias220", "dias320", "syst220",
             "syst120", "syst320")
shhs1.bp<-removeCols(df.bp, drop.cols)

df.bp2.clean<-df.bp2 %>% 
  select(-starts_with("pm")) %>%
  rename(diasavg2=avg23bpd_s2, sysavg2=avg23bps_s2)
joined.bp<-left_join(shhs1.bp, df.bp2.clean)
bp.corr<-corrNASpear(joined.bp)
source("lib/plotMeltedCorr.R")
p<-plotMeltedCorr(bp.corr, "cont")

df.blood <- basicSubset(datadict, shhs1.pruned, "Bloods")
df.blood2 <- basicSubset(datadict, shhs2.pruned, "Bloods")
blood.corr<-corrNASpear(df.bloods.joined)
plotMeltedCorr(blood.corr)
df.bloods.joined<-left_join(df.blood, df.bp)

df.ecg <- basicSubset(datadict, shhs1.pruned, "ECG")
df.ecg2 <- basicSubset(datadict, shhs2.pruned, "ECG")
terms_ecg.yn<-pullTerms(noyes.tab, df.ecg2)
df.ecg2.yn<-subsetCols(df.ecg2, terms_ecg.yn)
df.ecg2.yn %>%
  mutate_each(funs(as.factor)) %>%
  summary

joined.ecg<-left_join(shhs1.ecg.fct, shhs2.ecg.fct, by="obf_pptid")
corr.ecg.join<-catCVmat(joined.ecg %>%
           select(-part2deg.x, -part2deg.y, -mob1.x, -mob2.x, -mob1.y, -mob2.y,
                  -wpw.x, -wpw.y, -ventrate.x, -ventrate.y, -qrs.x, -qrs.y, 
                  -obf_pptid)) 

p<-plotMeltedCorr(corr.ecg.join, "cat")
cols=c("ilbbb", "lah", "lvh3_3",  "truposmi", "rtrial")
cols=c("ilbbb", "lah", "lvh3_3", "rtrial")
intcols=c("qrs","ventrate")
shhs1.ecg.fct<-df.ecg %>%
  mutate_each_(funs(factor(.)),cols)
df.ecg.fct<-shhs1.ecg.fct %>%
  select(-qrs, -ventrate, -obf_pptid) %>%
  mutate_each(funs(as.factor))
shhs2.ecg.fct<-df.ecg2 %>%
  mutate_each_(funs(factor(.)),cols)
df.ecg.fct2<-shhs2.ecg.fct %>%
  select(-qrs, -ventrate, -obf_pptid) %>%
  mutate_each(funs(as.factor))

df.ecg.num<-df.ecg %>% select(obf_pptid, ventrate, qrs) %>%
  mutate_each(funs(as.integer))
df.ecg.num2<-df.ecg2 %>% select(obf_pptid, ventrate, qrs)
corrNASpear(left_join(df.ecg.num, df.ecg.num2, by="obf_pptid"))

shhs2.ecg.fct %>%
  select(-part2deg, -mob1, -wpw, -ventrate, -qrs)


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

library(reshape2)

melted <- melt(cor(na.omit(shhs1.avg.noarous%>%select(-obf_pptid))))

boxplot(df.anthro %>% select (-obf_pptid, -height, -hip, -neck20, -waist, -bmi_s1))


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
df.sleephabit2 <-basicSubset(datadict, shhs2.pruned, "Sleep Habits")
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
  select(-obf_pptid, -cpap02, -hvsnrd02, -mdsa02, -o2thpy02, -stpbrt02, 
         -surgsa02, -surgtr02, 
         -starts_with("tfaw"), -starts_with("twuw")) %>%
  #boxplot
  mutate_each(funs(factor)) %>%
  summary

sleephab.num<-df.sleephabit %>%
  select(starts_with("hrs"), starts_with("mi"), naps02, 
         starts_with("yrs"), 
         -obf_pptid)
summary(sleephab.num)
boxplot(sleephab.num)
cor(na.omit(sleephab.num))

source("lib/lookUpDomain.R")
noyes.tab<-lookUpDomain(datadict, "yes")
check.tab<-lookUpDomain(datadict, "check")
terms_noyes<-pullTerms(noyes.tab, df.sleephabit)
terms_noyes<-c("obf_pptid", terms_noyes)
terms_check<-pullTerms(check.tab, df.sleephabit2)
terms_yn<-c("obf_pptid", terms_noyes, terms_check)
df.sleephabit.noyes2<-subsetCols(df.sleephabit2, terms_yn)
df.sleephabit.noyes<-subsetCols(df.sleephabit, terms_noyes)
joined.sleephab.yn<-left_join(df.sleephabit.noyes, df.sleephabit.noyes2)

sleephab.yn.cor<-catCVmat(df.sleephabit.noyes)
sleephab.yn.cor.join<-catCVmat(joined.sleephab.yn %>% select(-obf_pptid))
p<-plotMeltedCorr(sleephab.yn.cor.join, "cat")
catCrossTabs(df.sleephabit.noyes, "crosstabs-sleephab-noyes.txt")

df.sleephab.num<-removeCols(df.sleephabit, terms_noyes)
corr.sleephab.num<-df.sleephab.num %>%
  select(-obf_pptid, -starts_with("tfaw"), -starts_with("twuw")) %>%
  corrNASpear
p.num<-plotMeltedCorr(corr.sleephab.num, "cont")

df.sleephabit.noyes %>%
  mutate_each(funs(factor)) %>%
  summary

#tidysf36 <- sf36.grped %>%
#  gather(key, value, -gender, -race)

joined.demo.anthro.sf36<-left_join(joined.demo.anthro, df.sf36)
out <- capture.output(summary(my_very_time_consuming_regression))

cat("My title", out, file="summary_of_my_very_time_consuming_regression.txt", 
    sep="n", append=TRUE)
cola<-sapply(df.sleephabit, pull((2:dim(df.sleephabit)[2])))

df.sf36<-basicSubset(datadict, shhs1.pruned, "SF-36")
summary(df.sf36)
boxplot(df.sf36 %>%
          select(starts_with("raw"), -obf_pptid))
corr.sf36<-corrNASpear(df.sf36)
p<-plotMeltedCorr(corr.sf36, "cont")

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

corr.med<-catCVmat(df.med %>% select(-obf_pptid, -alphad1))
p<-plotMeltedCorr(corr.med, "cat")


df.qual <- basicSubset (datadict, shhs1.pruned, "Quality Of Life")

terms.noyes.qual<-pullTerms(noyes.tab, df.qual)
df.qual.yes<-subsetCols(df.qual, terms.noyes.qual)
catCrossTabs(df.qual.yes, "crosstabs-qual-yesno.txt")
corr.qual.yn <- catCVmat(df.qual.yes)
p<-plotMeltedCorr(corr.qual.yn, "cat")

df.qual.num<-removeCols(df.qual, terms.noyes.qual)
corr.qual.num<-corrNASpear(df.qual.num %>% select(-obf_pptid))
p.num <- plotMeltedCorr(corr.qual.num, "cont")


df.medhist <- basicSubset(datadict, shhs1.pruned, "Medical History")
terms.noyes.medhist<-pullTerms(noyes.tab, df.medhist)
df.medhist.yes<-subsetCols(df.medhist, terms.noyes.medhist)
df.medhist.num<-removeCols(df.medhist, terms.noyes.medhist)
corr.medhist.num<-corrNASpear(df.medhist.num)
p<-plotMeltedCorr(corr.medhist.num, "cont")
corr.medhist.yn<-catCVmat(df.medhist.yes)
p.cat<-plotMeltedCorr(corr.medhist.yn, "cat")
df.medhist.yes %>%
  mutate_each(funs(factor)) %>%
  summary

df.medhist.num %>% summary
ggplot(df.medhist.num, aes(y=alcoh, x=cgpkyr)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)
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

catCrossTabs(df.adverse.no0, "crosstabs-adverse.txt")

df.ess<- basicSubset(datadict, shhs1.pruned, "Epworth Sleepiness Scale")

corr.ess<-corrNASpear(df.ess)
p<-plotMeltedCorr(corr.ess, "cont")
df.ess %>%
  select(-obf_pptid) %>%
  mutate_each(funs(factor)) %>%
  summary

df.morning <- basicSubset(datadict, shhs1.pruned, "Morning Survey")

#diffa10, meds10
corr.morning<-df.morning %>% 
  select(-diffa10, -meds10, -obf_pptid) %>%
  na.omit %>%
  cor(method="spearman")
p<-plotMeltedCorr(corr.morning, "cont")
summary(df.morning)

df.morning%>%
  select(diffa10, meds10) %>%
  catCVmat

df.health <- basicSubset(datadict, shhs1.pruned, "Health Interview")

#asa15, evsmok15, nitro15, ns1yr15, slpill15, smknow15, 

corr.health<-df.health %>%
  select(-obf_pptid, -asa15, -evsmok15, -nitro15, -ns1yr15, -slpill15, 
         -smknow15, -napsmn15) %>%
  #summary
  na.omit %>%
  cor(method="spearman")

p.num<-plotMeltedCorr(corr.health, "cont")

df.health.yesno<-df.health %>%
  select(obf_pptid, asa15, evsmok15, nitro15, ns1yr15, slpill15, smknow15)

corr.health.yn<-catCVmat(df.health.yesno)
p<-plotMeltedCorr(corr.health.yn, "cat")
catCrossTabs(df.health.yesno, "crosstabs-healthint.txt")

df.sleeparch<-basicSubset(datadict, shhs1.pruned, "Sleep Architecture")
df.sleeparch2<-basicSubset(datadict, shhs2.pruned, "Sleep Architecture")
source("lib/lookUpUnits.R")
#stage shifts, minutes, percent, latencygrade (domain)
stagesh.tab<-lookUpUnits(datadict, "stage shifts")
terms_stagesh<-pullTerms(stagesh.tab, df.sleeparch)
minutes.tab<-lookUpUnits(datadict, "minutes")
terms_minutes<-pullTerms(minutes.tab, df.sleeparch)
df.sa.stagesh<-subsetCols(df.sleeparch, terms_stagesh)
df.sa.min<-subsetCols(df.sleeparch, terms_minutes)
percent.tab<-lookUpUnits(datadict, "percent")
terms_percent<-pullTerms(percent.tab, df.sleeparch)
df.sa.pc<-subsetCols(df.sleeparch, terms_percent)

minutes_corr<-corrNASpear(df.sa.pc)
p<-plotMeltedCorr(minutes_corr, "cont")

df.sleeparch %>%
  select(-obf_pptid, -slptime) %>%
  group_by(sleep_latency) %>%
  boxplot

df.sleeparch %>%
  group_by(sleep_latency) %>%
  #na.omit %>%
  summarize(mean=mean(slplatp), sd=sd(slplatp))

#get rid of slplatp

melteddesc<-melted %>%
  arrange(desc(value)) %>%
  filter(abs(value)>.5)
#assocstats in vcd package Cramer's V
#ICC in psych package or ICC package
#eta (sqrt of eta squared) from ANOVA

model <- lm(waist ~ bmicat, data=na.omit(df.anthro.cat))
term1<-c("waist", "bmicat")
paste(term1, "~", term2)
df<-df.anthro.cat
model <- lm(paste(term1[1], "~", term1[2]), data=na.omit(df))
rsq<-summary(model)$r.squared
sqrt(rsq)
boxplot(waist ~ bmicat, na.omit(df.anthro.cat))
library(psych)
library(vcd)

df<-df.demo1
cat_terms<-c("ethnicity", "gender", "mstat", "race")
cont_terms<-c("age_s1")

mat<-matrix(nrow=length(cat_terms), ncol=length(cont_terms))
rownames(mat)<-cat_terms
colnames(mat)<-cont_terms
for (i in 1:length(cat_terms)){
  for (j in 1:length(cont_terms)){
    model <- lm(paste(cont_terms[j], "~", cat_terms[i]), data=na.omit(df))
    rsq<-summary(model)$r.squared
    mat[i,j]<-sqrt(rsq)
  }
}

contnames<-setdiff(colnames(df.health), colnames(df.health.yesno))
catnames<-colnames(df.health.yesno %>% select(-obf_pptid))
eta<-etaAnovaMat(df.health, catnames, contnames)
plotMeltedCorr(eta, "cat")
df.health %>% select(evsmok15, smknow15, avesmk15, cigday15) %>%
  group_by(smknow15) %>%
  summarize(meana=mean(avesmk15, na.rm=T), meanc=mean(cigday15, na.rm=T), 
            sda=sd(avesmk15, na.rm=T), 
            sdc=sd(cigday15, na.rm=T))

catnames<-c("diffa10", "meds10")
contnames<-colnames(df.morning %>% select(-diffa10, -meds10, -obf_pptid))
eta<-etaAnovaMat(df.morning, catnames, contnames)

contnames<-colnames(df.sleephab.num %>% select(-obf_pptid))
catnames<-colnames(df.sleephabit.noyes %>% select(-obf_pptid))
eta<-etaAnovaMat(df.sleephabit, catnames, contnames)

contnames<-colnames(df.qual.num %>% select(-obf_pptid))
catnames<-colnames(df.qual.yes)
eta<-etaAnovaMat(df.qual, catnames, contnames)

contnames<-colnames(df.medhist.num %>% select(-obf_pptid))
catnames<-colnames(df.medhist.yes)
eta<-etaAnovaMat(df.medhist, catnames, contnames)

contnames<-c("qrs", "ventrate")
catnames<-colnames(df.ecg %>% select(-obf_pptid, -ventrate, -qrs))
eta<-etaAnovaMat(df.ecg, catnames, contnames)

source("lib/corrNaSpear.R")
df.lung<-basicSubset(datadict, shhs1.pruned, "Lung Function")
corrNASpear(df.lung)

source("lib/na_count.R")
source("lib/removeCorrelated.R")


corr.anthro %>%
  gather(var1, var2, value)
graph_cors <- melt(corr.anthro) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)
