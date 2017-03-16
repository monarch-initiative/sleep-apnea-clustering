

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