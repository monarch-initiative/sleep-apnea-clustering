#split Average Heart Rate
# Oxygen saturation
# Central Apnea
# Obstructive Apnea
# Hypopnea

df.oxysat <- basicSubset(datadict, shhs1.pruned, "Oxygen Saturation")
df.centap <-basicSubset(datadict, shhs1.pruned, "Central")
df.obap <-basicSubset(datadict, shhs1.pruned, "Obstructive")
df.hypop <-basicSubset(datadict, shhs1.pruned, "Hypopnea")
df.hr <- basicSubset(datadict, shhs1.pruned, "Heart Rate")
df.arousal <-basicSubset(datadict, shhs1.pruned, "Arousals")
df.index <-basicSubset(datadict, shhs1.pruned, "Indexes")

shhs1.avg.noarous %>%
  select(-obf_pptid) %>%
  summary

df<-df.oxysat
#remove min and max
df.min<-lookUpDispname(datadict, "Minimum")
df.max<-lookUpDispname(datadict, "Maximum")
terms_min<-pullTerms(df.min, df)
terms_max<-pullTerms(df.max, df)
terms_minmax<-c(terms_min, terms_max)

#w/ w/o arousals
df.arous <- lookUpDispname(datadict, "w/ arousals")
terms_arous<-pullTerms(df.arous, df)

#NREM REM
df.nrem <- lookUpDispname(datadict, "NREM")
terms_nrem<-pullTerms(df.nrem, df)

#Back Other
df.back <- lookUpDispname(datadict, "Back")
terms_back <- pullTerms(df.back, df)

#all 2 3 4 5%
df.2p<-lookUpDispname(datadict, "2%")
terms_2p<-pullTerms(df.2p, df)
df.3p<-lookUpDispname(datadict, "3%")
terms_3p<-pullTerms(df.3p, df)
df.4p<-lookUpDispname(datadict, "4%")
terms_4p<-pullTerms(df.4p, df)
df.5p<-lookUpDispname(datadict, "5%")
terms_5p<-pullTerms(df.5p, df)

shhs1.avg<-removeCols(df, terms_minmax)
shhs1.avg.noarous<-removeCols(shhs1.avg, terms_arous)
shhs1.avg.noarous.2p<-subsetCols(shhs1.avg.noarous, terms_2p)
shhs1.avg.noarous.5p<-subsetCols(shhs1.avg.noarous, terms_5p)

shhs1.avg.noarous.back<-subsetCols(shhs1.avg.noarous, terms_back)
shhs1.avg.noarous.other<-removeCols(shhs1.avg.noarous, terms_back)

shhs1.avg.noarous.nrem<-subsetCols(shhs1.avg.noarous, terms_nrem)

# reorganize data
long.df <- shhs1.avg.noarous.nrem %>% 
  select(starts_with("av")) %>%
  rename(avhnbp0=avhnbp, avhnop0=avhnop) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("position", "percent"), sep=5) %>%
  mutate(position = as.factor(position), percent=as.factor(percent))

long.df <- shhs1.avg.noarous.back %>% 
  select(starts_with("av")) %>%
  rename(avhnbp0=avhnbp, avhrbp0=avhrbp) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("stage", "percent"), sep=4) %>%
  mutate(stage = as.factor(stage), percent=as.factor(percent))

long.df <- shhs1.avg.noarous.5p %>%
  select(-ndes5ph) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("stage", "position"), sep=4) %>%
  mutate(stage=as.factor(stage), position=as.factor(position))


  
long.df <- shhs1.avg.noarous.2p %>%
  select(starts_with("av")) %>% #select(-ndes5ph) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("stage", "position"), sep=4, remove=TRUE) %>%
  mutate(stage=as.factor(stage), position=as.factor(position))

shhs1.avg.noarous.2p %>% 
  select(-starts_with("av")) %>%
  na.omit %>%
  #summary
  boxplot

boxplot(shhs1.avg.noarous.other %>%
  select(starts_with("ndes")))

boxplot(shhs1.avg.noarous.other %>%
  select(contains("sao2")) %>%
  mutate_each(funs(log)))

boxplot(shhs1.avg.noarous.nrem %>%
          select(-starts_with("av")))
#boxplot, scatterplot
ggplot(long.df, aes(y=log(value), x=stage, fill=position))+geom_boxplot()
ggplot(long.df, aes(y=log(value), x=stage, fill=percent))+geom_boxplot()
ggplot(long.df, aes(y=log(value), x=position, fill=percent))+geom_boxplot()

ggplot(shhs1.avg.noarous.5p, aes(y=avdnbp5, x=avdrop5)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)



#correlation
cor(na.omit(shhs1.avg.noarous.5p))

long.shhs.oxysat<-df.oxysat %>% 
  select(-obf_pptid) %>% 
  gather(BP, mmhg, diasbp:systbp)

#join with demo anthro subset
joined.demo.anthro.oxysat<-left_join(joined.demo.anthro, shhs1.avg.noarous)

sampledat<-joined.demo.anthro.oxysat %>%
  group_by(age_category_s1)

ggplot(data=sampledat, aes(y=avdnbp, x=as.factor(age_category_s1))) + geom_boxplot()

doubledata<-joined.demo.anthro.oxysat %>%
  select(which(sapply(., is.double)))

cor(na.omit(doubledata))
melted<-melt(cor(na.omit(sampledata)))
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile")
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

sampledata<-joined.demo.anthro.oxysat %>%
  select(age_s1, bmi_s1, neck20, hip, waist, starts_with("avdn"), 
         starts_with("sao2"))

# example data
set.seed(1)
DF <- data.frame(x=sample(c("Y","N"),100,T),y=sample(c("Y","N"),100,T))

# how to get correlation
DF[] <- lapply(DF,as.integer)
cor(DF)
#            x          y
# x  1.0000000 -0.0369479
# y -0.0369479  1.0000000

# visualize it
library(corrplot)
corrplot(cor(DF))

DF <- data.frame(v1 = sample(c("Y","N"), 100, T),
                 v2 = sample(c("Y","N"), 100, T),
                 v3 = sample(c("Y","N"), 100, T),
                 v4 = sample(c("Y","N"), 100, T),
                 v5 = sample(c("Y","N"), 100, T))
DF[] <- lapply(DF,as.integer)
library(sjPlot)
sjp.corr(DF)
sjt.corr(DF)

long.df <- shhs1.avg %>% 
  select(-obf_pptid) %>%
  #rename(avhnbp0=avhnbp, avhnop0=avhnop) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("stage", "position"), sep=5) %>%
  mutate(stage= as.factor(stage), position=as.factor(position))

ggplot(data=long.df, aes(y=value, x=stage, fill=position)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

long.df<-df.index %>%
  select(starts_with("rdi"), -obf_pptid, -ends_with("a")) %>%
  gather(col_name, value)

ggplot(data=long.df, aes(y=value, x=col_name)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df.index %>%
  select(starts_with("ahi"), -obf_pptid) %>%
  boxplot

df.index %>%
  select(starts_with("c")) %>% boxplot
  gather(col_name, value)
  
df.resp <- df.index %>%
  left_join(df.oxysat) %>%
  left_join(df.centap) %>%
  left_join(df.obap) %>%
  left_join(df.hypop) %>%
  left_join(df.hr) %>%
  left_join(df.arousal)

resp_corr<-corrNASpear(df.oxysat %>% select (-obf_pptid))
melted<-melt(resp_corr)
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#find most correlated features
# cutoff  abs .75?
#which one has more missing values?
#which one has less variability?
# eliminate redundant feat w more missing values and less variance

melteddesc<-melted %>%
  arrange(desc(value)) %>%
  filter(abs(value)>.90) %>%
  filter(Var1 != Var2)

#oahi
#rdi3p
terms_to_remove<-unique(pull(melteddesc, Var1))
terms_to_remove1<-sapply(terms_to_remove, as.character)
correlated<-df.oxysat %>%
  select(one_of(terms_to_remove1))




na_count <- function (y) {
  z<-sum(length(which(is.na(y))))
  return(z)
}
na_count(correlated[,1])
nas<-sapply(correlated, na_count)
data.frame(nas)


correlated.nona<-removeCols(correlated, names(which(nas>0)))

corrdf<-sapply(correlated.nona, IQR, na.rm=TRUE)

rangedf<-sapply(correlated.nona, range, na.rm=TRUE)
variation<-corrdf/(rangedf[2, ]-rangedf[1, ])

keeplist<-c(names(which.max(variation)), 
            names(which.max(rangedf[2,]-rangedf[1,])), 
            names(which.max(corrdf)))
setdiff(terms_to_remove1, keeplist)
removeCols(df.oxysat, setdiff(terms_to_remove1, keeplist))

df.oxysat.nocor<-removeCorrelated(df.oxysat)
df.index.nocor<-removeCorrelated(df.index)

mat<-left_join(df.oxysat.nocor, df.index.nocor)
resp_corr<-corrNASpear(mat)
melted<-melt(resp_corr)
qplot(x=Var1, y=Var2, data=melted, fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
