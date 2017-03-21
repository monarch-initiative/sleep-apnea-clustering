#split Average Heart Rate
# Oxygen saturation
# Central Apnea
# Obstructive Apnea
# Hypopnea

df.oxysat <- basicSubset(datadict, shhs1.pruned, "Oxygen Saturation")

#remove min and max
df.min<-lookUpDispname(datadict, "Minimum")
df.max<-lookUpDispname(datadict, "Maximum")
terms_min<-pullTerms(df.min, df.oxysat)
terms_max<-pullTerms(df.max, df.oxysat)
terms_minmax<-c(terms_min, terms_max)

#w/ w/o arousals
df.arous <- lookUpDispname(datadict, "w/ arousals")
terms_arous<-pullTerms(df.arous, df.oxysat)

#NREM REM
df.nrem <- lookUpDispname(datadict, "NREM")
terms_nrem<-pullTerms(df.nrem, df.oxysat)

#Back Other
df.back <- lookUpDispname(datadict, "Back")
terms_back <- pullTerms(df.back, df.oxysat)

#all 2 3 4 5%
df.2p<-lookUpDispname(datadict, "2%")
terms_2p<-pullTerms(df.2p, df.oxysat)
df.3p<-lookUpDispname(datadict, "3%")
terms_3p<-pullTerms(df.3p, df.oxysat)
df.4p<-lookUpDispname(datadict, "4%")
terms_4p<-pullTerms(df.4p, df.oxysat)
df.5p<-lookUpDispname(datadict, "5%")
terms_5p<-pullTerms(df.5p, df.oxysat)

shhs1.avg<-removeCols(df.oxysat, terms_minmax)
shhs1.avg.noarous<-removeCols(shhs1.avg, terms_arous)
shhs1.avg.noarous.2p<-subsetCols(shhs1.avg.noarous, terms_2p)
shhs1.avg.noarous.5p<-subsetCols(shhs1.avg.noarous, terms_5p)

shhs1.avg.noarous.back<-subsetCols(shhs1.avg.noarous, terms_back)
shhs1.avg.noarous.other<-removeCols(shhs1.avg.noarous, terms_back)

# reorganize data
long.df <- shhs1.noarous.avg.nrem %>% 
  select(-avsao2nh) %>%
  rename(avdnbpall=avdnbp, avdnopall=avdnop) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("position", "percent"), sep=6) %>%
  mutate(position = as.factor(position), percent=as.factor(percent))

long.df <- shhs1.avg.noarous.other %>% 
  select(starts_with("avd")) %>%
  rename(avdnop0=avdnop, avdrop0=avdrop) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("stage", "percent"), sep=4) %>%
  mutate(stage = as.factor(stage), percent=as.factor(percent))

long.df <- shhs1.avg.noarous.5p %>%
  select(-ndes5ph) %>%
  gather(col_name, value) %>%
  separate(col_name, into=c("stage", "position"), sep=4) %>%
  mutate(stage=as.factor(stage), position=as.factor(position))

boxplot(shhs1.avg.noarous.other %>%
  select(starts_with("ndes")))

boxplot(shhs1.avg.noarous.other %>%
  select(contains("sao2")) %>%
  mutate_each(funs(log)))

#boxplot, scatterplot
ggplot(long.df, aes(y=log(value), x=stage, fill=percent))+geom_boxplot()
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
  select(age_s1, bmi_s1, neck20, hip, waist, starts_with("avdn"), starts_with("sao2"))