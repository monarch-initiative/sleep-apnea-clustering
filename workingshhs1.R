#SHHS1 column selection

source("lib/isgreaterthanzero.R")
source("lib/recode8asna.R")
source("lib/recode8asgauss.R")
source("lib/recode2as0.R")
source("lib/getYNterms.R")

#get rid of any columns with more than 50% "NA" (missing values)
df.halfna<-shhs1.pruned %>%
  select(which(colMeans(is.na(.)) < 0.5))

df.demo<-basicSubset(datadict, df.halfna, "Demographics")
df.demo.mod<-df.demo %>% 
  select(-pptid, -age_category_s1, -race, -ethnicity, -mstat, -educat) %>%
  mutate_each(funs(unlist(lapply(., recode2as0))), gender) %>%
  mutate(gender=as.factor(gender)) %>%
  select(obf_pptid, age_s1, gender)

df.ecg <- basicSubset(datadict, df.halfna, "ECG")
terms_ecg.yn <-getYNterms(datadict, df.ecg)
terms_ecg.yn <-c("obf_pptid", terms_ecg.yn)
df.ecg.yn<-subsetCols(df.ecg, terms_ecg.yn)


df.ecg.yn.clean <-df.ecg.yn %>%
  select(-wpw, -part2deg, -mob1, -mob2, -rvh,
         -av3deg, -rtrial) %>%
  mutate_each(funs(factor), -obf_pptid)

terms_ecg.yn <-getYNterms(datadict, df.ecg)
df.ecg.other<-removeCols(df.ecg, terms_ecg.yn)
df.ecg.other %>%
  select(obf_pptid, antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor))

df.ecg.other.clean<-df.ecg.other %>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  #select(antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor),-obf_pptid)

df.ecg.clean<-left_join(df.ecg.yn.clean, df.ecg.other.clean)
df.ecg.clean <- df.ecg.clean %>%
  select(-lah, -paced, -afib, -ilbbb, -lbbb, -iventblk, -nonsp_st, -rbbb, 
         -irbbb, -lvh3_3, -antlatmi) %>%
  select(obf_pptid, antsepmi)

df.anthro1 <-basicSubset(datadict, df.halfna, "Anthropometry")
# pm202 = weight, pm207 = height, pm212abc= neck

df.anthro<-df.anthro1 %>%
  select(obf_pptid, bmi_s1)

#fix mistaken entry
#df.anthro$hip[2716]=110.4

df.medalert<-basicSubset(datadict, df.halfna, "Medical Alert")

df.medalert.clean<-df.medalert %>%
  select(-hrov150) %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), -obf_pptid) %>%
  mutate_each(funs(as.factor), -obf_pptid) 


df.medalert.clean<-df.medalert.clean %>%
  select(obf_pptid, ahiov50)

df.med<-basicSubset(datadict, df.halfna, "Medications")
df.med.clean<- df.med %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  select(-anar1c1, -pvdl1, -alphad1, -anar31, -vasod1, -anar1a1, -aced1, -basq1,
         -betad1, -htnmed1, -anar1b1) %>%
  select(obf_pptid, sympth1, dig1, ccb1, niac1, ntca1,  hctzk1, #ace1
         asa1, alpha1, nsaid1, ohga1, thry1) %>%

  select(obf_pptid, sympth1)

df.medhist <- basicSubset(datadict, df.halfna, "Medical History")

df.medhist.clean<-df.medhist %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), 
              -obf_pptid) %>%
  mutate_each(funs(factor), -obf_pptid, -cgpkyr, -alcoh) %>%
  select(obf_pptid, ca15, asthma15, pacem15, htnderv_s1, runny15, cgpkyr, cabg15,
         alcoh, sa15, othrcs15) %>%
  select(obf_pptid, ca15, htnderv_s1)

df.morning <- basicSubset(datadict, df.halfna, "Morning Survey")
df.morning.clean<-df.morning %>%
  select(-pipe10) %>%
  select(obf_pptid, ltdp10, wrface10, cgrtts10, minfa10) %>%
  select(obf_pptid, wrface10)

df.health <- basicSubset(datadict, df.halfna, "Health Interview")
df.health.clean<-df.health%>%
  select(-napsmn15, -smknow15) %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), nitro15, slpill15) %>%
  mutate_each(funs(factor), -obf_pptid, -coffee15, -napshr15, 
              -soda15, -stress15, -tea15) %>%
  select(obf_pptid, evsmok15)

df.qual <- basicSubset (datadict, df.halfna, "Quality Of Life")
terms.noyes.qual<-getYNterms(datadict, df.qual)
df.qual.num<-removeCols(df.qual, terms.noyes.qual)

terms.noyes.qual<-c("obf_pptid", terms.noyes.qual)
df.qual.yes<-subsetCols(df.qual, terms.noyes.qual)
df.qual.yes %>%
  mutate_each(funs(factor), -obf_pptid)

df.qual.clean<-left_join(df.qual.yes, df.qual.num)
df.qual.clean<-df.qual.clean %>% 
  select(obf_pptid, down25, wk1blk25, exclnt25, phctdn25, carful25) %>%
  select(obf_pptid, carful25)

df.sleephabit <-basicSubset(datadict, df.halfna, "Sleep Habits")
sh_yn <-getYNterms(datadict, df.sleephabit)
df.sleephab.num<-removeCols(df.sleephabit, sh_yn)
summary(df.sleephab.num)

#get rid of cpap02, stpbrt02, surgsa02
df.sleephabit.num.clean<-df.sleephab.num %>%
  select( 
         -starts_with("tfaw"), -starts_with("twuw")) %>%
  mutate_each(funs(unlist(lapply(., recode8asGauss))), hosnr02, issnor02, loudsn02)

sh_yn<-c("obf_pptid", sh_yn)
df.sleephab.yn<-subsetCols(df.sleephabit, sh_yn)
 
df.sleephab.yn.clean<- df.sleephab.yn %>%
  select(-o2thpy02) %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), mdsa02, hvsnrd02, 
              stpbrt02) %>%
  mutate_each(funs(factor), -obf_pptid) %>% 
  select(-hvsnrd02)

df.sleephab.clean<-left_join(df.sleephabit.num.clean, df.sleephab.yn.clean)
df.sleephab.clean<-df.sleephab.clean %>%
  select(obf_pptid, loudsn02, sob02, hrswd02, wu2em02, tfa02, 
         sleepy02, mdsa02, surgtr02) %>%
  select(obf_pptid, sob02, hrswd02, wu2em02, tfa02, sleepy02, mdsa02) %>%
  select(obf_pptid, mdsa02, wu2em02)

df.bp <- basicSubset(datadict, df.halfna, "Blood Pressure")
drop.cols<-c("aai", "ankbp", "armbp", "dias120", "dias220", "dias320", "syst220",
             "syst120", "syst320")
df.bp<-removeCols(df.bp, drop.cols)

#bloods
df.blood <- basicSubset(datadict, df.halfna, "Bloods")
df.bloods.joined<-left_join(df.blood, df.bp)
df.bloods.clean<-df.bloods.joined %>%
  select(obf_pptid, chol)

#lung function
df.lung <- basicSubset(datadict, shhs1.pruned, "Lung Function")
df.lung[1572,]<-NA
df.lung[2330,]<-NA
df.lung.clean<-df.lung %>%
  select(obf_pptid, fvc)

df.sleeparch<-basicSubset(datadict, df.halfna, "Sleep Architecture")
df.sleeparch.clean<-df.sleeparch %>%
  select(-starts_with("st"), -starts_with("times"), -starts_with("timer")) %>%
  select(-slptime, -sleep_latency) %>%
  select(obf_pptid, time_bed, nsupinep, pcstahda, pctsa75h, tmstg2p,
         remlaip, pctsa90h, slpeffp, hremt1p, hremt2p, hremt34p, slplatp,
         tmremp, pcstah3d, scstg1p, scstg2p, slptawp) %>%
  select(obf_pptid, pcstahda, scstg1p, tmremp, scstg2p, pctsa75h, hremt34p, slplatp)

df.ess1<- basicSubset(datadict, df.halfna, "Epworth Sleepiness Scale")
df.ess<-df.ess1 %>% select(obf_pptid, ess_s1)

#SF-36
df.sf36<-basicSubset(datadict, df.halfna, "SF-36")
df.sf36.clean<-df.sf36 %>% 
  select(-starts_with("raw")) %>%
  select(obf_pptid, mcs_s1, pcs_s1)

df.index <-basicSubset(datadict, df.halfna, "Indexes")
df.index.clean<-df.index %>%
  select(-ends_with("a")) %>% #arousals

  select(obf_pptid, rdirem2p)

df.oxysat <- basicSubset(datadict, df.halfna, "Oxygen Saturation")

df.oxysat.clean<-df.oxysat %>%
  select(obf_pptid, avdnbp, avdrop, ndes3ph, avsao2nh, mnsao2nh, mnsao2rh) %>%
  select(obf_pptid, avsao2nh)

df.hr <- basicSubset(datadict, df.halfna, "Heart Rate")

df.hr.clean<-df.hr %>%
  select(obf_pptid, davbnoh)

df.arousal <-basicSubset(datadict, df.halfna, "Arousals")

df.arousal.clean<-df.arousal %>% 
  select(obf_pptid, ai_nrem, ai_rem, arrembp) %>%
  select(obf_pptid, arrembp)

df.joined<-left_join(df.demo.mod, df.anthro)
#df.joined<-left_join(df.joined, df.ecg.clean)
#df.joined<-left_join(df.joined, df.medalert.clean)
df.joined<-left_join(df.joined, df.med.clean)
df.joined<-left_join(df.joined, df.medhist.clean)
df.joined<-left_join(df.joined, df.morning.clean)
df.joined<-left_join(df.joined, df.health.clean)
df.joined<-left_join(df.joined, df.qual.clean)
df.joined<-left_join(df.joined, df.sleephab.clean)
df.joined<-left_join(df.joined, df.sleeparch.clean)

df.joined<-left_join(df.joined, df.lung.clean)
df.joined<-left_join(df.joined, df.bloods.clean)
df.joined<-left_join(df.joined, df.ess)
df.joined<-left_join(df.joined, df.sf36.clean)

df.joined<-left_join(df.joined, df.arousal.clean)
df.joined<-left_join(df.joined, df.hr.clean)
df.joined<-left_join(df.joined, df.oxysat.clean)
df.joined<-left_join(df.joined, df.index.clean)

df<-df.joined %>% select(-obf_pptid) %>% na.omit





