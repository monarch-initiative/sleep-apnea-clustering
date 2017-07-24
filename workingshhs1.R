is.greaterThanZero<-function(x){
  if (is.na(x)){
    y=NA
  } else
    if (x>0){
      y=1
    } else{
      y=0
    }
  return (y)
}

recode8asNA<-function(x){
  if (is.na(x)){
    y=NA
  } else
    if (x==8){
      y=NA
    } else
      y=x
    return (y)
}

recode8asGauss<-function(x){
  if (is.na(x)){
    y=NA
  } else
    if (x==8){
      y <- sample(1:4, 1)
    } else
      y=x
    return (y)
}

df.halfna<-shhs1.pruned %>%
  select(which(colMeans(is.na(.)) < 0.5))

source("lib/recode2as0.R")
source("lib/getYNterms.R")

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

#remove av3deg, mob1, mob2, part2deg, rvh, wpw
df.ecg.yn.clean <-df.ecg.yn %>%
  select(-wpw, -part2deg, -mob1, -mob2, -rvh,
         -av3deg, -rtrial) %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  summary

#Keep: nonsp_tw, lvhst, antsepmi
#remove: nonsp_st, st4_1_3, 
terms_ecg.yn <-getYNterms(datadict, df.ecg)
df.ecg.other<-removeCols(df.ecg, terms_ecg.yn)
df.ecg.other %>%
  select(obf_pptid, antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor)) %>%
  #na.omit %>%
  summary

df.ecg.other.clean<-df.ecg.other %>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  #select(antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor),-obf_pptid)

df.ecg.clean<-left_join(df.ecg.yn.clean, df.ecg.other.clean)
df.ecg.clean <- df.ecg.clean %>%
  select(-lah, -paced, -afib, -ilbbb, -lbbb, -iventblk, -nonsp_st, -rbbb, 
         -irbbb, -lvh3_3, -antlatmi) %>%
  select(obf_pptid, antsepmi)

#htnderv_s1 ccb ace hctzk (all hypertension drugs)
#sa15 mds02 (MD diagnosis of sleep apnea)
#asa15 asa1 (aspirin, anti-inflam)
#infmi nonsp_tw (ecg stuff)
#gender alcoh
#phctdn25 -wk1blk25
#wu2em02 tfa02
#pctsa75h -ohga1 (diabetes drug)
#parrptdiab -hremt34p
#cgrtts10 coffee15
#tmstg2p -tmremp
#cough315 
#time_bed scstg1p
#happy25 
#slplatp -slpeffp
#remlaip 
#bmi_s1
#pcstahda

df.anthro1 <-basicSubset(datadict, df.halfna, "Anthropometry")
# pm202 = weight, pm207 = height, pm212abc= neck

df.anthro<-df.anthro1 %>%
  select(obf_pptid, bmi_s1)

#fix mistaken entry
df.anthro$hip[2716]=110.4

df.medalert<-basicSubset(datadict, df.halfna, "Medical Alert")

df.medalert.clean<-df.medalert %>%
  select(-hrov150) %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), -obf_pptid) %>%
  mutate_each(funs(as.factor), -obf_pptid) %>%
  summary
#2

#after FA

df.medalert.clean<-df.medalert.clean %>%
  select(obf_pptid, ahiov50)

df.med<-basicSubset(datadict, df.halfna, "Medications")

#alphad2, ccbt2, dihir2, hprns2, kblkr2, -mlpd2, pvdl2, prob2, urcos2, vasod2,
#wtls2
df.med.clean<- df.med %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  select(-anar1c1, -pvdl1, -alphad1, -anar31, -vasod1, -anar1a1, -aced1, -basq1,
         -betad1, -htnmed1, -anar1b1) %>%
  select(obf_pptid, sympth1, dig1, ccb1, niac1, ntca1,  hctzk1, #ace1
         asa1, alpha1, nsaid1, ohga1, thry1) %>%
  #select(obf_pptid, ace1, alpha1, hctzk1, sympth1, ohga1, ntca1, thry1, nsaid1) %>%
  select(obf_pptid, sympth1)

#estrgn1 progrest premarin
#sympth1 istrd1 pdei1
#dig1 warf1 loop1
#ccb1 ccbir ccbsr
#niac1 lipid1 
#vaso1 diuret1
#ace1  insuln1
#ntca1 benzod1
#asa1 ntg1  
#hctzk1 -hctz1
#alpha1 
#nsaid1
#ohga1
#thry1

df.medhist <- basicSubset(datadict, df.halfna, "Medical History")

df.medhist.clean<-df.medhist %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), 
              -obf_pptid) %>%
  mutate_each(funs(factor), -obf_pptid, -cgpkyr, -alcoh) %>%
  #select(obf_pptid, asthma15, ca15, cabg15, copd15, stroke15, parrptdiab, hf15, angina15, cabg15, 
  #       mi15, pacem15, sa15, htnderv_s1, runny15) %>%
  select(obf_pptid, ca15, asthma15, pacem15, htnderv_s1, runny15, cgpkyr, cabg15,
         alcoh, sa15, othrcs15) %>%
  select(obf_pptid, ca15, htnderv_s1)
  summary

#ca15 angina15 mi15   
#asth1215 asthma15 copd15 
#pacem15 hf15  
#srhype htnderv_s1
#runny15 sinus15
#cgpkyr emphys15
#sa15 -cabg15
#alcoh gender
#cough315 phlegm15
#bmi_s1 parrptdiab

df.morning <- basicSubset(datadict, df.halfna, "Morning Survey")
df.morning.clean<-df.morning %>%
  select(-pipe10) %>%
  select(obf_pptid, ltdp10, wrface10, cgrtts10, minfa10) %>%
  select(obf_pptid, wrface10)

#pcstahda ndes3ph oai4pa
#ltdp10 rest10 shlg10 
#fev1
#wrface10 wrhead10 vest10
#minfa10 diffa10 slplatp
#happy25 
#phctdn25 -pcs_s1
#wu2em02 tfa02
#asa15 cigars10
#cai4p cent_obs_ratio
#time_bed hwlghr10
#scstg1p
#chol trig
#arremop nsupinep
#cgrtts10 

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
  mutate_each(funs(factor), -obf_pptid) %>%
  summary

df.qual.clean<-left_join(df.qual.yes, df.qual.num)
df.qual.clean<-df.qual.clean %>% 
  select(obf_pptid, down25, wk1blk25, exclnt25, phctdn25, carful25) %>%
  select(obf_pptid, carful25)

#pcstahda rdi4pns oai4pa ndes3ph
#happy25 -mcs_s1 down25 blue25
#wk1blk25 wksblk25 climb125 pcs_s1
#exclnt25 genhth25
#phctdn25 phacls25
#mdsa02 
#fvc gender
#ahi_c0h4a rdirem2p
#cai4p  cent_obs_ratio
#wu2em02 tfa02
#tmstg2p
#chol
#slpeffp
#hremt34p
#nsupinep arremop

df.sleephabit <-basicSubset(datadict, df.halfna, "Sleep Habits")

sh_yn <-getYNterms(datadict, df.sleephabit)
df.sleephab.num<-removeCols(df.sleephabit, sh_yn)

summary(df.sleephab.num)

#get rid of cpap02, stpbrt02, surgsa02
df.sleephabit.num.clean<-df.sleephab.num %>%
  select( 
         -starts_with("tfaw"), -starts_with("twuw")) %>%
  mutate_each(funs(unlist(lapply(., recode8asGauss))), hosnr02, issnor02, loudsn02) %>%
  select(which(colMeans(is.na(.)) < 0.5)) %>%
  #boxplot
  #mutate_each(funs(factor)) %>%
  summary

boxplot(df.sleephab.num %>% 
          select(hosnr02, issnor02, loudsn02) %>%
          mutate_each(funs(unlist(lapply(., recode8asNA)))), 
        main="before imputation")

boxplot(df.sleephab.num %>% 
          select(hosnr02, issnor02, loudsn02) %>%
          mutate_each(funs(unlist(lapply(., recode8asGauss)))), 
        main="after imputation")

hist(df.sleephabit.num.clean$loudsn02)

sh_yn<-c("obf_pptid", sh_yn)
df.sleephab.yn<-subsetCols(df.sleephabit, sh_yn)
df.sleephab.yn %>%
  mutate_each(funs(factor)) %>%
  summary  
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
#sob02 cp02
#loudsn02 hosnr02 
#funres02 sleepy02
#hrswd02 hrswe02
#wu2em02 wudnrs02
#tfa02 mi2slp02
#surgtr02 
#mdsa02 


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
#avdnbp3 oai4pa 
#ahi_c0h4a rdirem2p
#fvc fev1 
#funres02 ess_s1
#tmstg2p -tmremp
#cai4p cent_obs_ratio
#wu2em02 hrswd02
#scstg1p
#time_bed
#nsupinep arremop
#diasbp
#chol trig
#mdsa02



df.sleeparch<-basicSubset(datadict, df.halfna, "Sleep Architecture")

df.sleeparch.clean<-df.sleeparch %>%
  select(-starts_with("st"), -starts_with("times"), -starts_with("timer")) %>%
  select(-slptime, -sleep_latency) %>%
  select(obf_pptid, time_bed, nsupinep, pcstahda, pctsa75h, tmstg2p,
         remlaip, pctsa90h, slpeffp, hremt1p, hremt2p, hremt34p, slplatp,
         tmremp, pcstah3d, scstg1p, scstg2p, slptawp) %>%
  select(obf_pptid, pcstahda, scstg1p, tmremp, scstg2p, pctsa75h, hremt34p, slplatp)
  #select(obf_pptid, pcstahda, pctsa90h, remlaip, nsupinep, slptawp, scstg1p, scstg2p, 
  #       slplatp, hremt1p, hremt34p,
  #       slpeffp, tmremp)

#pctlt75 pctsa75h pctsa80h
#timebedp time_bed
#nsupinep nremepop
#pcstahda pcstah3d
#slpeffp slp_eff
#tmstg2p scstg2p
#remlaip rem_lat1 remlaiip
#pctsa90h pctlt90
#scstg1p tmstg1p
#slplatp slp_lat
#tmremp scremp
#remt2p hremt2p 
#remt1p hremt1p
#remt34p hremt34p 
#pctsthyp pctsa95h

#ahi_a0h4 pcstah3d 
#pctsa80h pctlt80 
#nsupinep 
#time_bed
#slpeffp
#rdirem2p
#scstg2p
#pctsthyp rdi0p
#hslptawp slptawp scstg1p tmstg1p
#remlaip
#rdi2ps
#cai0p 
#remt1p hremt1p
#remt34p 
#remt2p
df.ess1<- basicSubset(datadict, df.halfna, "Epworth Sleepiness Scale")
df.ess<-df.ess1 %>% select(obf_pptid, ess_s1)

#SF-36
df.sf36<-basicSubset(datadict, df.halfna, "SF-36")
df.sf36.clean<-df.sf36 %>% 
  select(-starts_with("raw")) %>%
  select(obf_pptid, mcs_s1, pcs_s1)

#ai_nrem ai_all ahnremop
#pcs_s1 (everything except mh re)
#avdnbp3 avdrbp2 oai4pa
#rdirem2p ahi_c0h4a 
#mcs_s1 mh re
#ai_rem arremop 
#cai4p gender
#arrembp
#ess_s1
#trig
#diasbp
#davnoh
#chol trig


df.index <-basicSubset(datadict, df.halfna, "Indexes")

df.index.clean<-df.index %>%
  select(-ends_with("a")) %>%
  #select(obf_pptid, ahi_a0h4, ahi_a0h3) %>%
  select(obf_pptid, ahi_c0h4, rdirem2p) #,
  #       rdi2ps, cai0p) %>%
  select(obf_pptid, rdirem2p)

#ahi_c0h4a ahi_a0h3a ahi_c0h3a 
#oai4pa oai4p oai0p
#rdi3ps rdi2ps rdi0ps rdi4ps
#rdirem2p rdirem3p
#cai4p cai4pa cai0p
#rdi4pns rdi3pns
#rdi0pns rdi0p
#cent_obs_ratio cent_obs_ratioa

df.oxysat <- basicSubset(datadict, df.halfna, "Oxygen Saturation")

df.oxysat.clean<-df.oxysat %>%
  select(obf_pptid, avdnbp, avdrop, ndes3ph, avsao2nh, mnsao2nh, mnsao2rh) %>%
  select(obf_pptid, avsao2nh)
#avdnbp3 avdnbp2 avdnbp4 avdnbp avdnop2
#avdrop3 all other avdrop
#avsao2nh, rh, sao2nrem, sao2rem
#ndes3ph (2,4,5)
#avdrbp2 avdrbp

df.hr <- basicSubset(datadict, df.halfna, "Heart Rate")

df.hr.clean<-df.hr %>%
  select(obf_pptid, davbnoh)

#davbnoh savbroh savbnoh davbroh  aavbnoh aavbroh
#smxbroh dmxbnoh
#smnbnoh
#ndes3ph rdi4pns
#cai4p gender
#bmi_s1 ahi

#remove min and max
df.min<-lookUpDispname(datadict, "Minimum")
df.max<-lookUpDispname(datadict, "Maximum")
terms_min<-pullTerms(df.min, df)
terms_max<-pullTerms(df.max, df)
terms_minmax<-c(terms_min, terms_max)

#w/ w/o arousals
df.arous <- lookUpDispname(datadict, "w/ arousals")
terms_arous<-pullTerms(df.arous, df)

df.avg<-removeCols(df, terms_minmax)
df.avg.noarous<-removeCols(df.avg, terms_arous)
df<-df.avg.noarous

df.arousal <-basicSubset(datadict, df.halfna, "Arousals")

#ai_nrem ai_all arnremop ahnremop
#avdnbp3 avdrbp2
#arremop ai_rem ahrembp
#cai4p gender
#arrembp 
#age
#davbnoh 
#ahnrembp

#pcstah3d pcstahda ahi_c0h4 respiratory index
#alpha1 (hypertension) ntca1 (depression) parrptdiab (diabetes)
#asa1 (meds, anti-inflam)
#-pcs_s1 (physical sf36) -wk1blk25 (walking a block) phctdn25 (limit time for work)
#-asthma15 -sympth1 (hypotension, hf) (asthma)
#-hremt2p (rem to stage 2 shifts) slptawp (sleep to awake shifts)
#hctzk1 (hypertension) htnderv_s1
#fvc gender
#scstg2p (percent of time in stage 2) tmstg2p (time in stage 2)
#-mcs_s1 (mental component sf36) happy25
#wu2em02 (waking up too early, unable to go back to sleep) -hrswd02 (hrs on weekdays)
#pctsa90h -avsao2nh
#davbnoh nsaid1 (inflamation)
#arrembp -nsupinep
#cough315 runny15
#-ccb1 (hyp) niac1 (inflam) chol 
#cgrtts10 cgpkyr 
#-slplatp
#ai_rem 
#loudsn02
#-diasbp
#hremt34p wrface10 (face mask disturbs)

df.arousal.clean<-df.arousal %>% 
  select(obf_pptid, ai_nrem, ai_rem, arrembp) %>%
  select(obf_pptid, arrembp)

df.joined<-left_join(df.demo.mod, df.anthro)
df.joined<-left_join(df.joined, df.ecg.clean)
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


