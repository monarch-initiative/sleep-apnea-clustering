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

df.halfna<-shhs2.pruned %>%
  select(which(colMeans(is.na(.)) < 0.5))

df.demo<-basicSubset(datadict, shhs2.pruned, "Demographics")
df.demo.mod<-df.demo %>% 
  select(-pptid, -age_category_s2, -race) %>%
  mutate_each(funs(unlist(lapply(., recode2as0))), gender) %>%
  mutate(gender=as.factor(gender)) %>%
  select(-age_s1)

df.ecg <- basicSubset(datadict, shhs2.pruned, "ECG")
source("lib/getYNterms.R")
terms_ecg.yn <-getYNterms(datadict, df.ecg)
terms_ecg.yn <-c("obf_pptid", terms_ecg.yn)
df.ecg.yn<-subsetCols(df.ecg, terms_ecg.yn)

#remove av3deg, mob1, mob2, part2deg, rvh, wpw
df.ecg.yn.clean <-df.ecg.yn %>%
  select(-wpw, -part2deg, -mob1, -mob2, -rvh,
         -av3deg) %>%
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
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid, -qrs, -ventrate) %>%
  #select(antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor), -qrs, -ventrate, -obf_pptid) %>%
  mutate_each(funs(as.numeric), qrs, ventrate)

df.ecg.clean<-left_join(df.ecg.yn.clean, df.ecg.other.clean)

df.ecg.clean<-df.ecg.clean %>%
  select(-afib, -apbs, -ilbbb, -irbbb, -iventblk, -lah, -lbbb, -lvh3_1, -lvh3_3,
         -lvhst, -nodal, -paced, -rbbb, -rtrial, -st4_1_3, -st5_1_3) %>%
  select(obf_pptid, infmi, nonsp_st, av1deg)

#nonsp_st nonsp_tw
#infmi antsepmi
#av1deg
#vpbs
#antlatmi

df.anthro2 <-basicSubset(datadict, df.halfna, "Anthropometry")
# pm202 = weight, pm207 = height, pm212abc= neck

df.anthro<-df.anthro2 %>%
  mutate(neck=(pm212a+pm212b+pm212c)/3) %>%
  rename(weight=pm202, height=pm207)



df.anthro<-df.anthro2 %>%
  mutate(neck=(pm212a+pm212b+pm212c)/3) %>%
  rename(weight=pm202, height=pm207) %>%
  select(-starts_with("pm")) %>%
  select(-weight, -height, -neck)

df.medalert<-basicSubset(datadict, df.halfna, "Medical Alert")

#drop hrund30, treat 8 as NA
#drop medalert altogether, too many NA
df.medalert.clean<-df.medalert %>%
  select(-hrund30) %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), -obf_pptid) %>%
  mutate_each(funs(as.factor), -obf_pptid) %>%
  summary

df.med<-basicSubset(datadict, df.halfna, "Medications")

df.med %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  summary

#alphad2, ccbt2, dihir2, hprns2, kblkr2, -mlpd2, pvdl2, prob2, urcos2, vasod2,
#wtls2
df.med.clean<- df.med %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  select(-pvdl2, -prob2, -alphad2, -ccbt2, -dihir2, -hprns2, -kblkr2, -mlpd2, 
         -urcos2, -vasod2, -wtls2) %>%
  select(-a2a2, -a2ad2, -aced2, -adpi2, -agdi2, -alzh2, -amlod2, -anar1a2, 
         -anar1b2, -anar1c2, -anar32, -basq2, -benzod2, -betad2, -ccbir2, 
         -dihsr2, -dltir2, -edd2, -fibr2, -iprtr2, -nifir2, -oaia2, -slf12,
         -thzd2, -verir2, -bgnd2, -dltsr2, -insuln2, -niac2, -nifsr2, -otch2b2,
         -tca2, -versr2, -ccb2, -kspr2, -slf22, -xoi2, -prknsn2) %>%
  select(obf_pptid, sympth2, lipid2, dig2, vaso2, thry2,
         hctz2, cox22, ohga2, asa2, ntca2, ccbsr2)

#sympth2 istrd2 both .914, estrgn2 with gender, dig2 with nonsp_st, ohga2 with bmi

#agdi2 <fctr>, alpha2 <fctr>,
#   alzh2 <fctr>, amlod2 <fctr>, anar1a2 <fctr>, anar1b2 <fctr>, anar1c2 <fctr>,
#   anar32 <fctr>, asa2 <fctr>, basq2 <fctr>, benzod2 <fctr>, beta2 <fctr>,
#   betad2 <fctr>, bgnd2 <fctr>, ccb2 <fctr>, ccbir2 <fctr>, ccbsr2 <fctr>,
#   cox22 <fctr>, dig2 <fctr>, dihsr2 <fctr>, diur2 <fctr>, dltir2 <fctr>,
#   dltsr2 <fctr>, edd2 <fctr>, estrgn2 <fctr>, fibr2 <fctr>, h2b2 <fctr>,
#   hctz2 <fctr>, hctzk2 <fctr>, htnmed2 <fctr>, insuln2 <fctr>, iprtr2 <fctr>,
#   istrd2 <fctr>, kcl2 <fctr>, kspr2 <fctr>, lipid2 <fctr>, loop2 <fctr>,
#   niac2 <fctr>, nifir2 <fctr>, nifsr2 <fctr>, nsaid2 <fctr>, ntca2 <fctr>,
#   ntg2 <fctr>, oaia2 <fctr>, ohga2 <fctr>, ostrd2 <fctr>, otch2b2 <fctr>,
#   pdei2 <fctr>, ppi2 <fctr>, premar2 <fctr>, prknsn2 <fctr>, progst2 <fctr>,
#   slf12 <fctr>, slf22 <fctr>, sttn2 <fctr>, sympth2 <fctr>, tca2 <fctr>,
#   thry2 <fctr>, thzd2 <fctr>, vaso2 <fctr>, verir2 <fctr>, versr2 <fctr>,
#   warf2 <fctr>, xoi2 <fctr>

#estrg
#dig2
#sympth
#lipid
#vaso
#hctz2
#hctzk2
#cox22
#asa2
#ntca2
#thry2
#h2b2
#ccbsr2
df.medhist <- basicSubset(datadict, df.halfna, "Medical History")

df.medhist.clean<-df.medhist %>%
  rename(emphysema=hi201a, crbron=hi201b, copd=hi201c, asthma=hi201d, 
         rls=hi216) %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), 
              -obf_pptid) %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  select(obf_pptid, asthma, rls, htnderv_s2) %>%
  summary
#asthma largest, coincides with sympth, istrd2 (discovery!)
#htnderv coincides with ccbsr2, hctz2, vaso2
#dig2 coincides with nonsp_st
#bmi2 with ohga2 (diabetes)
#rls with h2b2

#ahi
#ql207
#waso
#ql205b
#coffee
#cigarettes
#totminslppctsa90h
#sh329
#qossl
#aspirin
#pipe
#cai4p
#sleptwell
#avg23bpd
#nremepbp
#rlsuml

df.morning <- basicSubset(datadict, df.halfna, "Morning Survey")
#ms212=stuffy nose yes/no
#204a quality of sleep light/deep
#204b QOS short/long
#204c QOS restless/restful
#205 QOS compared to usual
#209a wine, 209b mixed drinks, 209c beer
#210abc coffee, tea, soda
#211abc cigarettes, pipe, cigars
df.morning.clean<-df.morning %>%
  mutate(stuffy=as.factor(ms212)) %>%
  rename(qosld=ms204a, qossl=ms204b, qosrest=ms204c, qosusu=ms205, wine=ms209a,
         mixed=ms209b, beer=ms209c, coffee=ms210a, tea=ms210b, soda=ms210c, 
         cigarettes=ms211a, pipe=ms211b, cigars=ms211c) %>%
  select(-ms212) %>%
  select(obf_pptid, qossl, qosld, coffee, cigarettes, pipe, rptelaptimslpwak)

#qossl (Quality of Sleep)
#asthma, sympth2, istrd2
#htnderv
#gender
#dig2, nonsp_st
#rls
#ohga
#coffee
#rptelaptimslpwak
#cigarettes

#ahi
#ql207
#qosld
#waso -slpeffp
#totminslpwd
#ql205b
#smok20
#pctsa75h
#pctsa90h
#sh329
#pipe
#aspirin
#remlaip
#cai4p
#sh308b
#totminslp
#sleptwell

df.health <- basicSubset(datadict, df.halfna, "Health Interview")

df.health.clean<-df.health %>%
  select(-timslp) %>%
  mutate(aspirin=as.factor(hi202)) %>%
  rename(sleptwell=hi205, stress=hi207, rlsuml=hi208a, rlsunplefl=hi208b) %>%
  select(-hi202) %>%
  select(obf_pptid, aspirin, rlsuml, totminslp, sleptwell, stress) %>%
  summary

#htnderv ccbsr2, hctz2
#aspirin asa2
#asthma sympth2
#h2b2
#bmi, ohga cox22
#nonsp_st infmi
#gender vaso
#coffee rptelaptimslpwak
#ntca
#rlsuml rlsunplefl
#totminnap
#sleptwell
#cigarettes
#stress

#ahi
#ql207
#waso
#205b
#sh329
#pcts90
#aspirin
#totminslp
#pcts75
#cai4p
#remlaip
#rlsuml
#sleptwell

df.qual <- basicSubset (datadict, df.halfna, "Quality Of Life")

terms.noyes.qual<-getYNterms(datadict, df.qual)
df.qual.num<-removeCols(df.qual, terms.noyes.qual)

terms.noyes.qual<-c("obf_pptid", terms.noyes.qual)
df.qual.yes<-subsetCols(df.qual, terms.noyes.qual)
df.qual.yes %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  summary

#204a health limits time for work
#204b limits accomplishment
#204c limits kind of work activities
#204d causes difficulty performing work
#205a emotional probs limits time for work
#205b emotional limits accomplishment
#205c emo limits careful

df.qual.clean<-left_join(df.qual.yes, df.qual.num)
df.qual.clean<-df.qual.clean %>%
  select(obf_pptid, ql203h, ql204a, ql209a, ql211d, ql209c, ql205b, ql204d, 
         ql209a, ql205b, ql207)
#203h i g c d b
#204a b c 208
#209a e -g
#211d b -c -a 201
#209c b f
#htnderv ccbsr2 hctz2
#205b a c
#aspirin asa2
#asthma lipid
#nonsp_st rls
#h2b2 -coffee
#cox22
#qossl rptelaptimslpwak
#rls

#204d
#ahi
#209a
#209c
#205b
#waso
#207
#pcts90, gender

df.sleephabit <-basicSubset(datadict, df.halfna, "Sleep Habits")

sh_yn <-getYNterms(datadict, df.sleephabit)
df.sleephab.num<-removeCols(df.sleephabit, sh_yn)

summary(df.sleephab.num)

#304 number of naps/wk
#304a number of 5+min naps/wk
#305 regular nap every afternoon
#308a freq of trouble fall asleep
#308b wake up and difficult fall back asleep
#308d freq feeling unrested
#308e excess daytime sleepiness
#308f not enough sleep
#308g sleep pills
#308h nasal stuff
#308i leg jerks
#308j leg cramps
#310 freq snoring
#311 loud snore
#312 changes in snoring
#316 household members in snoring area
#325 cups of coffee/day
#326 tea
#327a glasses cola
#327b cans of cola
#328 wine
#329 beer
#330 hard liquor

df.sleephab.num.clean<-df.sleephab.num %>%
  mutate_each(funs(unlist(lapply(., recode8asNA))), 
              sh311, sh312, sh310) %>%
  select(-sh304, -smokstat_s2)

sh_yn<-c("obf_pptid", sh_yn)
df.sleephab.yn<-subsetCols(df.sleephabit, sh_yn)
df.sleephab.yn.clean<-df.sleephab.yn %>%
  mutate_each(funs(unlist(lapply(., recode8asNA)))) %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  rename(snore=sh309, surgsnor=sh313, stopbreath=sh314, sleepdisord=sh317, 
         smok20=sh320) %>%
  select(-snore, -surgsnor)
  summary 

df.sleephab.clean<-left_join(df.sleephab.num.clean, df.sleephab.yn.clean)

#310 311 stopbreath
#totminslpwd totminslpwe 
#gender sh329
#308b 308c
#308e d
#age
#308a
#308i j
#-sleepdisord

#ahi
#pcs_s2
#sh308b
#waso
#stopbreath
#mcs2
#sh329
#totslpwk
#pctsa90
#smok20
#ess
#avg dias
#remlaip
df.sleephab.clean<-df.sleephab.clean %>%
  select(obf_pptid, sh308b, stopbreath, sh329, totminslpwd, smok20)

df.bp <- basicSubset(datadict, df.halfna, "Blood Pressure")

df.bp.clean<-df.bp %>%
  select(-starts_with("pm")) %>%
  select(obf_pptid, avg23bpd_s2)

df.sleeparch<-basicSubset(datadict, df.halfna, "Sleep Architecture")

df.sleeparch.clean<-df.sleeparch %>%
  select(-starts_with("st"), -starts_with("times"), -starts_with("timer")) %>%
  select(obf_pptid, pcstahda, slp_rdi, pctsa75h, nremepbp, waso, tmstg2p,
         remlaip, pctsa90h, slpeffp, hremt1p, hremt34p)

#pctlt80 pctsa80h .968 pctlt75 pctsa75h pctsa70h pctsa85h
#mnstg34p
#minstg2p
#minstg1p
#pcstahda pcstahar pcstah3d pctstapn pctsthyp hslptawp
#-nsupinep nremepbp -nremepop
#minremp
#sh308b a d c f e

#pcstahda pcstah3d pctstapn pctsthyp  ahi_a0h3 oa4i4p ndes3ph rdi2ps avdnbp2 rdirem2p ainrem
#slp_rdi slp_time slpprdp time_bed timebedp minstg2p minremp
#pctsa75h 80h 70h pctlt75 pctlt80
#nremepbp -nremepop -nsupinep rempbp -remepop
#waso timstg1p slptawp minstg1p hslptawp
#tmstg2p -tmstg34p minstg2p -mnstg34p
#remlaip remlaiip rem_lat1 -tmremp
#pcts90h 85 pctlt90 85 -avgsat
#slp_eff slpeffp -slplatp slp_lat
#hremt1p remt1p -remt2p
#hremt34p remt34p

df.ess2<- basicSubset(datadict, df.halfna, "Epworth Sleepiness Scale")
df.ess<-df.ess2 %>% select(obf_pptid, ess_s2)

#SF-36
df.sf36<-basicSubset(datadict, df.halfna, "SF-36")
df.sf36.clean<-df.sf36 %>% 
  select(-starts_with("raw")) %>%
  select(obf_pptid, mcs_s2, bp_s2)
  #select(-re_s1, -rp_s1, -pcs_s1)

#ahi_a, ahi_c, pcsthda, ndes, oai, rdi
#slpeff -waso
#mh_s2 mcs gh vt
#bp pf_s2 pcs2 rp sf
#res2


df.index <-basicSubset(datadict, df.halfna, "Indexes")

df.index.clean<-df.index %>%
  select(obf_pptid, ahi_a0h3, oai4p, rdi2ps, rdirem2p, cai4p)

#ahi_c0h3a, ahi_a0h3a, ahi_a0h3, all rdi (rdinr2p), ndes3ph
#oai4pa oai4p
#rdi2ps rdi0ps rdi4ps
#rdirem2p
#cai4pa cai4p cai0p
#davbnoh

df.oxysat <- basicSubset(datadict, df.halfna, "Oxygen Saturation")

df.oxysat.clean<-df.oxysat %>%
  select(obf_pptid, avdnbp2, avgsat, ndes3ph)
#davbnoh aav, dav, sav (all hr)
#avdnbp2 (all other avdn, avdr) ndes5ph
#avgsat (avsao2nh, rh, sao2nrem, sao2rem)
#ai_nrem, ai_all, anhremop, ahnrembp
#ahremop, ai_rem, arremop, avdrop(0, 2, 3)
#ndes3ph (2,4,5)

df.hr <- basicSubset(datadict, df.halfna, "Heart Rate")

df.hr.clean<-df.hr %>%
  select(obf_pptid, davbnoh)

#savbroh savbnoh davbroh davbnoh aavbnoh aavbroh
#ai_nrem ai_all arnremop ahnremop ahnrembp
#arnrembp
#ai_rem ahremop

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
#ai_rem ahremop arremop
#arrembp arnrembp

df.arousal.clean<-df.arousal %>% 
  select(obf_pptid, ai_nrem, ai_rem)

df.joined<-left_join(df.demo.mod, df.anthro)
df.joined<-left_join(df.joined, df.ecg.clean)
df.joined<-left_join(df.joined, df.med.clean)
df.joined<-left_join(df.joined, df.medhist.clean)
df.joined<-left_join(df.joined, df.morning.clean)
df.joined<-left_join(df.joined, df.health.clean)
df.joined<-left_join(df.joined, df.qual.clean)
df.joined<-left_join(df.joined, df.sleephab.clean)
df.joined<-left_join(df.joined, df.sleeparch.clean)

df.joined<-left_join(df.joined, df.bp.clean)
df.joined<-left_join(df.joined, df.ess)
df.joined<-left_join(df.joined, df.sf36.clean)

df.joined<-left_join(df.joined, df.arousal.clean)
df.joined<-left_join(df.joined, df.hr.clean)
df.joined<-left_join(df.joined, df.oxysat.clean)
df.joined<-left_join(df.joined, df.index.clean)

df<-df.joined %>% select(-obf_pptid) %>% na.omit
