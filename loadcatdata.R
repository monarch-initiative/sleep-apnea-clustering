df.ecg <- basicSubset(datadict, shhs1.pruned, "ECG")

source("lib/getYNterms.R")
terms_ecg.yn <-getYNterms(datadict, df.ecg)
terms_ecg.yn <-c("obf_pptid", terms_ecg.yn)
df.ecg.yn<-subsetCols(df.ecg, terms_ecg.yn)
df.ecg.yn<-df.ecg.yn %>%
  mutate_each(funs(as.factor))

#remove truposmi, vpbs, st5_1_3, wpw, part2deg, nodal, mob2
df.ecg.yn.clean <-df.ecg.yn %>%
  select(-truposmi, -vpbs, -st5_1_3, -wpw, -part2deg, -nodal, -mob2) %>%
  mutate_each(funs(factor), -obf_pptid) %>%
  summary

df.ecg.other<-removeCols(df.ecg, terms_ecg.yn)
df.ecg.other %>%
  select(antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor)) %>%
  #na.omit %>%
  summary

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

df.ecg.other.clean<-df.ecg.other %>%
  select(obf_pptid, antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw)%>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  mutate_each(funs(factor), -obf_pptid) 

df.ecg.other<-df.ecg.other %>%
  select(antlatmi, antsepmi, infmi, nonsp_st, nonsp_tw) %>%
  mutate_each(funs(as.factor))

df.ecg.clean<-left_join(df.ecg.yn.clean, df.ecg.other.clean)

source("lib/catCrossTabs.R")
source("lib/catCVmat.R")
source("lib/plotMeltedCorr.R")

catCrossTabs(df.ecg.other, "crosstabs-ecg-other.txt")
ecg_catmat<-catCVmat(df.ecg.other)

#can convert these 5 columns into yes/no
#Anything >0 -> 1


df.ecg.cont <- df.ecg %>%
  select(qrs, ventrate) %>%
  mutate_each(funs(as.numeric))
summary(df.ecg.cont)

df.medalert<-basicSubset(datadict, shhs1.pruned, "Medical Alert")
df.medalert %>%
  select(-obf_pptid) %>%
  mutate_each(funs(as.factor)) %>%
  summary

#drop hrund30, treat 8 as NA

recode8asNA<-function(x){
  if (is.na(x)){
    y=NA
  } else
  if (x==8){
    y=NA
  }
  return (y)
}

df.medalert %>%
  select(-hrund30) %>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  mutate_each(funs(as.factor), -obf_pptid) %>%
  summary

df.medalert.clean<-df.medalert %>%
  select(-hrund30) %>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  mutate_each(funs(as.factor), -obf_pptid)

df.joined<-left_join(df.ecg.clean, df.medalert.clean)
  

df.med<-basicSubset(datadict, shhs1.pruned, "Medications")

df.med %>%
  select(-obf_pptid) %>%
  mutate_each(funs(factor)) %>%
  summary

df.med.clean<-df.med %>%
  mutate_each(funs(factor), -obf_pptid)

df.joined<-left_join(df.joined, df.med.clean)

df.medhist <- basicSubset(datadict, shhs1.pruned, "Medical History")
terms.noyes.medhist<-getYNterms(datadict, df.medhist)
terms.noyes.medhist<-c("obf_pptid", terms.noyes.medhist)
df.medhist.yes<-subsetCols(df.medhist, terms.noyes.medhist)
df.medhist.clean<-df.medhist.yes %>%
  select(-starts_with("prev")) %>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  mutate_each(funs(factor), -obf_pptid)

#treat 8 as NA

df.medhist.num<-removeCols(df.medhist, terms.noyes.medhist)

df.joined<-left_join(df.joined, df.medhist.clean)

df.adverse <- basicSubset(datadict, shhs1.pruned, "Adverse Events")
df.adverse %>%
  select(-obf_pptid) %>%
  mutate_each(funs(factor)) %>%
  summary

df.adverse.no0<-df.adverse %>%
  select(-imhihrae, -imlohrae, -imosatae, -urosatae, -imdbpae, -imsbpae, -othprbae, -skrctnae, -tripae, -urdbpae, -ursbpae)

#dont include df.adverse at all

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
  mutate_each(funs(factor)) %>%
  summary
  catCVmat
#treat 2 in meds10 as NA (actually just get rid of it since too many NAs)

df.morning.clean<-df.morning%>%
    select(obf_pptid, diffa10) %>%
    mutate(diffa10=as.factor(diffa10))

df.joined<-left_join(df.joined, df.morning.clean)

df.health <- basicSubset(datadict, shhs1.pruned, "Health Interview")

#asa15, evsmok15, nitro15, ns1yr15, slpill15, smknow15, 
health_yn<-getYNterms(datadict, df.health)
health_yn<-c("obf_pptid", health_yn)
df.health.yn<-subsetCols(df.health, health_yn)
df.health.yn %>%
  mutate_each(funs(factor)) %>%
  summary

#treat 8 in nitro and slpill as NA, delete smknow15, ns1yr15
df.health.clean<-df.health.yn %>%
  select(-ns1yr15, -smknow15) %>%
  mutate_each(funs(unlist(lapply(., is.greaterThanZero))), -obf_pptid) %>%
  mutate_each(funs(factor), -obf_pptid) 

df.joined<-left_join(df.joined, df.health.clean)

corr.health<-df.health %>%
  select(-obf_pptid, -asa15, -evsmok15, -nitro15, -ns1yr15, -slpill15, 
         -smknow15, -napsmn15) %>%
  #summary
  na.omit %>%
  cor(method="spearman")

df.qual <- basicSubset (datadict, shhs1.pruned, "Quality Of Life")

terms.noyes.qual<-getYNterms(datadict, df.qual)
terms.noyes.qual<-c("obf_pptid", terms.noyes.qual)
df.qual.yes<-subsetCols(df.qual, terms.noyes.qual)
df.qual.yes %>%
  mutate_each(funs(factor)) %>%
  summary

df.qual.clean<-df.qual.yes %>%
  mutate_each(funs(factor), -obf_pptid)

df.joined<-left_join(df.joined, df.qual.clean)

catCrossTabs(df.qual.yes, "crosstabs-qual-yesno.txt")
corr.qual.yn <- catCVmat(df.qual.yes)
p<-plotMeltedCorr(corr.qual.yn, "cat")

df.qual.num<-removeCols(df.qual, terms.noyes.qual)
corr.qual.num<-corrNASpear(df.qual.num %>% select(-obf_pptid))
p.num <- plotMeltedCorr(corr.qual.num, "cont")

df.sleephabit <-basicSubset(datadict, shhs1.pruned, "Sleep Habits")
sh_yn <-getYNterms(datadict, df.sleephabit)
df.sleephab.yn<-subsetCols(df.sleephabit, sh_yn)
df.sleephab.yn %>%
  mutate_each(funs(factor)) %>%
  summary  


#get rid of cpap02, stpbrt02, surgsa02
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

df.joined<-left_join(df.joined, df.med.clean)
df.joined<-left_join(df.joined, df.qual.clean)
df.joined<-left_join(df.joined, df.health.clean)
df.joined<-left_join(df.joined, df.morning.clean)
df.joined<-left_join(df.joined, df.medhist.clean)

df<-df.ecg.clean %>% select(-obf_pptid, -apbs) %>% na.omit %>% 
  mutate_each(funs(as.integer))
Data_binary <- as.matrix(df)
for (i in 1:ncol(Data_binary)){
  Data_binary[,i] <- 1 * (Data_binary[,i] > median(Data_binary[,i]))
}

library("IsingSampler")
Res <- EstimateIsing(as.matrix(Data_binary), method = "uni")
Graph_Ising1 <- qgraph(Res$graph, layout = "spring", 
                       labels = colnames(Data_binary))

library("IsingFit")
Res <- IsingFit(as.matrix(Data_binary),gamma = 0.25, plot=FALSE)
Graph_Ising2 <- qgraph(Res$weiadj, layout = "spring", 
                       labels = colnames(Data_binary))