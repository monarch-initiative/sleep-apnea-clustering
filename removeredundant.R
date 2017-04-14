
summary(df.ecg.yn.clean)
df.ecg.yn.clean<-df.ecg.yn.clean %>%
  select(-apbs)

source("lib/plotMeltedCorr.R")
source("lib/removeCorrelated.R")
bin_mat<-catCVmat(df.ecg.yn.clean %>% select(-obf_pptid))
plotMeltedCorr(bin_mat, "cat")

removeCorrelated(df.ecg.yn.clean)

#st4_1_3, lvhst, lvh3_1, lvh3_3, afib
#nonsp_st, rbbb, lbbb, iventblk, nonsp_tw
#morning: asa15

melted<-melt(bin_mat)
melteddesc<-melted %>%
  arrange(desc(value)) %>%
  filter(abs(value)>.10) %>%
  filter(Var1 != Var2)

terms_to_remove<-unique(pull(melteddesc, Var1))
terms_to_remove1<-sapply(terms_to_remove, as.character)

correlated<-df %>%
  select(one_of(terms_to_remove1))
nas<-sapply(correlated, na_count)
data.frame(nas)

df<-df.qual.clean
bin_mat<-catCVmat(df %>% select(-obf_pptid))
plotMeltedCorr(bin_mat, "cat")

#medalert: oxyund70
#med: 

#ccb1     18      Var1    Var2     value
#1     ccb1  ccbsr1 0.6564514
#2  estrgn1 premar1 0.6187469
#3  estrgn1 progst1 0.4311521
#4  diuret1   hctz1 0.4000972
#5  premar1 progst1 0.3694790
#6  diuret1  hctzk1 0.3641032
#7  diuret1 htnmed1 0.3603116
#8  diuret1   loop1 0.3550063
#9     ccb1 htnmed1 0.3502861
#10   beta1 htnmed1 0.3277588
#11  ccbsr1 htnmed1 0.3256979
#12    ace1 htnmed1 0.3249234
#13  istrd1 sympth1 0.2867338
#14    ccb1  ccbir1 0.2521283
#15  lipid1   niac1 0.2422934
#16   pdei1 sympth1 0.2422213
#17  istrd1  ostrd1 0.2411654
#18    dig1   warf1 0.2357670
#19   hctz1 htnmed1 0.2123034
#20    dig1   loop1 0.2109428
#21  ostrd1 sympth1 0.1936067
#22  hctzk1 htnmed1 0.1932039
#23  istrd1   pdei1 0.1901752
#24 htnmed1   loop1 0.1883768
#25    dig1 htnmed1 0.1760358
#26 htnmed1    ntg1 0.1427467
#27   loop1   warf1 0.1405439
#28 anar1a1    dig1 0.1361671
#29    ace1 diuret1 0.1349992
#30   loop1    ntg1 0.1346584
#31  ostrd1   pdei1 0.1336481
#32   beta1 diuret1 0.1263259
#33  ccbir1 htnmed1 0.1250933
#34   basq1  lipid1 0.1230199
#35 htnmed1  lipid1 0.1209736
#36 benzod1   ntca1 0.1199608
#37    asa1 htnmed1 0.1185450
#38    dig1 diuret1 0.1184218
#39    asa1  lipid1 0.1145379
#40    ccb1    ntg1 0.1092891
#41    ace1   loop1 0.1090320
#42    ccb1 diuret1 0.1029564
#43 htnmed1   warf1 0.1025041
#44 htnmed1   vaso1 0.1022154
#45    asa1   beta1 0.1012543
#46  ccbir1    ntg1 0.1005911
#47    dig1    ntg1 0.1001010
#estrgn1  18
#diuret1  18
#premar1  18
#beta1    18
#ccbsr1   18
#ace1     18
#istrd1   18
#lipid1   18
#pdei1    18
#dig1     18
#hctz1    18
#ostrd1   18
#hctzk1   18
#htnmed1  18
#loop1    18
#anar1a1  18
#ccbir1   18
#basq1    18
#benzod1  18
#asa1     18

#medhist
#         Var1       Var2     value
"1  htnderv_s1     srhype 0.6011975
2    asth1215   asthma15 0.3878631
3    cough315   phlegm15 0.3325395
4     runny15    sinus15 0.3063048
5    angina15       mi15 0.2749451
6    angina15       ca15 0.2447444
7        ca15       mi15 0.2376073
8    angina15     cabg15 0.2351234
9      cabg15       mi15 0.2263728
10     copd15   emphys15 0.2182831
11   angina15 htnderv_s1 0.2071036
12       hf15       mi15 0.2014387
13 htnderv_s1       mi15 0.2002980
14       ca15     cabg15 0.1907048
15   asthma15   crbron15 0.1845131
16   cough315    runny15 0.1626378
17   asth1215   crbron15 0.1580981
18     copd15   crbron15 0.1538582
19   phlegm15    runny15 0.1499548
20   angina15       hf15 0.1494248
21   cough315   crbron15 0.1491192
22   crbron15   phlegm15 0.1458273
23   crbron15   emphys15 0.1437211
24   asth1215     copd15 0.1431732
25       ca15 htnderv_s1 0.1417700
26 htnderv_s1 parrptdiab 0.1398831
27       ca15   othrcs15 0.1381007
28       hf15 htnderv_s1 0.1271200
29   asthma15     copd15 0.1262800
30   asth1215   cough315 0.1250038
31 htnderv_s1   stroke15 0.1240771
32   cough315    sinus15 0.1233994
33     cabg15 htnderv_s1 0.1226124
34     cabg15   othrcs15 0.1218732
35   phlegm15    sinus15 0.1187843
36       mi15   othrcs15 0.1168128
37     cabg15       hf15 0.1159303
38   angina15   othrcs15 0.1155354
39   asth1215   phlegm15 0.1115526
40   asthma15   cough315 0.1056363
41   othrcs15    pacem15 0.1035365
42   emphys15   phlegm15 0.1012109
"#[1] "htnderv_s1" "asth1215"   "cough315"   "runny15"    "angina15"   "ca15"      
#[7] "cabg15"     "copd15"     "hf15"       "asthma15"   "phlegm15"   "crbron15"  
#[13] "mi15"       "othrcs15"   "emphys15" 

"htnderv_s1   asth1215   cough315    runny15   angina15       ca15     cabg15 
0         37         32         35        116         31         30 
copd15       hf15   asthma15   phlegm15   crbron15       mi15   othrcs15 
57        241         39         34         35        117         35 
emphys15 
35"

"nas
exefrt25 454
limit25  452
emacls25 444
phacls25 440
carful25 457
emctdn25 440"

"       Var1     Var2     value
1  exefrt25  limit25 0.4997724
2   limit25 phctdn25 0.4551527
3  exefrt25 phacls25 0.4483378
4   limit25 phacls25 0.4456135
5  emacls25 emctdn25 0.4406602
6  exefrt25 phctdn25 0.4305983
7  phacls25 phctdn25 0.4215064
8  carful25 emctdn25 0.4158483
9  carful25 emacls25 0.3996073
10 emacls25 phacls25 0.3189181
11 emctdn25 phctdn25 0.2870910
12 emctdn25 exefrt25 0.2658891
13 emctdn25 phacls25 0.2530270
14 emctdn25  limit25 0.2481357
15 emacls25 exefrt25 0.2479690
16 carful25 exefrt25 0.2450079
17 carful25 phacls25 0.2411672
18 carful25 phctdn25 0.2346618
19 carful25  limit25 0.2267718
20 emacls25 phctdn25 0.2211425
21 emacls25  limit25 0.2149162"

#carful25, emacls25, emctdn25, exefrt25, limit25, phacls25, phctdn25

centRes <- centrality(Graph_Ising1)

# Node strength (degree):
centRes$OutDegree # Or InDegree, it's the same in unweighted networks

# Closeness:
centRes$Closeness

# Betweenness:
centRes$Betweenness

centralityPlot(Graph_Ising1)




