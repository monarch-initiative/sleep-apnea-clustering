#forget about cvdevents
recode2as0<-function(x){
  if (is.na(x)){
    x=NA
  } else
    if (x==2){
      x=0
    }
  return (x)
}

source("\lib\recode2as0.R")

df.cvd.clean<-cvdsum %>%
  select(which(colMeans(is.na(.)) < 0.5)) %>%
  select(-pptid, -censdate, -visitnumber) %>%
  mutate(gender=unlist(lapply(gender, recode2as0))) %>%
  mutate(vital=as.factor(vital), mi_fatal=as.factor(mi_fatal),
         stk_fatal=as.factor(stk_fatal), chd_death=as.factor(chd_death),
         cvd_death=as.factor(cvd_death), any_chd=as.factor(any_chd), 
         any_cvd=as.factor(any_cvd), prev_revpro=as.numeric(prev_revpro), 
         gender=as.factor(gender), race=as.factor(race)) %>%
  select(-prev_revpro, -mi_fatal, -any_cvd, -any_chd, -stk_fatal) %>%
  
  summary

library(polycor)
df<-df.cvd.clean %>% select(-obf_pptid) %>% na.omit
N=nrow(df)
hc <- hetcor(as.data.frame(df), ML=TRUE, pd=TRUE)

source("lib/plotMeltedCorr.R")
plotMeltedCorr(hc$correlations, "cont")

library(psych)
fa.parallel(hc$correlations, n.obs=N)
vss(hc$correlations, n=8, n.obs=N, rotate="varimax")

faPC <- fa(r=hc$correlations, nfactors=8, n.obs=N, rotate="varimax")
faPC$loadings

fa.plot(faPC, cut=0.5, labels=names(df))
fa.diagram(faPC)




library(cluster)



gower_dist <- daisy(as.data.frame(df %>% select(-obf_pptid, -race)),
                    metric = "gower", 
                    type=list(symm=c(1,19,21), asymm=c(7,9,10,11,18)))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair

df[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
##                            name accept_rate Outstate Enroll
## 682 University of St. Thomas MN   0.8784638    11712    828
## 284     John Carroll University   0.8711276    11700    820
##     Grad.Rate Private   isElite
## 682        89     Yes Not Elite
## 284        89     Yes Not Elite
# Output most dissimilar pair

df[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

hier=hclust(gower_dist, method="complete")

dend<-as.dendrogram(hier)
clusth<-cutree(hier, k=6)
df.wclust<-df %>%
  na.omit%>%
  mutate(cluster=clusth)

df.wclust %>%
  filter(cluster==1) %>%
  summary

library(dendextend)
dend <- color_branches(dend, k=6)
plot(dend)

df.wclust.cat<-df.wclust %>%
  select(vital, mi_fatal, stk_fatal, chd_death, cvd_death, any_chd, any_cvd,
         gender, cluster)

df.wclust.cont<-df.wclust %>%
  select(prev_mi, prev_mip, prev_stk, mi, mip, stroke, angina, revasc_proc, 
         ptca, cabg, chf, prev_chf, prev_revpro, age_s1, cluster)

data<-df.wclust.cat

df.working<-df.wclust.cont