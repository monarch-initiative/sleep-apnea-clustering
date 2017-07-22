library(polycor)
library(psych)
source("lib/plotMeltedCorr.R")

#Function for factor analysis


N=nrow(df.orig)
hc <- hetcor(as.data.frame(df.orig))


plotMeltedCorr(hc$correlations, "cont")


fa.parallel(hc$correlations, n.obs=N)
vss(hc$correlations, n=24, n.obs=N, rotate="varimax")

faPC <- fa(r=hc$correlations, nfactors=22, n.obs=N, rotate="varimax")
faPC$loadings

fa.plot(faPC, cut=0.5, 
        labels=NA,
        #labels=names(df.orig), 
        choose=c(1,2), show.points=TRUE)
fa.diagram(faPC)