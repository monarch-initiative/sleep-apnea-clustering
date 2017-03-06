library(vegan)
data(dune)
d<-vegdist(dune)

par(mfrow=c(1,3))
par(mar=c(3,4,1,1)+.1)

csin <- hclust(d, method="single")
plot(csin, hang=-1)

ccom <- hclust(d, method="complete")
plot(ccom, hang=-1)
caver <- hclust(d, method="aver")
plot(caver, hang=-1)

vegemite(dune, caver)

plot(csin, hang=-1)
rect.hclust(csin, 3)
plot(ccom, hang=-1)
rect.hclust(ccom, 3)
plot(caver, hang=-1)
rect.hclust(caver, 3)

cl <- cutree(ccom, 3)

table(cl)

table(cl, cutree(csin, 3))
table(cl, cutree(caver, 3))

ord <- cmdscale(d)
ordiplot(ord)

ordihull(ord, cl, lty=3)
ordispider(ord, cl, col="blue", label=TRUE)
ordiellipse(ord, cl, col="red")

ordiplot(ord, dis="si")
ordihull(ord, cutree(caver, 3))
ordiplot(ord, dis="si")
ordicluster(ord, csin)

ordiplot(ord, dis="si")
ordicluster(ord, caver)
ordiplot(ord, dis="si")
ordicluster(ord, caver, prune=2)

den <- as.dendrogram(caver)
x <- scores(ord, display = "sites", choices = 1)
oden <- reorder(den, x)

par(mfrow=c(2,1))
par(mar=c(3,4,1,1)+.1)
plot(den)
plot(oden)
par(mfrow=c(1,1))

vegemite(dune, oden)

tabasco(dune, caver)
tabasco(dune, caver, Rowv = FALSE)
tabasco(dune, oden, Rowv = FALSE)

mst <- spantree(d)

ordiplot(ord, dis="si")
lines(mst, ord)

plot(mst, type="t")

plot(d, cophenetic(csin), asp=1)
abline(0, 1)
plot(d, cophenetic(ccom), asp=1)
abline(0, 1)
plot(d, cophenetic(caver), asp=1)
abline(0, 1)

cor(d, cophenetic(csin))
cor(d, cophenetic(ccom))
cor(d, cophenetic(caver))

data(dune.env)
cl <- factor(cl)
Moist <- with(dune.env, as.numeric(as.character(Moisture)))
with(dune.env, as.numeric(Moisture))

anova(lm(Moist ~ cl))
anova(rda(Moist ~  cl))
with(dune.env, table(cl, Management))

library(labdsv)
const(dune, cl)
importance(dune, cl)

mod <- indval(dune, as.numeric(cl))
names(mod)

mod$maxcls
mod$pval
summary(mod)

summary(mod, type = "long")

ckm <- kmeans(decostand(dune, "hell"), 3)
ckm$cluster

ordiplot(ord, dis="si")
ordihull(ord, ckm$cluster, col="red")

ccas <- cascadeKM(decostand(dune, "hell"), 2, 15)
plot(ccas, sortq=TRUE)

library(cluster)
cfuz <- fanny(d, 3, memb.exp=1.7)
names(cfuz)

ordiplot(ord, dis="si")
ordiplot(ord, dis="si", type="n")
stars(cfuz$membership, locatio=ord, draw.segm=TRUE, add=TRUE, scale=FALSE, len=0.1)
ordihull(ord, cfuz$clustering, col="blue")