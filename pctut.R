data(iris)
# this is a little tweak so that things line up nicely later on
iris$Species <- factor(iris$Species,
                       levels = c("versicolor","virginica","setosa"))
head(iris)

round(cor(iris[,1:4]), 2)

pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)

summary(pc)

plot(pc,type="lines")

biplot(pc)

library(rgl)
plot3d(pc$scores[,1:3], col=as.numeric(iris$Species))

text3d(pc$scores[,1:3],texts=rownames(iris))
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)

set.seed(42)
cl <- kmeans(iris[,1:4],3)
iris$cluster <- as.factor(cl$cluster)

plot3d(pc$scores[,1:3], col=iris$cluster, main="k-means clusters")
plot3d(pc$scores[,1:3], col=as.numeric(iris$Species), main="actual species")

with(iris, table(cluster, Species))

di <- dist(iris[,1:4], method="euclidean")
tree <- hclust(di, method="ward")
iris$hcluster <- as.factor((cutree(tree, k=3)-2) %% 3 +1)
# that modulo business just makes the coming table look nicer
plot(tree, xlab="")
rect.hclust(tree, k=3, border="red")

with(iris, table(hcluster, Species))