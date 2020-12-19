iris
head(iris)
summary(iris$Species)

iris_new = iris
iris_new$Species = NULL
head(iris_new)
tail(iris_new)

results = kmeans(iris_new,3)
results
results$cluster
results$centers
results$size
results$withinss
results$betweenss

plot(iris$Petal.Length , iris$Petal.Width , col = results$cluster)
points(results$centers , pch=2 , col="green")
