
# Lab: Unsupervised Learning


## Clustering

### $K$-Means Clustering
###
set.seed(2)
x = matrix(rnorm(50*2), nrow=50, ncol=2)
plot(x[,1], x[,2])
plot(x)


x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

plot(x[,1], x[,2])


###
?kmeans

set.seed(1)
#n start how many times i try new start values 
km.out = kmeans(x, 2, nstart = 20)
km.out

km.out$cluster
#variance
km.out$withinss
km.out$tot.withinss

###cex is for size of points
plot(x, col = (km.out$cluster + 1),
    main = "K-Means Clustering Results with K = 2",
    xlab = "x1", ylab = "x2", pch = 20, cex = 2)

###
set.seed(1)
km.out = kmeans(x, 3, nstart = 20)
km.out
km.out$cluster

plot(x, col = (km.out$cluster + 1),
    main = "K-Means Clustering Results with K = 3",
    xlab = "x1", ylab = "x2", pch = 20, cex = 2)



### Hierarchical Clustering

###
?hclust

hc.complete = hclust(dist(x), method = "complete")
hc.complete

###
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

###
plot(hc.complete, main = "Complete Linkage",
    xlab = "", sub = "")

plot(hc.average, main = "Average Linkage",
    xlab = "", sub = "")

plot(hc.single, main = "Single Linkage",
    xlab = "", sub = "")


###
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)


###scale the data
?scale
xsc = scale(x)
hc.complete.sc = hclust(dist(xsc), method = "complete")

plot(hc.complete.sc, main = "Hierarchical Clustering with Scaled Features")



## NCI60 Data Example

###
library(ISLR2)

?NCI60
names(NCI60)

nci.labs = NCI60$labs
nci.data = NCI60$data

dim(nci.data)

table(nci.labs)


### Clustering the Observations of the NCI60 Data

###
sd.data = scale(nci.data)

###
data.dist = dist(sd.data)

plot(hclust(data.dist, method = "complete"), 
     xlab = "", sub = "", ylab = "",
    labels = nci.labs, main = "Complete Linkage")

plot(hclust(data.dist, method = "average"),
    labels = nci.labs, main = "Average Linkage",
    xlab = "", sub = "", ylab = "")

plot(hclust(data.dist, method = "single"),
    labels = nci.labs,  main = "Single Linkage",
    xlab = "", sub = "", ylab = "")

###
hc.out = hclust(dist(sd.data), method = "complete")
hc.clusters = cutree(hc.out, 4)

###
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

table(hc.clusters, nci.labs)


###
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)