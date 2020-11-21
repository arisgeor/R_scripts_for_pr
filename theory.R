library(rpart)
library(rpart.plot)
library(e1071)
library(MLmetrics)
library(ROCR)
library(class)
library(neuralnet)
library(vegan)
library(cluster)
library(mixtools)

rm = rm(list=ls())

### Hierrarchical
n = 4      # number of elements
df <- data.frame(x = rnorm(n), y = rnorm(n))
dist = dist(df)
dist
dist[1] = 3
dist[2] = 5
dist[3] = 4
dist[4] = 1
dist[5] = 7
dist[6] = 2
dist

hc = hclust(dist, method = "complete")    # or single
plot(hc)
clusters = cutree(hc, k = 2)
plot(df, col = clusters+1, pch = 15)
text(df, labels = row.names(df), pos = 2)

### Eigenvalues
values = c(2.35,1.87,0.15,0.05)
info_loss = (values[3]+values[4])/sum(values)
info_loss

### k-NN
X1 = c(0, -2, 3, 3)
X2 = c(0, 0, 0, 1)
Y = c(1, 1, 2, 2)
rnames = c("A", "B", "C", "D")
data = data.frame(X1, X2, Y, row.names = rnames)
knn(data[,-3], c(0.7,0.4), data[,3], k = 3, prob = TRUE)

### Simple Matching / Jaccard
m01 = 2
m10 = 1
m00 = 1
m11 = 4
smc = (m11 + m00)/(m01 + m10 + m11 + m00)
jaccard = (m11)/(m01 + m10 + m11)
smc
jaccard