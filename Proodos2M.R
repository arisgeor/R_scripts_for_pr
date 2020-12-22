cat("\014")
remove(list = ls())

data = read.csv("data.txt", stringsAsFactors = TRUE)
###############################################################################################################
# Libraries
library(class)			-> knn
library(MLmetrics)		-> Accuracy
library(vegan)			-> isomap
library(scatterplot3d)
library(cluster)		-> silhouette
library(dbscan)
library(mixtools)		-> EM
###############################################################################################################
# Lab 6

# scale center: afairei meso oro, scale: diairei mesh timh, unique
transformed <- scale(data, center = TRUE, scale = TRUE)
data = unique(data)

# sample 250 samples of 1:nrow(data)
sampdata = data[sample(nrow(data), 250, replace = TRUE),]

# διακριτοποιεί τα δεδομένα στα διαστήματα (0, 10], (10, 20], …, (90, 100], dig.lab = digits label
cut(x, seq(0,100,10), dig.lab = 4))

# 3D scatteplot
s3d = scatterplot3d(data, color = "green", pch = 19, scale.y = 1.5)
coords <- s3d$xyz.convert(pdata)
text(coords$x, coords$y, labels=row.names(pdata), pos=2)

# PCA
pca_model <- prcomp(data, center = TRUE, scale = TRUE)
eigenvalues = pca_model$sdev^2
eigenvectors = pca_model$rotation

# significance
barplot(eigenvalues / sum(eigenvalues))

# first 2 pca components
pca_components = as.data.frame(predict(pca_model, data)[, 1:2])

# isomap calculate and plot
isom <- isomap(dist(data), ndim=2, k = 4)
data_2d <- isom$points
colors = data_2d[,1] -  min(data_2d[,1]) + 1
scatterplot3d(data, angle = 88, scale.y = 5, color = colors)
###############################################################################################################
# Lab 7

# centers
c1 = c(-4,10)
c2 = c(0,0)
c3 = c(4,10)
centers = rbind(c1,c2,c3)

# kmeans centers, random if scalar
model = kmeans(data, centers = kdata[1:3,])
centers = model$centers
cl = model$cluster

# separation, cohesion
cohesion = model$tot.withinss
separation = model$betweenss

# Plot data and centers
plot(data, col = model$cluster, pch = 15, , main = "kmeans")
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

# silhouette
model_silhouette = silhouette(model$cluster, dist(data))
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])

# Heatmap
data_ord = data[order(model$cluster),]
heatmap(as.matrix(dist(data_ord)), Rowv = NA, Colv = NA,col = heat.colors(256), revC = TRUE)
###############################################################################################################
# Lab 8

# single and complete link
hc_single = hclust(dist(data), method = "single")
hc_complete = hclust(dist(data), method = "complete")
plot(hc_single)
rect.hclust(hc_single, k = 2)
clusters = cutree(hc_single, k = 2)

# DBSCAN
model = dbscan(data, eps = 2, minPts = 2)
clusters = model$cluster
plot(data, col = model$cluster + 1, pch = ifelse(model$cluster, 1, 4), , main = "dbscan")
###############################################################################################################
# Lab 9

# Gaussian mixtures 1D
plot(data.frame(x, 0), ylim = c(-0.01, 0.25), col = y, xlab = "Data", ylab = "Density")
lines(density(x), col = y)
model = normalmixEM(x, mu = c(0,1), sd.constr = c(1,1))
model$mu
model$sigma
model$lambda
model$loglik


# 1 = likelihood-iterations / 2 = density
plot(model, which = 2)
lines(density(x), lty = 2, lwd = 2)

# Gausian mixtures 2D
model = mvnormalmixEM(data, k = 3 , epsilon = 0.1)
clusters = max.col(model$posterior)
centers = matrix(unlist(model$mu), byrow = TRUE, ncol = 2)

# AIC BIC
numparams = length(model$mu) + length(model$stdev) + length(model$lambda)
AIC[k] = 2 * numparams - 2 * model$loglik
BIC[k] = log(length(x)) * numparams - 2 * model$loglik
###############################################################################################################
# Misc

# i clusters silhouette
slc = c()
for (i in 2:20){
  clusters = cutree(hc, k = i)
  slc [i-1] = mean(silhouette(clusters, d)[, 3])
}
plot(2:20, slc, type="b", xlab="Number of Clusters", ylab="Silhouette")

# knn dist
knndist = kNNdist(mdata, k = 10)
plot(sort(knndist), type = 'l', xlab = "Points sorted by distance", ylab = "10-NN distance")
abline(h = 0.65, col = "grey")

# AIC BIC search
AIC = c()
BIC = c()
par(mfrow = c(2, 2))
for (k in 2:5) {
  model <- normalmixEM(x, k = k, epsilon = 0.0001)
  plot(model, which = 2, main2 = paste("Density (k = ", k, ")", sep = ""))
  
  numparams = length(model$mu) + length(model$stdev) + length(model$lambda)
  AIC[k] = 2 * numparams - 2 * model$loglik
  BIC[k] = log(length(x)) * numparams - 2 * model$loglik
}
par(mfrow = c(1, 1))