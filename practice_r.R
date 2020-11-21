library(rpart)
library(rpart.plot)
library(e1071)
library(MLmetrics)
library(ROCR)
library(class)
library(neuralnet)
library(vegan)

library(cluster)
library(dbscan)
library(mixtools)

rm = rm(list=ls())

### Eigenvalues
values = c(1.32,1.15,0.85,0.42)
info_loss = (values[3]+values[4])/sum(values)
info_loss

WEATHER = c("SUNNY","CLOUDY","RAINY","SUNNY","RAINY")
TEMPERATURE = c("HIGH","LOW","LOW","LOW","HIGH")
PLAY = c("YES","YES","NO","YES","NO")
data = data.frame(WEATHER,TEMPERATURE,PLAY)

model <- naiveBayes(PLAY ~ ., data = data, laplace = 0)
pred = predict(model, data.frame(WEATHER = "CLOUDY", TEMPERATURE = "HIGH" ),type = "raw" )
pred

### Simple Matching / Jaccard
m01 = 2
m10 = 1
m00 = 4
m11 = 1
smc = (m11 + m00)/(m01 + m10 + m11 + m00)
smc
jaccard = m11/(m01 + m10 + m11)

###########################################

rm = rm(list=ls())
setwd("C:\\Users\\Aristos\\Desktop")
training = read.csv("data.csv")

#gini_body (sedan, crossover)
absfreq = table(training[, c(1,5)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
levels(training$Body)
GINI_sedan = 1 - freq["sedan", "Low"]^2 - freq["sedan", "High"]^2
GINI_crossover = 1 - freq["crossover", "Low"]^2 - freq["crossover", "High"]^2

GINI_body = freqSum["sedan"] * GINI_sedan + freqSum["crossover"] * GINI_crossover
GINI_body

#Entropy of Body
freq = prop.table(table(training[, c(5)]))
Entropy_All = - freq["Low"] * log2(freq["Low"]) - freq["High"] * log2(freq["High"])

absfreq = table(training[, c(1, 5)]) #body is the first column
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))

Entropy_sedan = - freq["sedan", "Low"] * log2(freq["sedan", "Low"]) - freq["sedan", "High"] * log2(freq["sedan", "High"])
Entropy_crossover = - freq["crossover", "Low"] * log2(freq["crossover", "Low"]) - freq["crossover", "High"] * log2(freq["crossover", "High"])
GAIN_body = Entropy_All - freqSum["sedan"] * Entropy_sedan - freqSum["crossover"] * Entropy_crossover
GAIN_body

#tree pred
newdata = training[,c(1,2,5)]
model <- rpart(price ~ body + engine, method = "class", data = newdata, 
               minsplit = 1, minbucket = 1, cp = -1)
rpart.plot(model, extra = 104, nn = TRUE)
trvalue <- data.frame(body = factor("sedan", levels(newdata$body)),  
                      engine = factor("diesel", levels(newdata$engine)))
pred = predict(model, trvalue)
pred

# knn pred
newdata11 = training[,c(3,4,5)]
knn(newdata11[,-3], c(2.3,1.2), newdata11[,3], k = 7, prob = TRUE)

# Bayes pred
newdata12 = training[,c(1,2,5)]
model <- naiveBayes(price ~ ., data = newdata12, laplace = 1)
trvalue <- data.frame(body = factor("crossover", levels(newdata12$body)),  
                      engine = factor("gas", levels(newdata12$engine)))
predict(model, trvalue, type = "raw")

#SVM Pred
new_data13 = training[,c(3,4,5)]
svm_model = svm(price ~ ., kernel="radial", type="C-classification", data = new_data13, gamma = 1)
pred = predict(svm_model, new_data13[,-3])
prediction = as.data.frame(pred)
Recall(training$price, pred, positive = "High")

# 
# new_data14 = training[,3:4]
# model = kmeans(new_data14, centers = 2)

#kmeans me kentra ta 2 prwta shmeia.
new_data14 = data.frame(training[,3], training[,4])
model = kmeans(new_data14, centers = new_data14[1:2,])

model_silhouette = silhouette(model$cluster, dist(new_data14))
plot(model_silhouette)
mean(model_silhouette[,3])

# 
newdata15 = training[,c(3,4)]
d = dist(newdata15)
hc_complete = hclust(d, method = "complete")
plot(hc_complete)
clusters = cutree(hc_complete, k = 2)
plot(hc_complete)
rect.hclust(hc_complete, k = 2)

model_silhouette = silhouette(clusters, d)
plot(model_silhouette)
mean(model_silhouette[,3])

# 
newdata16 = training[,c(3,4)]
model = dbscan(newdata16, eps = 0.3, minPts = 7)
clusters = model$cluster
plot(newdata16, col = clusters + 1, pch = 15)