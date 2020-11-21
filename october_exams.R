library(rpart)
library(rpart.plot)
library(e1071)
library(MLmetrics)
library(ROCR)
library(class)
library(neuralnet)
library(cluster)
library(dbscan)
library(mixtools)
#library(vegan)

### Eigenvalues
values = c(1.32,1.15,0.85,0.42) #when you keep 2, you keep the 2 highest
info_loss = (values[3]+values[4])/sum(values)
info_loss

#Bayes
WEATHER = c("SUNNY","CLOUDY","RAINY","SUNNY","RAINY")
TEMPERATURE = c("HIGH","LOW","LOW","LOW","HIGH")
PLAY = c("YES","YES","NO","YES","NO")
data = data.frame(WEATHER,TEMPERATURE,PLAY)

model <- naiveBayes(PLAY ~ ., data = data, laplace = 0)
pred = predict(model, data.frame(WEATHER = "CLOUDY", TEMPERATURE = "HIGH" ),type = "raw" )
pred

#Simple Matching / Jaccard
m01 = 3
m10 = 1
m00 = 3
m11 = 1
smc = (m11 + m00)/(m01 + m10 + m11 + m00)
j = (m11)/(m01 + m10 + m11)

#################################################

training = read.csv("poker.csv")

#GINI_hand
absfreq = table(training[, c(2,4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
levels(training$Hand)
GINI_big = 1 - freq["Big", "No"]^2 - freq["Big", "Yes"]^2
GINI_small = 1 - freq["Small", "No"]^2 - freq["Small", "Yes"]^2

GINI_hand = freqSum["Big"] * GINI_big + freqSum["Small"] * GINI_small
GINI_hand

#GINI_Character
absfreq = table(training[, c(3,4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
levels(training$Character)
GINI_Bluffer = 1 - freq["Bluffer", "No"]^2 - freq["Bluffer", "Yes"]^2
GINI_NonBluffer = 1 - freq["NonBluffer", "No"]^2 - freq["NonBluffer", "Yes"]^2

GINI_Character = freqSum["Bluffer"] * GINI_Bluffer + freqSum["NonBluffer"] * GINI_NonBluffer
GINI_Character

#GAIN_sex
freq = prop.table(table(training[, c(4)]))
Entropy_All = - freq["No"] * log2(freq["No"]) - freq["Yes"] * log2(freq["Yes"])

absfreq = table(training[, c(1, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))

Entropy_Male = - freq["Male", "No"] * log2(freq["Male", "No"]) - freq["Male", "Yes"] * log2(freq["Male", "Yes"])
Entropy_Female = - freq["Female", "No"] * log2(freq["Female", "No"]) - freq["Female", "Yes"] * log2(freq["Female", "Yes"])
GAIN_sex = Entropy_All - freqSum["Male"] * Entropy_Male - freqSum["Female"] * Entropy_Female
GAIN_sex

#Tree Pred
newdata = training[,c(1,2,3,4)]
model <- rpart(Win ~ Sex + Hand + Character, method = "class", data = newdata, 
               minsplit = 1, minbucket = 1, cp = -1)
rpart.plot(model, extra = 104, nn = TRUE)
trvalue <- data.frame(Sex = factor("Male", levels(newdata$Sex)),  
                      Hand = factor("Big", levels(newdata$Hand)),
                      Character = factor("NonBluffer", levels(newdata$Bluffer)))
pred = predict(model, trvalue)
pred

#Bayes Pred
newdata12 = training[,c(1,2,3,4)]
model <- naiveBayes(Win ~ ., data = newdata12, laplace = 1)
trvalue <- data.frame(Sex = factor("Female", levels(newdata12$Sex)),  
                      Hand = factor("Small", levels(newdata12$Hand)),
                      Character = factor("Bluffer", levels(newdata12$Bluffer)))
predict(model, trvalue, type = "raw")

##################################################################################

data = read.csv("clustering_data.csv")

#kmeans 
model = kmeans(data, centers = data[1:2,])
model$cluster
model$centers
model_silhouette = silhouette(model$cluster, dist(data))
plot(model_silhouette)
mean(model_silhouette[,3])

#single linkage
d = dist(data)
hc_single = hclust(d, method = "single")
plot(hc_single)
clusters = cutree(hc_single, k = 2)
plot(hc_single)
rect.hclust(hc_single, k = 2)

model_silhouette = silhouette(clusters, d)
plot(model_silhouette)
mean(model_silhouette[,3])

#complete linkage
hc_complete = hclust(d, method = "complete")
plot(hc_complete)
clusters = cutree(hc_complete, k = 2)
plot(hc_complete)
rect.hclust(hc_complete, k = 2)

model_silhouette = silhouette(clusters, d)
plot(model_silhouette)
mean(model_silhouette[,3])


#compare the 3 silhouettes found and choose the max as the best!
###################
