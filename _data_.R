#quiz code
#most of the code you ll need is already in "Practical Machine Learning in R"
#here you ll find usefull commands for the exams (esp. for clustering)
#as well as some detailed explanations <3 
#above each chapter i included the index pages for the code you 'll need from the book :P simple stuff is not included in here.

############################################################################################################################

#all the libraries you 'll need
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

data = read.csv("data.csv") #plz dont fuck this up like I did.

#Trees & GINI --> 
	library(rpart)
	library(rpart.plot)
	#for the entire tree
	model <- rpart(Target ~ ., method = "class", data = data, minsplit = 1, minbucket = 1, cp = -1) # this ~ . means "all atributes"
	rpart.plot(model, extra = 104, nn = TRUE)

	# Question from Exams. What if it asks for a single case prediction?
	newdata = training[,c(1,2,5)]
	model <- rpart(price ~ body + engine, method = "class", data = newdata, 
               minsplit = 1, minbucket = 1, cp = -1)
	rpart.plot(model, extra = 104, nn = TRUE)

	trvalue <- data.frame(body = factor("sedan", levels(newdata$body)),  
                      engine = factor("diesel", levels(newdata$engine)))
	pred = predict(model, trvalue)

	#1)GINI for all the training set
	absfreq = table(data[,"Insurance"]) #for the entire set, i select only the target column ("Insurance" is the target here"). 
	freqSum = prop.table(absfreq)
	GINI_Insurance = 1 - freqSum["Yes"]^2 - freqSum["No"]^2 # Yes/No were the 2 options of the targer column.
	#2)GINI for male (SEX). Attention! I do not calculate GINI for SEX, but for Male, which is a level of sex. 
	absfreq = table(data[,c("Sex","Insurance")]) #i select the respective columns
	freq = prop.table(absfreq, 1)
	freqSum = rowSums(prop.table(absfreq))
	GINI_M = 1 - freq["M", "No"]^2 - freq["M", "Yes"]^2 #again, be carefull on the inputs of the parenthesis. 
	#3)GINI for CarType. For the entire CarType and not one of it's level. 
	levels(data$CarType) #Usefull for viewing how many levels each attribute has 
	absfreq = table(data[,c("CarType","Insurance")])
	freq = prop.table(absfreq, 1)
	freqSum = rowSums(prop.table(absfreq))
	GINI_Family = 1 - freq["Family", "No"]^2 - freq["Family", "Yes"]^2 # I have to calculate each level first :( 
	GINI_Sedan = 1 - freq["Sedan", "No"]^2 - freq["Sedan", "Yes"]^2
	GINI_Sport = 1 - freq["Sport", "No"]^2 - freq["Sport", "Yes"]^2
	GINI_CarType = freqSum["Family"] * GINI_Family + freqSum["Sedan"] * GINI_Sedan + freqSum["Sport"] * GINI_Sport
	#4)GINI for Customer ID. (Customer ID, or any ID or that matter) always has a GINI Index of 0 
	#because the model overfits the data, 
	#since each element will always belong in a class.
	#^^These are the 4 GINI cases. 

	#Entropy for all the training set
	freq = prop.table(table(weather[, c(4)])) #c(4) is the target column! 
	Entropy_All = - freq["No"] * log2(freq["No"]) - freq["Yes"] * log2(freq["Yes"])
	#Entropy for an attribute(a column)
	absfreq = table(weather[, c(1, 4)]) #i have to select the correct column
	freq = prop.table(absfreq, 1)
	freqSum = rowSums(prop.table(absfreq))
	Entropy_Sunny = - freq["Sunny", "No"] * log(freq["Sunny", "No"])- freq["Sunny", "Yes"] * log(freq["Sunny", "Yes"])
	Entropy_Rainy = - freq["Rainy", "No"] * log(freq["Rainy", "No"])- freq["Rainy", "Yes"] * log(freq["Rainy", "Yes"])
	GAIN_Outlook = Entropy_All - freqSum["Sunny"] * Entropy_Sunny - freqSum["Rainy"] * Entropy_Rainy
	#unlike GINI, i must first calculate Entropy_All in order to find the rest

	#Create a Classification Tree (using the columns body and engine (and ofc the target))
	#To what class does the observation:"body = sedan", "engine = diesel" ,belong to?
	newdata = training[,c(1,2,5)]
	model <- rpart(price ~ body + engine, method = "class", data = newdata, 
		minsplit = 1, minbucket = 1, cp = -1)
	rpart.plot(model, extra = 104, nn = TRUE)
	trvalue <- data.frame(body = factor("sedan", levels(newdata$body)),
		engine = factor("diesel", levels(newdata$engine)))
	pred = predict(model, trvalue)
	pred


#Question: "Which attribute should we select for the first split?"
#A) Just plot the tree, and see the first split. The model will select the optimal first split on its own.
#For real, dont calculate each GINI by hand. Clock 's ticking! 

#If you have a multiple choice question, requiring to select the correct plot, you might not find the one you plotted in R
#In that case look at the last row of the tree (leaf nodes) and find the plot with the exact same leaf nodes as yours.
#The most obvious sign is that, in most plots, the number of leaf nodes will be different from your plot, 
#so you instantly disqualify these options.
#Sorry for the sedonaki. Check out quiz 2 (second to last question) for this scenario!



#NaiveBayes --> book page 47 
	model <- naiveBayes(Class ~ ., data = trainingdata) #Here, "Class" refers to the target! Could be anything (Y, Result etc...)

	#Confusion matrix and Metrics Calculation
	?ConfusionMatrix #gives info 
	ytest = testingdata[,1] #Select the correct target column! (here "Class" is the 1st column, not the last. Usually its either 1 or 3, but be carefull)
	xtest = testingdata[,-1] #all but the 1st column 
	pred = predict(model, xtest)
	ConfusionMatrix(ytest, pred)
	Precision(ytest, pred, "democrat") #if you omit "democrat" nothing important changes (not a political opinion xD)
	Recall(ytest, pred)
	F1_Score(ytest, pred)

	#ROC
	library(ROCR)
	predprob = predict(model, xtest, type = "raw") #i need the type = raw part to turn strings into numbers
	#otherwise i just put the column in the pred_object function
	pred_object = prediction(data$column, ytest, label.ordering = c("republican", "democrat"))
	ROCcurve = performance(pred_object, "tpr", "fpr")
	plot(ROCcurve, col="blue")
	#plot(ROCcurve2, col="red", add = TRUE) in order to compare the 2
	abline(0,1,col="gray")
	#To calculate the area under curve: 
	auc = performance(pred_object, "auc")
	auc@y.values

	# This is an exam question (thoery section). You are given a table and you are asked to predict the outcome of "PLAY".
	# Usually the point of theory questions is to answer them instantly, however this is a plan B, in case the answer isn't obvious. 
	WEATHER = c("SUNNY","CLOUDY","RAINY","SUNNY","RAINY")
	TEMPERATURE = c("HIGH","LOW","LOW","LOW","HIGH")
	PLAY = c("YES","YES","NO","YES","NO")
	data = data.frame(WEATHER,TEMPERATURE,PLAY)
	model <- naiveBayes(PLAY ~ ., data = data, laplace = 0)
	pred = predict(model, data.frame(WEATHER = "CLOUDY", TEMPERATURE = "HIGH" ),type = "raw" )

	#from exams (Sept. 2020) predict for a single point!
	newdata12 = training[,c(1,2,5)]
	model <- naiveBayes(price ~ ., data = newdata12, laplace = 1)
	trvalue <- data.frame(body = factor("crossover", levels(newdata12$body)),  
                      engine = factor("gas", levels(newdata12$engine)))
	predict(model, trvalue, type = "raw")

	#Create a Naive Bayes Classifier using the columns body and engine
	#To what class does the observation "body = crossover" and "engine = gas", belong to? 
	newdata12 = training[,c(1,2,5)]
	model <- naiveBayes(price ~ ., data = newdata12, laplace = 1)
	trvalue <- data.frame(body = factor("crossover", levels(newdata12$body)),  
                      engine = factor("gas", levels(newdata12$engine)))
	predict(model, trvalue, type = "raw")

#KNN --> book page 51 
	library(class)
	knn(xtrain, c(-1, 1), ytrain, k = 5, prob = TRUE) #also gives probability for the point to belong in class X!
	#example from exams
	newdata11 = training[,c(3,4,5)] #The 5th column was the "target" column, however in the new dataset i have 3 columns, the 3rd being the new target!
	knn(newdata11[,-3], c(2.3,1.2), newdata11[,3], k = 7, prob = TRUE)


#ANN --> book page (below knn :P)
	library(neuralnet)
	model <- neuralnet(Y ~ X1 + X2, alldata, hidden = 2, threshold = 0.01)
	# calculate Training Error and Mean of result
	yEstimateTrain = compute(model, alldata[, c(1:2)])$net.result
	TrainingError = alldata$Y - yEstimateTrain
	MAE = mean(abs(TrainingError))
	print(MAE)
	#you can do the same to calculate the Testing Error


#SVM --> book page 61
	library(e1071)
	model <- svm(Y ~ ., alldata, kernel = "radial", type = "C-classification", gamma = 1) #Y refers to the target! could be anything!
	ypred = predict(model, alldata[,-3]) #
	Accuracy(ypred, alldata$Y) #--> requires library(MLmetrics) alldata$Y == alldata[,3]
	#The following was asked on Sept. 2020
	prediction = as.data.frame(ypred)
	Recall(training$price, ypred, positive = "High") # "price" was the target
	
	#or for a single point
	model <- svm(Y ~ ., alldata, kernel = "radial", type = "C-classification", gamma = 1, probability = TRUE)
	new_data = data.frame(X1 = -2, X2 = -1.9)
	predict(model, new_data, probability = TRUE) #If you want to use "probability = TRUE", 
	#you must also write it when you create the model

	#training error
	training_error = c()
	for (gamma in gammavalues) {
	svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = gamma)
	pred = predict(svm_model, trainingdata[, c(1:2)])
	training_error = c(training_error, 1 - Accuracy(trainingdata$y, pred)) # y is the target!
	}

	#testing error
	testing_error = c()
	for (gamma in gammavalues) {
	svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = gamma) #we use training data!
	pred = predict(svm_model, testdata[, c(1:2)])
	testing_error = c(testing_error, 1 - Accuracy(testdata$y, pred))
	}	

	#to plot the 2 errors
	plot(training_error, type = "l", col="blue", ylim = c(0, 0.5), xlab = "Gamma", yl\
	ab = "Error", xaxt = "n")
	axis(1, at = 1:length(gammavalues), labels = gammavalues)
	lines(testing_error, col="red")
	legend("right", c("Training Error", "Testing Error"), pch = c("-","-"), col = c(\
	"blue", "red"))

	#select best gamma value using K-fold cross validation (max accuracy)
	accuracies <- c()
	for (gamma in gammavalues){
	predictions <- data.frame()
	testsets <- data.frame()
	for(i in 1:k){
	# Select 9 out of 10 folds for training and 1 for validation
	trainingset <- trainingdata[unlist(folds[-i]),]
	validationset <- trainingdata[unlist(folds[i]),]
	# Train and apply the model
	svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingset, gamma = gamma)
	pred = predict(svm_model, validationset[, c(1:2)])
	# Save predictions and testsets
	predictions <- rbind(predictions, as.data.frame(pred))
	testsets <- rbind(testsets, as.data.frame(validationset[,3]))
	}
	# Calculate the new accuracy and add it to the previous ones
	accuracies = c(accuracies, Accuracy(predictions, testsets))
	}
	print(accuracies)
	bestgamma = gammavalues[which.max(accuracies)]



###################################################################################################################################

#PCA
pca_model <- prcomp(training, center = TRUE, scale = TRUE)
eigenvalues = pca_model$sdev^2
eigenvalues[1]/sum(eigenvalues) #information integration for PC1	
sum(eigenvalues[5:9])/sum(eigenvalues)#keep the first 4 PCs and find info_loss. You "keep" the 4 first PCs, meaning you calculate i.l. with the rest!
  #for metrics --> library(MLmetrics)
  ypred = knn(training, testing, trainingType, k = 3)
  Accuracy(ypred, testingType) #testingtype = ytest, meaning just the target column from the test_data
  Recall(testingType, ypred, "2")
  eigenvectors = pca_model$rotation
  #significance
  barplot(eigenvalues / sum(eigenvalues))
  #optimal number of PC for maximizing Accuracy
  accuracies <- c()
  pValues = c(1:9)
  for (p in pValues) {
    training_pc <- as.data.frame(predict(pca_model, training)[, 1:p])
    testing_pc <- as.data.frame(predict(pca_model, testing)[, 1:p])
    ypred = knn(training_pc, testing_pc, trainingType, k = 3)
    accuracies = c(accuracies, Accuracy(ypred, testingType))
  }
  pValues[which.max(accuracies)]
  #^^paloykaki

#ISOMAP
#calculate and plot
isom <- isomap(dist(data), ndim=2, k = 4)
data_2d <- isom$points
colors = data_2d[,1] -  min(data_2d[,1]) + 1
scatterplot3d(data, angle = 88, scale.y = 5, color = colors)
  
###################################################################################################################################
  
#kmeans --> book page 99
library(cluster)
# centers vector
X = c(-4, 0, 4)
Y = c(10, 0, 10)
centers = data.frame(X,Y)
#The centers could be the first elements of the dataset eg. centers = data[1:2,]
model <- kmeans(data, centers)	#in clustering, when refering to the 'data" we do not need to include the target column.
model$tot.withinss 				#cohesion
model$betweenss 				#separation
model_silhouette <- silhouette(model$cluster, dist(data))
plot(model_silhouette)			#To visualize the results.
model1_mean_silhouette = mean(model_silhouette[, 3]) 
#although you can get it from the diagram (it says mean:...)
#you may be asked to compute a silhouette for another model so,
which.max(c(model1_mean_silhouette, model2_mean_silhouette))
#
#
#selecting the optimal number of clusters based on SSE.
SSE <- (nrow(cdata) - 1) * sum(apply(cdata, 2, var))
for (i in 2:10)
SSE[i] <- kmeans(cdata, centers = i)$tot.withinss
plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE") 
#from this plot we can extract the optimal number of clusters based on the elbow method.
#
#
#kmeans with the first N elements of the dataset as centers. which is the best N?
for (i in 2:5){
  model<-kmeans((data),centers=data[1:i,])
  print(mean(silhouette(model$cluster,dist(data))[,3]))
}
# Plot data and centers
plot(data, col = model$cluster, pch = 15, , main = "kmeans")
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)
# Heatmap
data_ord = data[order(model$cluster),]
heatmap(as.matrix(dist(data_ord)), Rowv = NA, Colv = NA,col = heat.colors(256), revC = TRUE)

########

#hclust --> needs dist on data! (also "clean" the data from the target column) --> book page 113 
target = dcdata [,3]
dcdata = dcdata[,-3] #all but the 3rd column, which is the target.
data_dist <- dist(dcdata)
model <- hclust(data_dist, method = "complete") #or single
clusters = cutree(model, k = 2)
#Accuracy requires library(MLmetrics), like all other Metrics.
Accuracy(clusters, target) #target = dcdata [,3]
#silhouette of the 1st cluster? (HC)
mean(model_silhouette[which(model_silhouette[,1]==1),3])
#
#optimal number of clusters based on silhouette?
slc = c()
for (i in 2:20){
	clusters = cutree(hc, k = i)
	slc [i-1] = mean(silhouette(clusters, d)[, 3]) }

########

#dbscan --> book page 126
library(dbscan)
target = dcdata [,3]
dcdata = dcdata[,-3] #all but the 3rd column, which is the target.
epsValues = c(0.75, 1.00, 1.25, 1.50)
for (epsilon in epsValues) {
  model <- dbscan(dcdata, eps = epsilon, minPts = 5)
  plot(dcdata, 
       col = model$cluster + 1, 
       pch = ifelse(model$cluster, 1, 4), 
       main = paste("DBSCAN with eps = ", epsilon, sep = ""))
}
#
#Number of clusters? (DBSCAN)
model = dbscan(data, eps = 0.25, minPts = 5)
clusters = model$cluster
plot(data, col = clusters + 1, pch = 15)
levels(as.factor(model$cluster))
#max(model$cluster)
#length(unique(model$cluster)) - 1
#
#
#find the best eps value given minPts=15. In which range is does the best eps value belong?
knndist = kNNdist(data, k = 15)
plot(sort(knndist), type = 'l', xlab = "Points sorted by distance", ylab = "15-NN distance") 
#I spot the knee of the curve. In knndist, k=minPts
#kNNdistplot(data,k=15)

##########

#GMMs
library(mixtools)
Y = data[,3]
data = data[,1:2]
model <- mvnormalmixEM(data, k = 3, epsilon = 0.01)
clusters = max.col(model$posterior)
centers = matrix(unlist(model$mu), byrow = TRUE, ncol = 2)
plot(data, col = clusters + 1) #plot data
points(centers, col = 1:length(centers) + 1, pch = "+", cex = 2)# plot centers


#other usufull commands
remove(list = ls())
head(data)


#Your buddy FlipFlop <3 