data=read.csv("data")
#esto oti xrisimopoioume ola ta data, allios used_data=data[,n:k] analogos tis stiles
#1o erotima
model<-kmeans(data, centers=data[1:2,])
model$centers #edo pairno apo to proto center th deyterh sthlh

#2o erotima
model<-kmeans(data, centers=data[1:3,])
mean(silhouette(model$cluster,dist(data))[,3])

#3o erotima
for (i in 2:5){
  model<-kmeans((data),centers=data[1:i,])
  print(mean(silhouette(model$cluster,dist(data))[,3]))
}
#ginontai print ta silhouettes->dialego th megalyterh...ginetai kai me dhmiourgia vector kai euresh max

#4o erotima
model<-dbscan(data,minPts=10, eps=0.05)
levels(as.factor(model$cluster)) #ta pairno ola ektos toy 0 (einai noise, de mas noiazei)

#5o erotima
knndist = kNNdist(data, k = 15)
plot(sort(knndist), type = 'l', xlab = "Points sorted by distance", ylab = "15-NN distance") 
#brisko peripoy poy kanei gonato...minPts=k sto knndist

#6o erotima
hc<-hclust(dist(data),method="complete")
clusters=cutree(hc,k=3)
model_sil=silhouette(clusters,dist(data))
mean(model_sil[which(model_sil[,1]==1),3])
