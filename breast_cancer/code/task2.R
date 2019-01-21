#2.1 read the dta file
load(file = "./data/bcw_processed.rta")
variables=data_processed[1:9]
outcome=data_processed[,10]

#2.2 cluster the data based on variables
set.seed(2452)
nclust=2
kmeans.result=kmeans(variables,nclust)

#2.2 Plot the results of the clusters  as a 2D plot based on kmean result and set the center of each cluster
p1=plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = kmeans.result$cluster)
title(paste("k=",nclust,sep=""))
points(kmeans.result$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col =1:nclust,pch=8,cex=2)

#2.3 Plot the results of the clusters as a 2D plot based on given classses and set the center of each cluster
p2=plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = outcome)
title(paste("k=",nclust,sep="","original graph"))
ori_mean=aggregate(data_processed[, 1:2], list(Class=data_processed$Class), mean)
points(ori_mean[2:3],col =1:nclust,pch=8,cex=2)

#2.4 The clusters generated can basically represent the benign vs malignant classes even serveral points are not excatly the same
attach(mtcars)
par(mfrow=c(1,2))
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = outcome)
title(paste("k=",nclust,sep="","original graph"))
points(kmeans.result$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col =1:nclust,pch=8,cex=2)
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = kmeans.result$cluster)
title(paste("k=",nclust,sep=""))
points(ori_mean[2:3],col =1:nclust,pch=8,cex=2)

#2.5 Cluster the data into 3,4,5 clusters and plot them
kmeans.result3=kmeans(variables,3)
kmeans.result4=kmeans(variables,4)
kmeans.result5=kmeans(variables,5)
attach(mtcars)
par(mfrow=c(1,3))
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = kmeans.result3$cluster)
title(paste("k=","3",sep=""))
points(kmeans.result3$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col =1:3,pch=8,cex=2)
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = kmeans.result4$cluster)
title(paste("k=","4",sep=""))
points(kmeans.result4$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col =1:4,pch=8,cex=2)
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col = kmeans.result5$cluster)
title(paste("k=","5",sep=""))
points(kmeans.result5$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],col =1:5,pch=8,cex=2)


#2.6 compute the SSE for the models when k = 2, 3, 4, 5 and compare
# The results are respectively 18829.18, 15747.95, 14863.43 and 13758.15. The SSE is getting lower when
# the k is getting bigger. The sse drops dramatically when k changes from 2 to 3, which means k = 3 would be 
# the most appropriate model. But actually there are only 2 classes for now ,we can do some further scientific study to find
# out whether there is a new type of class based on the kmeans result
sse=vector('numeric')
for(i in 2:5){
  #k-means function in R has a feature withinss which stores sse for each cluster group
  sse[i]=sum(kmeans(variables,centers=i)$withinss)
}
#Converting the sse to a data frame and storing corresponding value of k
sse=as.data.frame(sse)
sse

#2.7 using the hclust function with  default parameters and plot the corresponding dendrogram with clust = 2,3,4,5

n=nrow(data_processed)
idx=sample(1:n,205)
data_sample=variables[idx,]
hc=hclust(dist(data_sample),method='ave')
attach(mtcars)
par(mfrow=c(2,2))
for(nclust in 2:5){
 plot(hc,hang=-1,labels=data_processed$Class[idx])
 rect.hclust(hc,k=nclust)
 groups=cutree(hc,k=nclust)
}
#2.8 Based on the plots when ncluster= 2 and 3, we can clear see that the second cluster can be devided into 2 subtypes, but not necessarily have to.
# If to divide the disease types into 3 clusters, the number of the third one might reletively less.
# We can see the disease can basically be devided into 2 types,but can also have a subtype under the second type if
# the theretical study have proven that it is necessary.

#2.9 The outcomes of using "single" method have great differences with using other 2 methods
# And the outcomes of "complete" and "average" are basically the same.
# In my opinion, using average agglomeration method is the best,
# because its cluster structure looks more reasonable.Branches have been clustered from higher level to lower level
hc=hclust(dist(data_sample),method='com')
attach(mtcars)
par(mfrow=c(2,2))
for(nclust in 2:5){
  plot(hc,hang=-1,labels=data_processed$Class[idx])
  rect.hclust(hc,k=nclust)
  groups=cutree(hc,k=nclust)
}
hc=hclust(dist(data_sample),method='sing')
attach(mtcars)
par(mfrow=c(2,2))
for(nclust in 2:5){
  plot(hc,hang=-1,labels=data_processed$Class[idx])
  rect.hclust(hc,k=nclust)
  groups=cutree(hc,k=nclust)
}
