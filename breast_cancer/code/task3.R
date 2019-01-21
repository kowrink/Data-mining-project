#3.1 load the data,divde the data into testing and training dataset
load(file = "./data/bcw_processed.rta")
seed=(2452)
sample=sample.int(n = nrow(data_processed), size = floor(.70*nrow(data_processed)), replace = F)
train=data_processed[sample, ]
test=data_processed[-sample, ]
train_vars=train[,1:9]
train_labs=train[,10]
test_vars=test[,1:9]
testn_labs=test[,10]

#3.2 Build a tree model to predict the class  and Calculate the accuracy, precision, and recall. 
#Assume all the variables are obey to normal distribution, Use ttest to caculate whether different classes would 
# have different distribution on the variable
lapply(data_processed[-10], function(x) t.test(x ~ data_processed$Class))
#All the p value are lower than 0.05,For each variable, there is a statistical difference between the different classes
#So all the variablesv should put into the model
install.packages("party")
library(party)
class_formula= Class ~ Mitoses + Normal.Nucleoli + Bland.Chromatin + Bare.Nuclei + 
  Single.Epithelial.Cell.Size + Marginal.Adhesion + Uniformity.of.Cell.Shape +Uniformity.of.Cell.Size+Clump.Thickness
tree=ctree(class_formula,data=train)
plot(tree)
plot(tree,type="simple")
tree_pred=predict(tree,newdata=test_vars)
tree_matrix=as.matrix(table(Actual=testn_labs,Predicted=tree_pred))
n=sum(tree_matrix)
nc=nrow(tree_matrix)
diag=diag(tree_matrix)
rowsums=apply(tree_matrix,1,sum)
colsums=apply(tree_matrix,2,sum)
accuracy=sum(diag)/n
precision=diag/colsums
recall=diag/rowsums
f1=2*precision*recall/(precision+recall)
eva_result=data.frame(precision,recall,f1)
accuracy
eva_result


#3.4 Apply K-NN classification to predict the labels when n=1,2,3,4,5 and return the accuracy,precision,recall of different n
install.packages("class")
library(class)
for(i in 1:5){
  #k-means function in R has a feature withinss which stores sse for each cluster group
  knn_pred=knn(train=train_vars,test=test_vars,cl=train_labs,k=i)
  knn_matrix=as.matrix(table(Actual=testn_labs,Predicted=knn_pred))
  n[i]=sum(knn_matrix)
  nc[i]=nrow(knn_matrix)
  diag[i]=diag(knn_matrix)
  rowsums[i]=apply(knn_matrix,1,sum)
  colsums[i]=apply(knn_matrix,2,sum)
  accuracy[i]=sum(diag[i])/n
  precision[i]=diag[i]/colsums
  recall[i]=diag[i]/rowsums
  f1[i]=2*precision*recall/(precision+recall)
}
#Converting the sse to a data frame and storing corresponding value of k
conclusion=cbind.data.frame(accuracy,precision,recall,f1)
conclusion


