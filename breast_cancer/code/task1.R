library(foreign)
#1.1 Read the Data
data=read.table("./data/breast-cancer-wisconsin.data",sep=",")

#1.2 Change variables' names
names(data)=c("Sample.code.number","Clump.Thickness","Uniformity.of.Cell.Size","Uniformity.of.Cell.Shape","Marginal.Adhesion","Single.Epithelial.Cell.Size","Bare.Nuclei","Bland.Chromatin","Normal.Nucleoli","Mitoses","Class")

#1.3 Turn all "?" into na and delete the rows that contain na
data[data == '?'] <- NA
data_nona_=na.omit(data)
sum(is.na(data_nona_))

# Search NA is data and find out all Na are in column "Bare nuclei" Transform Bare nuclei's type into integer
data_nona_$Bare.Nuclei <- as.integer(data_nona_$Bare.Nuclei)

#1.4 Take the data from column 2 to 11 as processed data
data_processed=data_nona_[,2:11]

#1.5 Turn "class" into factor type
data_processed$Class <- as.factor(data_processed$Class)

#1.6 Save the data
save(data_processed, file = "./data/bcw_processed.rta")
