library(readxl)
Franchise<-read_excel(file.choose())
str(Franchise)
summary(Franchise)

library(DataExplorer)
DataExplorer::create_report(Franchise)
boxplot(Franchise)
boxplot(Franchise$X1)
boxplot(Franchise$X2)
boxplot(Franchise$X3)
boxplot(Franchise$X4)
boxplot(Franchise$X5)
boxplot(Franchise$X6)

library(car)
scatterplot(Franchise$X3, Franchise$X1)
scatterplot(Franchise$X3, Franchise$X2)
scatterplot(Franchise$X2, Franchise$X1)
scatterplot(Franchise$X4, Franchise$X1)
scatterplot(Franchise$X5, Franchise$X1)
scatterplot(Franchise$X6, Franchise$X1)
scatterplot(Franchise$X6, Franchise$X5)

#Renaming the variables
names(Franchise)<-c("NetSales","StoreSize","InventoryCost","AdvertisingCost","Density","NumOfStores")

#Correlation matrix of dataset
cor(Franchise)
#Plotting the correlation diagram
corPlot(cor(Franchise))

#Running full linear regression model
Franchise_rm1<-lm(Franchise$NetSales~Franchise$StoreSize+Franchise$InventoryCost+Franchise$AdvertisingCost+Franchise$Density+Franchise$NumOfStores)
summary(Franchise_rm1)
