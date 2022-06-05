Lesliesalt<- read_excel(file.choose())

View(Lesliesalt)

str(Lesliesalt)
summary(Lesliesalt)

Lesliesalt$County<-as.factor(Lesliesalt$County)
Lesliesalt$Flood<-as.factor(Lesliesalt$Flood)

attach(Lesliesalt)

#Correlation matrix
cor(Lesliesalt[,c(-2,-7)])
corPlot(cor(Lesliesalt[,c(-2,-7)]))

#Correlation Plot
corrplot(cor(Lesliesalt[,c(-2,-7)]))

#Scatter Plot
#Scatter Plot
plot(Price,Date,main = "Price Vs. Date")
plot(Price, Elevation, main = "Price Vs. Elevation")
plot(Size, Price, main = "Price Vs. Size")
plot(Sewer, Price, main = "Price Vs. Sewer")
plot(Distance, Price, main = "Price Vs. Distance")

#In scatter plot of Size vs price, we can see that higher the size have lower the price.
#Which makes it questionable. Hence, ingnoring the variable for analysis.
#Distance to Price scatter plot doesn't show strong correlation.

#Boxplot
boxplot(Lesliesalt$Price, main = "Price")
boxplot(Lesliesalt$Date, main = "Date")
boxplot(Lesliesalt$Elevation, main = "Elevation")
boxplot(Lesliesalt$Size, main = "Size")
boxplot(Lesliesalt$Sewer, main = "Sewer")
boxplot(Lesliesalt$Distance, main = "Distance")

#Regression Assumptions
with(Lesliesalt, plot(Size, Price, pch=19, cex=0.6))
with(Lesliesalt, plot(Sewer, Price, pch=19, cex=0.6))
with(Lesliesalt, plot(Distance, Price, pch=19, cex=0.6))

#Running full model regression
Leslie_rm <-lm(Price~., data = Lesliesalt)
anova(Leslie_rm)
summary(Leslie_rm)

#Running regression excluding Distance variable
Leslie_rm1 <- lm(Price~. -Distance, data = Lesliesalt)
anova(Leslie_rm1)
summary(Leslie_rm1)

#Running another model
Leslie_rm2 <- lm(Price~Elevation+Sewer+County+Flood, data= Lesliesalt)
summary(Leslie_rm2)

#Extracted Residual and Fitted values from Leslier_rm2 model
fit1<-fitted(Leslie_rm2)
res1<-residuals(Leslie_rm2)

##Merge the fitted and residual values with LeslieSalt dataset for comparison sake
LeslieReg <- cbind(Lesliesalt, fit1, res1)

##Plot the actual versus fitted values in a plot
with(LeslieReg, plot(fit1,res1, pch=19, cex=0.6))
abline(a=0,b=0)
plot(fit1,res1)