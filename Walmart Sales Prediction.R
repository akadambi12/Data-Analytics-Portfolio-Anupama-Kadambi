#Final Project
#Anupama Kadambi 
#ASU ID-1222559637
# Checks if package is installed, installs if necessary, and loads package for current session.

install.packages('lubridate')
library(tidyverse)
install.packages('dplyr')
library(magrittr)

walmart<-as.data.frame(read.csv('Walmart.csv'))
head(walmart)


walmart3<-as.data.frame(walmart[ ,c(3,5,6,7,8)])
walmart3 <- walmart3 %>% mutate_all(~(scale(.) %>% as.vector))#Standardizing


#-------------------------------------------------------------------------------
#Feature importance

library(earth)
regressor <- earth(walmart3$Weekly_Sales ~ . , data=walmart3)
ev <- evimp (regressor) # estimate variable importance
plot (ev)
print(ev)
#-------------------------------------------------------------------------------
install.packages('lubridate')
library(lubridate)
date<-as.Date(walmart$Date,format="%d-%m-%Y")
walmart$Months<-as.numeric(format(date,"%m"))
walmart$Years<-as.numeric(format(date,'%Y'))
#-------------------------------------------------------------------------------
#Multicollinearity
#correlation
walmart2<-walmart[ ,c(1,3,4,5,6,7,8,9,10)]
cor(walmart2)

#-------------------------------------------------------------------------------
#correlation heatmap
install.packages("reshape2")
library(reshape2)
corr_mat <- round(cor(walmart2),2)
melted_corr_mat <- melt(corr_mat) #reduce size of correlated data
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,fill=value)) +geom_tile()
cor(walmart2)
#-------------------------------------------------------------------------------

#Changing Store number to categorical variable.
walmart$Strn<-NA
walmart$Strn<-as.factor(walmart$Store)

#Changing Holiday to Binary variables
walmart$Is_Hol<-NA
walmart$Is_Hol<-as.factor(walmart$Holiday_Flag)

#-------------------------------------------------------------------------------

#Converting Months to categorical variable
walmart$Month<-NA
walmart$Month<-as.factor(walmart$Months)

#Converting Years to categorical variable
walmart$Year<-NA
walmart$Year<-as.factor(walmart$Years)

#Number of categorical values
table(walmart$Is_Hol)

walmart<-walmart[ ,c(2,3,5,6,7,8,11,12,13,14)]
colnames(walmart)<-c("Date","Weekly_sales","Temp","Fuel_price","CPI","Unemployment","Store_number","Is_Holiday","Month","Year")
#-------------------------------------------------------------------------------
#Exploratory Data Analysis
#-------------------------------------------------------------------------------
#Checking for missing values in data.eg-NA
which(is.na(walmart$Store_number))
which(is.na(walmart$Date))
which(is.na(walmart$Weekly_sales))
which(is.na(walmart$Temp))
which(is.na(walmart$Fuel_price))
which(is.na(walmart$CPI))
which(is.na(walmart$Unemployment))
which(is.na(walmart$Is_Holiday))
which(is.na(walmart$Month))
which(is.na(walmart$Year))

#-------------------------------------------------------------------------------
#Data visualization 
library(dplyr)
library(ggplot2)
ggplot(data=walmart,aes(x=walmart$Date,y=walmart$Weekly_sales,color=walmart$Is_Holiday))+geom_point()+labs(title="Sales on different days",xlab="Date",ylab="Weekly sales")

#-------------------------------------------------------------------------------
#Create histogram for response variable
hist(walmart$Weekly_sales,main="Weekly sales at Walmart")
ggplot(walmart, aes(x=walmart$Temp)) + geom_histogram(bins=100)+labs(title="Histogram for Temperature")
ggplot(walmart, aes(x=walmart$Weekly_sales)) + geom_histogram(bins=100)+labs(title="Histogram for Sales")
ggplot(walmart, aes(x=walmart$CPI)) + geom_histogram(bins=100)+labs(title="Histogram for CPI")
ggplot(walmart, aes(x=walmart$Unemployment)) + geom_histogram(bins=100)+labs(title="Histogram for Unemployment rate")
ggplot(walmart, aes(x=walmart$Fuel_price)) + geom_histogram(bins=100)+labs(title="Histogram for Fuel rate")
#-------------------------------------------------------------------------------

#Scatter plots for numerical predictors.
library(dplyr)
library(ggplot2)
ggplot(data=walmart,aes(x=walmart$Temp,y=walmart$Weekly_sales))+geom_point()+labs(title="Temperature vs sales",xlab="Temperature",ylab="Weekly sales")
ggplot(data=walmart,aes(x=walmart$Fuel_price,y=walmart$Weekly_sales))+geom_point()+labs(title="Fuel price vs sales",xlab="Fuel price",ylab="Weekly sales")
ggplot(data=walmart,aes(x=walmart$CPI,y=walmart$Weekly_sales))+geom_point()+labs(title="CPI vs sales",xlab="CPI",ylab="Weekly sales")
ggplot(data=walmart,aes(x=walmart$Unemployment,y=walmart$Weekly_sales))+geom_point()+labs(title="Unemployment rate vs sales",xlab="unemployment rate",ylab="Weekly sales")

#-------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
#Plots for numerical predictors vs Sales
ggplot(data=walmart, aes(x=walmart$Temp, y=walmart$Weekly_sales)) +geom_bar(stat="identity")
ggplot(data=walmart, aes(x=walmart$Fuel_price, y=walmart$Weekly_sales)) +geom_bar(stat="identity")
ggplot(data=walmart, aes(x=walmart$CPI, y=walmart$Weekly_sales)) +geom_bar(stat="identity",width=0.5)
ggplot(data=walmart, aes(x=walmart$Unemployment, y=walmart$Weekly_sales)) +geom_bar(stat="identity")
#-------------------------------------------------------------------------------
#Boxplots for numerical predictor
boxplot(walmart$Weekly_sales,main=" Boxplot for Sales")
boxplot(walmart$Temp,main=" Boxplot for Temperature")
boxplot(walmart$Fuel_price,main="boxplot for Fuel prices")
boxplot(walmart$CPI,main="Boxplot for CPI")
boxplot(walmart$Unemployment,main="Boxplot for Unemployment rate")

#-------------------------------------------------------------------------------
#Density plot
library(dplyr)
library(ggplot2)
walmart %>%  ggplot(aes(x=Weekly_sales))+geom_density(color="midnightblue",fill="skyblue")+ggtitle("Density plot for Weekly sales")
#-------------------------------------------------------------------------------
#plot regression analysis between sales and stores
m1<-lm(walmart$Weekly_sales~(walmart$Store_number))
plot(walmart$Weekly_sales~walmart$Store_number,main="regression analysis between sales and stores")
abline(m1,col="blue",lwd=2)
summary(m1)

#Regression analysis between sales and fuel prices
m2<-lm(walmart$Weekly_sales~walmart$Fuel_pr)
plot(walmart$Weekly_sales~walmart$Fuel_pr)
abline(m2,col="blue",lwd=2)

#Regression analysis between sales and unemployment
m3<-lm(walmart$Weekly_sales~walmart$Unemployment)
plot(walmart$Weekly_sales~walmart$Unemployment)
abline(m3,col="blue",lwd=2)

#Regression analysis between sales and Temperature
m4<-lm(walmart$Weekly_sales~walmart$Temp)
plot(walmart$Weekly_sales~walmart$Temp)
abline(m4,col="blue",lwd=2)  

#Regression analysis between sales and CPI
m5<-lm(walmart$Weekly_sales~walmart$CPI)
plot(walmart$Weekly_sales~walmart$CPI)
abline(m5,col="blue",lwd=2)

#Regression analysis between sales and Holidays
m6<-lm(walmart$Weekly_sales~walmart$Is_Holiday)
plot(walmart$Weekly_sales~walmart$Is_Holiday)
abline(m6,col="blue",lwd=2)

#-------------------------------------------------------------------------------
#Fit a model
library(ggplot2)
model<-lm(walmart$Weekly_sales~walmart$Temp+walmart$Fuel_price+walmart$CPI+walmart$Unemployment+walmart$Store_number+walmart$Is_Holiday+walmart$Month+walmart$Year,data=walmart)
summary(model)
coefficients(model)
plot(x=predict(model), y=walmart$Weekly_sales,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

#add diagonal line for estimated regression line
abline(a=0, b=1,col="red")


confint(model, level=0.95) # CIs for model parameters
fitted(model) # predicted values
residuals(model) # residuals
anova(model) # anova table
vcov(model) # covariance matrix for model parameters
influence(model) # regression diagnostics
y.hat<-predict(model)
plot(walmart$Weekly_sales,y.hat)
ggplot(walmart, aes(x = walmart$Weekly_sales, y = y.hat)) + 
  geom_point(alpha=0.7) +   
  geom_abline(slope = model$coefficients[[2]],
              intercept = model$coefficients[[1]],
              color='red', alpha=0.5)

#-------------------------------------------------------------------------------
#Other models
reduced.model1<-lm(walmart$Weekly_sales~walmart$Temp:walmart$Unemployment+walmart$Fuel_price:walmart$Unemployment+walmart$CPI:walmart$Unemployment+walmart$Unemployment+walmart$Store_number+walmart$Is_Holiday+walmart$Month+walmart$Year,data=walmart)
summary(reduced.model1)
reduced.model2<-lm(walmart$Weekly_sales~walmart$Temp+walmart$Fuel_price+walmart$CPI+walmart$Unemployment+walmart$Store_number+walmart$Is_Holiday)
summary(reduced.model2)
reduced.model3<-lm(walmart$Weekly_sales~walmart$Temp:walmart$Unemployment+walmart$Fuel_price:walmart$Unemployment+walmart$CPI:walmart$Unemployment+walmart$Unemployment+walmart$Store_number+walmart$Is_Holiday)
summary(reduced.model3)
model4<-lm(walmart$Weekly_sales~walmart$Temp+walmart$Fuel_price+walmart$CPI+walmart$Store_number+walmart$Is_Holiday+walmart$Month+walmart$Year,data=walmart)
summary(model4)
#-------------------------------------------------------------------------------
# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(model)

#-------------------------------------------------------------------------------
#Marginal relationships
library(car)
install.packages("tidyverse")
install.packages("GGally")
install.packages("ISLR")
library(tidyverse)
library(GGally)
library(ISLR)


marginalModelPlots(model)
pairs(walmart)
#-------------------------------------------------------------------------------
#Added variable plots
#many plots
library(car)
avPlots(model,id=list(labels=row.names(walmart)))

#-------------------------------------------------------------------------------
#Normal QQ Plot
qqPlot(model)

#-------------------------------------------------------------------------------
#Detecting Influential Points
influencePlot(model,id=list(labels=row.names(walmart)))
influenceIndexPlot(model,vars=c("Studentized","hat","Cook"),id=list(labels=row.names(walmart)))

#-------------------------------------------------------------------------------
library(binomTools)
library(generalhoslem)
library(caret)
library(OptimalCutpoints)

#-------------------------------------------------------------------------------
#Predicting
predicted<-predict(model)
res<-residuals(model)
res

#-------------------------------------------------------------------------------
#Residual Plots  
library(car)
residualPlots(model,~1,type="rstudent",id=list(labels=row.names(walmart)))
library(ggplot2)
#res<-resid(model)
plot(fitted(model), res)
#add a horizontal line at 0 
abline(0,0,col="blue",main="Residual plot")

#-------------------------------------------------------------------------------
install.packages('randomForest')
library(randomForest)
library(ggplot2)

set.seed(4543)

rf.fit <- randomForest(walmart$Weekly_sales ~ ., data=walmart, ntree=170,keep.forest=FALSE, importance=TRUE)
rf.fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
plot(rf.fit)
varImpPlot(rf.fit)

#-------------------------------------------------------------------------------




