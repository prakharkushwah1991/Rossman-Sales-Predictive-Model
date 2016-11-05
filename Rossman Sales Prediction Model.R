#ASSIGNMENT 4: BUILDING A LINEAR REGRESSION MODEL.
setwd("C:/Users/prakhar/Desktop/rossman store sales")
train<-read.csv("input/train.csv",header = T)
train
test<-read.csv("input/test.csv",header = T)
test
store<-read.csv("input/store.csv")
store

#merge the data sets.
train<-merge(train,store)
test<-merge(test,store)

head(train)
summary(train)

#replacing na's by mean value.
munge_data<-function(dt){
  dt$CompetitionDistance[is.na(dt$CompetitionDistance)]=round(mean(dt$CompetitionDistance,na.rm = T))
  dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)]=round(mean(dt$CompetitionOpenSinceMonth,na.rm = T))
  dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)]=round(mean(dt$CompetitionOpenSinceYear,na.rm = T))  
  dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)]=round(mean(dt$Promo2SinceWeek,na.rm = T))
  dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)]=round(mean(dt$Promo2SinceYear,na.rm = T))
  
  #converting to numeric values
  dt$StateHoliday=as.numeric(dt$StateHoliday)
  dt$Assortment=as.numeric(dt$Assortment)
  dt$StoreType=as.numeric(dt$StoreType)
  dt$PromoInterval=as.numeric(dt$PromoInterval)
  
  #separating date columns
  dt$Date=as.Date(dt$Date,format="%Y-%m-%d")
  dt$month<-as.integer(format(dt$Date,"%m"))
  dt$day<-as.integer(format(dt$Date,"%d"))
  dt$year<-as.integer(format(dt$Date,"%y"))
  
  return(dt)
  
}
train=munge_data(train)
train
head(train)
summary(train)

#identifying numeric variables
numericdata<-train[sapply(train,is.numeric)]
numericdata

#identifying correlation.
descrCor<-cor(numericdata)
install.packages("ggplot2")
installed.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("caret")
library(caret)
highlyCorrelated<-findCorrelation(descrCor,cutoff = 0.6)
#identifying variable names of highly correlated variables.
highlyCorCol<-colnames(numericdata)[highlyCorrelated]
#print highly correlated attributes
highlyCorCol

#remove highly correlated variables and create a new dataset
dat3<-train[,which(colnames(train)%in%highlyCorCol)]
dim(dat3)

#Build a linear regression model.
fit=lm(Sales~.,data = dat3)

#check model performance
summary(fit)

#extracting coefficients
summary(fit)$coeff

#extracting r-squared value
summary(fit)$r.squared

#extracting adj r squared
summary(fit)$adj.r.squared

#stepwise selection based on AIC.
install.packages("MASS")
library(MASS)
step<-stepAIC(fit,direction = "both")
summary(step)

# backward selection based on AIC
step<-stepAIC(fit,direction = "backward")
summary(step)

# forward selection based on AIC.
step<-stepAIC(fit,direction = "forward")
summary(step)


#step wise selection with BIC
n=dim(dat3)[1]
stepBIC=stepAIC(fit,k=log(n))
summary(stepBIC)

#R function: standardised coefficients
stdz.coff<-function(regmodel){
  b<-summary(regmodel)$coef[-1,1]
  sx<-sapply(regmodel$model[-1],sd)
  sy<-sapply(regmodel$model[-1],sd)
  beta<-b*sx/sy
  return(beta)
}

std.Coeff=data.frame(Standardized.Coeff=stdz.coff(stepBIC))
std.Coeff=cbind(variable=row.names(std.Coeff),std.Coeff)
row.names(std.Coeff)=NULL

#checking multicollearity
#check for VIF of all the variables

vif(stepBIC)


#Autocorrelation test
durbinWatsonTest(stepBIC)


#normality of residuals(should be > 0.05)
res=residuals(stepBIC,type = "pearson")
shapiro.test(res)

#test for heteroscadasticity (should be > 0.05)
ncvTest(stepBIC)

#outliers-Bonferonni test
outlierTest(stepBIC)

#see residuals
resid<-residuals(stepBIC)

#relative importance
install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepBIC)

#predicted value
pred=predict(stepBIC,dat3)

#predicted vs actual values
finaldata=cbind(train,pred)
print(head(subset(finaldata,select = c(Sales,pred))))

#calculate RMSE
rmse <- sqrt(mean((dat3$Sales - pred)^2))
print(rmse)

#calculating rmse manually
y=dat3[,c("Sales")]
R.squared = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
print(R.squared)

#calculating adj R squared manually
n=dim(dat3)[1]
p=dim(summary(stepBIC)$coeff)[1]-1
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
print(adj.r.squared)

#Box cox transformation.
install.packages("lmSupport")
library(lmSupport)
modelBoxCox(stepBIC)


































