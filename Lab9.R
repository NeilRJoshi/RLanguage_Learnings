data <- read.csv("C:/Users/neilr/Downloads/ToyotaCorolla.csv")
View(data)
library(dplyr)
dim(data)
data = data[1:1000,]
car.df=select(data,Price,Age=Age_08_04,Kilometers=KM,FuelType=Fuel_Type,HP,Metallic=Met_Color,Automatic,CC,Doors,QuartTax=Quarterly_Tax,Weight)
View(car.df)
install.packages("forecast")
library(forecast)
install.packages("leaps")
library(leaps)


a1 = car.df[,!colnames(car.df) %in% c("FuelType","Metallic","Automatic")]
round(cor(a1),3)
plot(a1)

model1 = lm(Price~., data = car.df)
summary(model1)

table(car.df$FuelType)

model2 = lm(Price~Age+Kilometers+HP*FuelType+Metallic+Automatic+CC+Doors+QuartTax+Weight,data=car.df)
summary(model2)
plot(car.df$Price~car.df$Kilometers,type="p")
pricekilo=lm(Price~Kilometers,data=car.df)
summary(pricekilo)

abline(pricekilo,col="blue",lwd=2)

pricekilo2=lm(Price~Kilometers+I(Kilometers^2),data=car.df)
summary(pricekilo2)
coef=pricekilo2$coefficients
x=1:250000
y=function(x) {
  return(coef[1]+coef[2]*x+coef[3]*x^2)}
points(x=x,y=y(x),type="l",col="red",lwd=2)
car.df3=data.frame(car.df,Kilometers2=car.df$Kilometers^2)
model3=lm(Price~.,data=car.df3)
summary(model3)
cor(car.df3$Kilometers,car.df3$Kilometers2)
FuelDiesel=ifelse(car.df$FuelType=="Diesel",1,0)
FuelPetrol=ifelse(car.df$FuelType=="Petrol",1,0)
FuelCNG=ifelse(car.df$FuelType=="CNG",1,0)
car.df2=cbind(car.df,FuelDiesel,FuelPetrol, FuelCNG)
View(car.df2)

#-------------------------------------------------------------------------------------------
#Q1.
plot(car.df$Price~car.df$Age, type="p")

fd = lm(Price~Age,data = car.df2[car.df2$FuelDiesel==1,])
fp = lm(Price~Age,data = car.df2[car.df2$FuelPetrol==1,])
fc = lm(Price~Age,data = car.df2[car.df2$FuelCNG==1,])

abline(fd,col="blue",lwd=2)
abline(fp,col="red",lwd=2)
abline(fc,col="orange",lwd=2)

#Q2.------------------------------------------------------
modelq2 = lm(Price~Age*FuelType, data = car.df2)
summary(modelq2)

#Q3--------------------------------------------------------
modelq3 = lm(Price~Age*FuelType + ., data = car.df2)
summary(modelq3)

#Q4-------------------------------------------------------
set.seed(1)
train.index=sample(c(1:1000),700,replace=FALSE)
train.df=car.df[train.index,] 
valid.df=car.df[-train.index,]

mod4=lm(Price~.,data=train.df)
summary(mod4)

pred4=predict(mod4,valid.df)
error=valid.df$Price-pred4

forecast::accuracy(pred4,valid.df$Price)
search=leaps::regsubsets(Price~.,data=train.df,nbest=1,nvmax=ncol(train.df),method="exhaustive")
res=summary(search)
names(res)
res$which
plot(1:11,res$adjr2,type="b")
plot(1:11,res$bic,typ="b",col="red")

mod5=step(mod4,direction="forward")
summary(mod5)
pred5=predict(mod5,valid.df)
forecast::accuracy(pred5,valid.df$Price)
