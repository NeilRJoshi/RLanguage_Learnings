library(dplyr)
wage <- read.csv("C:/Users/neilr/Downloads/wage.csv")
View(wage)
dim(data)
options(scipen=999)
library(boot)
library(splines)

data = wage
round(cor(data[,colnames(data) %in% c("year","age","logwage","wage")]),3)
medianwage=median(data$wage)
data$highwage=ifelse(data$wage>medianwage,1,0)

logit1=glm(highwage~year+age+education,family=binomial(link="logit"),data=data)
summary(logit1)

logit2=glm(highwage~year+age+maritl+race+education+jobclass+health+health_ins,family=binomial(link="logit"),data=data) 
summary(logit2)

probit1=glm(highwage~year+age+education,family=binomial(link="probit"),data=data)
summary(probit1)

#Trainning and validation approch
set.seed(1)
train.index=sample(1:3000,2000,replace=FALSE)
train=data[train.index,] 
valid=data[-train.index,]

logit3=glm(highwage~year+age+education,family=binomial(link="logit"),data=train)
summary(logit3)
valid$prob=predict(logit3,valid,type="response")
valid$pred=ifelse(valid$prob>0.5,1,0)

#confusion matrix
confusion=table(actual=valid$highwage,predicted=valid$pred)
confusion

#K-cross
#logit4=glm(highwage~year+age+education,family=binomial(link="logit"),data=data)

#Q1---------------------------------------------------------------------
a = c(2,3,4,5,6)
for(n in a){
set.seed(1)
mod_q1=glm(highwage~poly(age,degree=n,raw=TRUE),family=binomial(link="logit"),data=data)

cost.accuracy=function(r, pi = 0) mean(abs(r-pi)<0.5)
cv.accuracy=boot::cv.glm(data,mod_q1,cost=cost.accuracy,K=10)

print(cv.accuracy$delta[1])
}

#Q2----------------------------------------------------------------------
  r = c(2,3,4,5,6,7,8,9,10)
  for(n in r){
  set.seed(1)
  
  ti = sample(1:nrow(data),(0.6*nrow(data)), replace = FALSE)
  
  data$agedummy=cut(data$age,breaks=n)
  levels(data$agedummy)
  table(data$agedummy)
  
  train_q2 = data[ti,]
  valid_q2 = data[-ti,]
  
  
  
  dummy_q2=glm(highwage~agedummy,family=binomial(link="logit"),data=train_q2)
  summary(dummy_q2)
  
  
  valid_q2$probab = predict(dummy_q2,valid_q2,type="response")
  valid_q2$pred=ifelse(valid_q2$probab>0.5,1,0)
  ConfusionTable = table(actual=valid_q2$highwage,predicted=valid_q2$pred)
  
  TP=ConfusionTable[1,1]
  FN=ConfusionTable[1,2]
  FP=ConfusionTable[2,1]
  TN=ConfusionTable[2,2]
  accuracy=(TP+TN)/nrow(valid_q2)
  precision=TP/(TP+FP)
  recall=TP/(TP+FN)
  error=1-accuracy
  print(accuracy)
  }
#Q3------------------------------------------------------------------------

c = c(3,4,5,6,7)
for (h in c) {
  set.seed(1)
  gam1=glm(highwage~ns(year,df=h)+ns(age,df=h)+education + jobclass + health + health_ins,family=binomial(link="logit"),data=data)
  summary(gam1)
  cv.accuracy_q3=boot::cv.glm(data,gam1,cost=cost.accuracy,K=10)
  print(cv.accuracy_q3$delta[1])
}
