Hitters <- read.csv("C:/Users/neilr/Downloads/Hitters.csv")
View(Hitters)
data = Hitters
View(data)
options(scipen=999)
library(leaps)
install.packages("glmnet")
require(glmnet)

sum(is.na(data$Salary))
data=na.omit(data)
sum(is.na(data))

#exhaustive
model1=regsubsets(Salary~.,data,nvmax=19,method="exhaustive")
model1.summary=summary(model1)
model1.summary$adjr2


par(mfrow=c(3,1))
plot(model1.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
index=which.min(model1.summary$rss)
points(index,model1.summary$rss[index],col="red",cex=2,pch=20)
which.max(model1.summary$adjr2)
which.min(model1.summary$bic)
coef(model1,6)

#Test and Validation method
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(data),replace=TRUE)
test=!train
model2=regsubsets(Salary~.,data[train,],nvmax=19,method="exhaustive")
test.mat=model.matrix(Salary~.,data[test,])
val.errors=numeric(19)
for(i in 1:19)
  {
  coefi=coef(model2,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((data$Salary[test]-pred)^2)
  }
val.errors
best=which.min(val.errors)
coef(model2,best)

model2.full=regsubsets(Salary~.,data,nvmax=19,method="exhaustive")
coefi=coef(model2.full,best)
coefi
subset.pred=test.mat[,names(coefi)]%*%coefi

#CrossValidation
k=10
set.seed(1)
folds=sample(1:k,nrow(data),replace=TRUE)
cv.errors=matrix(0,k,19,dimnames=list(NULL,as.character(1:19)))
dim(cv.errors)

for(j in 1:k){
  model3=regsubsets(Salary~.,data[folds!=j,],nvmax=19,method="exhaustive")
  test.mat=model.matrix(Salary~.,data=data[folds==j,])
  for (i in 1:19){
    coefi=coef(model3,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    cv.errors[j,i]=mean((data$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,MARGIN=2,mean)
plot(mean.cv.errors,type="b")
best=which.min(mean.cv.errors)
model3.full=regsubsets(Salary~.,data,nvmax=19,method="exhaustive")
coef(model3.full,best)

#Ridge Regression
library(glmnet)
install.packages("glmnet")

grid=10^seq(10,-2,length=100)
y=data$Salary
x=model.matrix(Salary~.,data)[,-1]
model4=glmnet(x=x,y=y,alhpa=0,lambda=grid)
dim(coef(model4))
names(model4)
model4$lambda[70]
coef(model4)[,70]
plot(model4,xvar="lambda")

#choosing lamda using cross-val
cv.out=cv.glmnet(x=x,y=y,alpha=0,nfolds=10)
plot(cv.out)
names(cv.out)
bestlambda=cv.out$lambda.min
model4.full=glmnet(x=x,y=y,alhpa=1,lambda=bestlambda)
predict(model4.full,s=bestlambda,type="coefficients")[1:20,]
cv.out$cvm[cv.out$lambda==bestlambda]
ridge.pred=predict(model4.full,newx=x,type="link")

check = data.frame(AtBat=315,
                   Hits=81,
                   HmRun=7,
                   Runs=24,
                   RBI=38,
                   Walks=39,
                   Years=14,
                   CAtBat=3449,
                   CHits=835,
                   CHmRun=69,
                   CRuns=321,
                   CRBI=414,
                   CWalks=375,
                   League="N",
                   Division="W",
                   PutOuts=632,
                   Assists=43,
                   Errors=10,
                   Salary=475,
                   NewLeague="N"
)



library(leaps)

#Q1--------------------------------------------------------------------------
k=5
set.seed(1)
folds=sample(1:k,nrow(data),replace=TRUE)
cv.errors=matrix(0,k,19,dimnames=list(NULL,as.character(1:19)))
dim(cv.errors)



for(j in 1:k){
  model_q1=regsubsets(Salary~.,data[folds!=j,],nvmax=19,method="forward")
  test.mat=model.matrix(Salary~.,data=data[folds==j,])
  for (i in 1:19){
    coefi=coef(model_q1,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    cv.errors[j,i]=mean((data$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,MARGIN=2,mean)
plot(mean.cv.errors,type="b")
best=which.min(mean.cv.errors)
model_q1.full=regsubsets(Salary~.,data,nvmax=19,method="forward")
coef(model_q1.full,best)

#Q2-----------------------------------------------------------------------------
k=5
set.seed(1)
folds=sample(1:k,nrow(data),replace=TRUE)
cv.errors=matrix(0,k,19,dimnames=list(NULL,as.character(1:19)))
dim(cv.errors)



for(j in 1:k){
  model_q2=regsubsets(Salary~.,data[folds!=j,],nvmax=19,method="backward")
  test.mat=model.matrix(Salary~.,data=data[folds==j,])
  for (i in 1:19){
    coefi=coef(model_q2,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    cv.errors[j,i]=mean((data$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,MARGIN=2,mean)
plot(mean.cv.errors,type="b")
best=which.min(mean.cv.errors)
model_q2.full=regsubsets(Salary~.,data,nvmax=19,method="backward")
coef(model_q2.full,best)

#Q3-----------------------------------------------------------------------
#Ridge
set.seed(1)
y=data$Salary
x=model.matrix(Salary~.,data)[,-1]
cv.out=cv.glmnet(x=x,y=y,alpha=0,nfolds=5)
plot(cv.out)
names(cv.out)
bestlambda=cv.out$lambda.min
model_q3a.full=glmnet(x=x,y=y,alpha=0,lambda=bestlambda)
predict(model_q3a.full,s=bestlambda,type="coefficients")[1:20,]

cv.out$cvm[cv.out$lambda==bestlambda]

ridge.pred=predict(model_q3a.full,newx=x,type="link")

#LASSO
set.seed(1)
cv.out=cv.glmnet(x=x,y=y,alpha=1,nfolds=5)
bestlambda=cv.out$lambda.min
model_q3b.full=glmnet(x=x,y=y,alpha=1,lambda=bestlambda)
predict(model_q3b.full,s=bestlambda,type="coefficients")[1:20,]

cv.out$cvm[cv.out$lambda==bestlambda]

lasso.pred=predict(model_q3b.full,newx=x,s=bestlambda,type="link")

