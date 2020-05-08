st=as.data.frame(state.x77)
View(st)
summary(st)

#rename columns
colnames(st)[4]="LifeExp"
colnames(st)[6]="HSGrad"
#create a new variable as population density
Density=st$Population*1000/st$Area
st=cbind(state.name,st,Density,state.center,state.division,state.region)
colnames(st)[1]="statename"
colnames(st)[13]="division"
colnames(st)[14]="region"
summary(st)

#Q1--------------------------------------
corr=round(cor(st[,2:10]),3)
corr
plot(st[,2:10])

df = as.data.frame(corr)
income_vec = df[2,]
strong_pos = max(income_vec[income_vec != max(income_vec)])
strong_pos
strong_neg = min(income_vec)
strong_neg

#Q2--------------------------------------
# 1. Income is negatively associated with Illiteracy:
#   Negative corelation is strong thus increase in illiteracy will have an reduce income.
# 2. Income is Positively associated with HSGrad:
#   Positive Corelation is Strong thus increase in HSGrad will have an increase in income

#Q3--------------------------------------
m1 = lm(Income~Illiteracy + HSGrad, data = st)
summary(m1)
# 

#Q4-------------------------------------
m2 = lm(Income~Illiteracy + HSGrad + Density, data = st)
summary(m2)

#Q5-------------------------------------
new1=c(1.0,48,100)
new2=c(0.7,50,70)
new3=c(2.0,45,440)
new4=c(1.4,47.5,15)
new=rbind(new1,new2,new3,new4)
rownames(new)=c("state1","state2","state3","state4")
colnames(new)=c("Illiteracy","HSGrad","Density")
new=as.data.frame(new)

inc =predict(m2,new)
predicted_output = cbind(new,inc)
predicted_output




