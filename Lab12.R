library(ggplot2)
View(mpg)

g1=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,color=class))
g2=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,size=class)) 
g3=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,alpha=class))
g4=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy,shape=class))
library(gridExtra)
grid.arrange(g1,g2,g3,g4,ncol=2)



g5=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+
  facet_wrap(~class,nrow=2)
g6=ggplot(data=mpg)+geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv~cyl)

grid.arrange(g5,g6,ncol=2)

#Additional geometrics
#method1
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  geom_smooth(mapping=aes(x=displ,y=hwy))
#method2
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth()

s0=ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method="loess")

s1=ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method="lm")

s2=ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method="lm", formula=y~poly(x,2))

s3=ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method="loess",span=1)
grid.arrange(s0,s1,s2,s3,ncol=2)

#Bar Charts
ggplot(data=diamonds)+geom_bar(aes(x=cut), stat="count")
ggplot(data=diamonds)+stat_count(aes(x=cut))

b1=ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity),position='stack')
b2=ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity),position="dodge")
b3=ggplot(data=diamonds)+geom_bar(mapping=aes(x=cut,fill=clarity),position="fill")
grid.arrange(b1,b2,b3,ncol=3)

n1=ggplot(data=diamonds)+
  geom_histogram(aes(x=price,fill=clarity),color="white",position="stack")
n2=ggplot(data=diamonds)+
  geom_area(aes(x=price,fill=clarity),color="white",stat="density",position="stack")
n3=ggplot(data=diamonds)+
  geom_area(aes(x=price,fill=clarity,color=clarity),stat="density",alpha=0.1,position="identity")
n4=ggplot(data=diamonds)+
  geom_freqpoly(aes(x=price,color=clarity),position="identity")

grid.arrange(n1,n2,n3,n4,ncol=2)

#Bar Chart for Summary Statistics
#get the summary statistics, mean and se
library(dplyr)
mpgsummary=mpg%>%
  group_by(manufacturer)%>%
  summarize(count=n(),avgcty=mean(cty,na.rm=TRUE),sdcty=sd(cty,na.rm=TRUE),
            avghwy=mean(hwy,na.rm=TRUE),sdhwy=sd(hwy,na.rm=TRUE))%>%
  mutate(secty=sdcty/sqrt(count),sehwy=sdhwy/sqrt(count))

c1=ggplot(mpgsummary,aes(x=manufacturer,y=avgcty))+geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=avgcty-secty,ymax=avgcty+secty),width=0.3)
c2=ggplot(mpgsummary,aes(x=manufacturer,y=avghwy))+geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=avghwy-sehwy,ymax=avghwy+sehwy),width=0.3)
grid.arrange(c1,c2,ncol=1)

#Table Join
library(tidyr)
left=mpgsummary%>%
  gather(`avgcty`,`avghwy`,key="varname",value="avgmpg")%>%
  mutate(roadtype=substr(varname,nchar(varname)-2,nchar(varname)))%>%
  select(manufacturer,count,roadtype,avgmpg)
right=mpgsummary%>%
  gather(`secty`,`sehwy`,key="varname",value="sempg")%>%
  mutate(roadtype=substr(varname,nchar(varname)-2,nchar(varname)))%>%
  select(manufacturer,roadtype,sempg)
# the position_dodge() function add extra parameter to "dodge" position
inner_join(left,right,by=c("manufacturer","roadtype"))%>%
  ggplot(aes(x=manufacturer,y=avgmpg,fill=roadtype))+
  geom_bar(position=position_dodge(),stat="identity")+
  geom_errorbar(aes(ymin=avgmpg-sempg,ymax=avgmpg+sempg),position=position_dodge(0.9),width=0.3)

ggplot(mpgsummary,aes(x=manufacturer,y=avgmpg,color=roadtype))+
  geom_errorbar(aes(ymin=avgmpg-sempg,ymax=avgmpg+sempg),position=position_dodge(0.1),width=0.3)+
  geom_point(position=position_dodge(0.1),size=3)+
  geom_line(aes(group=roadtype,linetype=roadtype),position=position_dodge(0.1))




#BoxPlot
d1=ggplot(data=mpg,mapping=aes(x=class,y=hwy))+
  stat_boxplot()
d2=ggplot(data=mpg,mapping=aes(x=class,y=hwy))+
  geom_boxplot()+coord_flip()
d3=ggplot(data=mpg,mapping=aes(x=class,y=hwy))+
  geom_violin()
d4=ggplot(data=mpg,mapping=aes(x=class,y=hwy))+
  geom_violin()+coord_flip()

grid.arrange(d1,d2,d3,d4,ncol=2)


#additional setting
e1=ggplot(mpg,aes(displ,hwy))+
  geom_point(aes(color=class))+
  geom_smooth(method="loess",se=FALSE)+
  labs(title="Fuel efficiency generally decreases with engine size",
       x="Engine displacement (L)",y="Highway fuel economy (mpg)",color="car type"
  )+
  xlim(1,7)+ylim(10,50)
e2=e1+scale_color_brewer(palette="Set1")
e3=e1+scale_color_brewer(palette="Paired")
e4=e1+scale_color_brewer(palette="Accent")
grid.arrange(e1,e2,e3,e4,ncol=2)

#Palette choice
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()


#Maps
#install.packages("maps")
library(maps)
usa=map_data("state")
View(usa)

ggplot(usa)+geom_polygon(aes(x=long,y=lat,group=group,fill=region),color="white")+
  coord_fixed(1.3)+
  guides(fill=FALSE,color=FALSE)

#States.x77 data
st=as.data.frame(state.x77)
View(st)
summary(st)

colnames(st)[4]="LifeExp"
colnames(st)[6]="HSGrad"
st$Density=st$Population*1000/st$Area
View(st)
st=cbind(state.name,st,state.division,state.region)
colnames(st)[1]="region"
colnames(st)[11]="division"
colnames(st)[12]="usregion"
usa$region=as.character(usa$region)
st$region=as.character(tolower(st$region))
usa=usa%>%left_join(st,by="region")

ggplot(usa)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=Density),color="white")+
  scale_fill_gradient(breaks=c(5,10,20,50,100,200,400),trans="log10")+
  coord_fixed(1.3)+guides(color=FALSE)

h1=ggplot(usa)+geom_polygon(aes(x=long,y=lat,group=group,fill=HSGrad),color="white")+
  coord_fixed(1.3)+
  guides(color=FALSE)

h2=ggplot(usa)+geom_polygon(aes(x=long,y=lat,group=group,fill=Income),color="white")+
  coord_fixed(1.3)+
  guides(color=FALSE)

h3=ggplot(usa)+geom_polygon(aes(x=long,y=lat,group=group,fill=LifeExp),color="white")+
  coord_fixed(1.3)+
  guides(color=FALSE)

h4=ggplot(usa)+geom_polygon(aes(x=long,y=lat,group=group,fill=Murder),color="white")+
  coord_fixed(1.3)+
  guides(color=FALSE)

grid.arrange(h1,h2,h3,h4,ncol=2)


#Q1---------------------------------------------------------------------------
q1 = ggplot(data=mpg)+geom_point(mapping=aes(x=cty,y=hwy,color=class), position="jitter") + 
  facet_wrap(~manufacturer,ncol=5)

q1

#Q2-----------------------------------------------------------------------------
q2 = ggplot(data=mpg,mapping=aes(x=reorder(manufacturer,hwy, median),y=hwy))+
  stat_boxplot()

q2

#Q3-------------------------------------------------------------------------
q3=ggplot(data=mpg)+geom_bar(mapping=aes(x=manufacturer,fill = trans), position="stack") + scale_color_brewer(palette="Paired")

q3

#Q4------------------------------------------------------------------------


q4 = ggplot(data=mpg,mapping=aes(x=cty,y=displ, color = drv))+
  geom_point(aes(shape=as.factor(year))) +
  geom_smooth(method="lm", formula=y~poly(x,2), se=FALSE) + labs(title = "Cty vs Displ")
#+ facet_wrap(~drv)


q4
