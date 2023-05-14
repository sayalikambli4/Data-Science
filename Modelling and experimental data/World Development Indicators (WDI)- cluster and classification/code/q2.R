#1
summary(child_mort)

summary(exports)

summary(health)

summary(imports)


df$continent <- toupper(df$continent)
summary(total_fer)
summary(pop_dens)
ggplot(df,aes(y=covid_deaths,x=child_mort)) + geom_point(color="blue")+xlim(0,150)+ylim(0,3000)+ylab("Covid deaths")+xlab("child mortality rate")+theme_bw()
ggplot(df,aes(x=continent,y=covid_deaths)) + geom_point(color="red")+ylab("Covid deaths")+ylim(0,3500)+theme_classic()


ggplot(kdata,aes(y=covid,x=cluster)) + geom_point(color="blue")+ylim(0,3000)

kd$centers


setwd('C:\\Users\\hp\\Desktop\\Masters\\Summer Term\\335- Modelling\\Final project\\code')
df<- read.csv("project_data.csv",header = TRUE)
library(ggplot2)


#2 K-means
k=5;
kdata <-df[,c(3:12)]
set.seed(123)
kd <- kmeans(kdata,centers=K, nstart=20)

kdata$cluster <- kd$cluster
kdata$continent<-as.factor(df$continent)
kdata$covid<-df$covid_deaths
kc<- as.data.frame(kdata$cluster) 
ggplot(kdata,aes(x=cluster,fill=continent)) + geom_bar()
summary(as.factor(kdata$cluster))



df
plot(df[df$cluster == 1 ,1:2] , pch="x" )
plot(kd$centers[ 1:3 , ] , type="l" , col=" red " , ylab="" )
lines(kd$centers[ 2 , ] , type="l" , col=" green " , ylab="" )
library( ISLR)
data$z <- data
#hclu


hdata <- dist(df[,c(3:12)])
hd <- hclust(hdata, method="complete")

plot(hd,labels=data$continent,main="",xlab="complete_linkage",ylab="Level")
rect.hclust(hd, k=5, border = " blue " )
hc <- as.data.frame(cutree(hd, k=5) )

cbind(hc ,kc)
mean(hc==kc)

