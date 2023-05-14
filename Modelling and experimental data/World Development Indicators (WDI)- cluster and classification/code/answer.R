setwd('C:\\Users\\hp\\Desktop\\Masters\\Summer Term\\335- Modelling\\Final project\\code')
data<- read.csv("project_data.csv",header = TRUE)

#2 K-means
K=4;
kdata <- data[,c(3:12)];
kd <- kmeans(kdata ,centers=K, nstart=20)
kd$cluster
km <- as.data.frame(kd$cluster)


plot(kdata[kd$cluster == 1 ,1:3] , pch="x" )
plot(kd$centers[ 1 , ] , type="l" , col=" red " , ylab="" )
lines(kd$centers[ 2 , ] , type="l" , col=" green " , ylab="" )

#hclu

hdata <- dis(data[,c(3:13)])
hd <- hclust(hdata, method="complete")
plot(hd,labels=(as.character(data$country)),main="",xlab="complete-linkage",ylab="level")
rect.hclust(hd, k=4, border = " red " )
hclust <- as.data.frame(cutree(hd, k=4) )




#3 LG
attach(data)
lg=glm(covid_deaths ~ child_mort + exports + health + imports + income + inflation + life_expec + total_fer + gdpp + pop_dens ,data=data)
lg
lgpre <- predict(lg , type="response" )
lgpre2 = rep("Down" ,148)
lgpre2[ lgpre>0.5]="Up"