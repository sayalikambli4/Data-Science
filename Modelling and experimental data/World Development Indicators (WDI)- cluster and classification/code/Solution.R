#loading library
library(ggplot2)
library(dplyr)
library(MASS)
library(nnet)
library(caret)
library(corrplot)

#importing dataset
setwd('C:\\Users\\hp\\Desktop\\Masters\\Summer Term\\335- Modelling\\Final project\\code')
dataset<- read.csv("project_data.csv",header = TRUE)
#fixing uppercase error in continent
dataset$continent <- toupper(dataset$continent)
df<- dataset
#Question 1
#dimension of dataset
dim(df)
attach(df)
#finding summaries details of attributes
summary(covid_deaths)
summary(child_mort)
summary(exports)
summary(health)
summary(imports)
summary(total_fer)
summary(pop_dens)
summary(income)
summary(gdpp)
attach(df)
#first graph to show relation ship between covid deaths and continents
ggplot(df,aes(x=continent,y=covid_deaths)) + geom_point(color="red")+
  ylab("Covid deaths")+ylim(0,3500)+theme_classic()+ggtitle("Covid Deaths in Continents")

# graph to show relation ship between covid deaths and Total fertility
ggplot(df,aes(y=covid_deaths,x=total_fer)) + geom_point(color="blue")+xlim(1,7.5)+ylim(0,3000)+
  ylab("Covid deaths")+xlab("Total fertility rate")+theme_bw()+ggtitle("Covid Deaths with Total fertility rate")


#question2 
#taking attributes 3 to 12(10)
# K-means clustering with k=5

k=5;
kdata <-df[,c(3:12)]
names(kdata)
set.seed(123)
kd <- kmeans(kdata,centers=K, nstart=20)

kdata$cluster <- kd$cluster
kdata$continent<-as.factor(df$continent)
kdata$covid<-df$covid_deaths

kc<- as.data.frame(kdata$cluster)
#ploting graph of clusters and continent
ggplot(kdata,aes(x=cluster,fill=continent)) + geom_bar()+theme_classic()+ggtitle("Clusters and Continents")+
  ylab("Total Countries in cluster")
kd$size
#Agglomerative Clustering
hdata <- dist(df[,c(3:12)])
hd <- hclust(hdata, method="complete")
hc <- as.data.frame(cutree(hd, k=5) )

#binding both cluster values
cbind(hc ,kc)
#calculating accuracy value
mean(hc==kc)
#this is to get clusters centers
round(kd$centers,2)

#Question 3
#To convert into binary variable finding threshold
mean(covid_deaths)

#creating new binary variable
df<-df %>% mutate(covid_deaths_bin=ifelse(covid_deaths>654.798,1,0))
attach(df)

summary(as.factor(df$covid_deaths_bin))
#fitting logistic regression model with 11 attributes
model=glm(covid_deaths_bin ~ X + child_mort + exports + health + imports + income + inflation + life_expec + total_fer + gdpp + pop_dens ,family = binomial,data=df)

modelpredict <- predict(model, type="response" )

#for converting probabilities values into categorical
modelpredict1 = rep(0 ,148)
modelpredict1[modelpredict>0.5]=1
df$Covid_deaths<-covid_deaths
df$Prediction_probability<-modelpredict


#finding accuracy of model


mean(df$covid_deaths_bin==modelpredict1)
#confusion matrix
table(covid_deaths_bin,modelpredict1)
#graph
ggplot(df,aes(x=Covid_deaths,y=Prediction_probability))+geom_point(size=0.5,color="brown",alpha=0.5)+stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
  labs(title = "Covid deaths prediction", x = "Covid Deaths",y = "Probability of predicting covid deaths")
specificity(actual=covid_deaths_bin,predicted=modelpredict)
sensitivity(actual=covid_deaths_bin,predicted=modelpredict)
#Question 4

data<-dataset
#converting into labels for classification
data1<-data %>% mutate(covid_deaths_labels=case_when(covid_deaths > 1900 ~ "Very High",
                                                     between(covid_deaths,1000,1900) ~ "High",
                                                     between(covid_deaths,500,1000)  ~ "Low",TRUE ~"Very Low"))

summary(as.factor(data1$covid_deaths_labels))
#finding correlation
corrplot(cor(data[,3:12]))

#LDA
set.seed(123)
lda=lda(covid_deaths_labels ~ X + child_mort + income+ exports + health + imports + inflation +total_fer  + pop_dens , data=data1 )

ldapredict <- predict(lda)$class
mean(ldapredict==data1$covid_deaths_labels)
#QDA
qda <- qda(covid_deaths_labels ~ X + child_mort + income+ exports + health + imports + inflation +total_fer  + pop_dens , data=data1 )
qdapredict <- predict(qda)$class

#Logistic Regression for multi-classification
test <- multinom(covid_deaths_labels ~ X + child_mort + income + exports + health  + imports + inflation + total_fer + pop_dens,data=data1)
glmpredict <- predict(test)


#Finding accuracy for models
mean(ldapredict==data1$covid_deaths_labels)
mean(qdapredict==data1$covid_deaths_labels)
mean(glmpredict==data1$covid_deaths_labels)

#confusion matrix
x1<-table(predicted =ldapredict ,actual= data1$covid_deaths_labels)
x2<-table(qdapredict , data1$covid_deaths_labels)
x3<-table(glmpredict , data1$covid_deaths_labels)

x1;x2;x3


#k-fold cv validation with k=10
set.seed(123)
t <- trainControl(method = "cv", number  = 5)

#for LDA
ld <- train(covid_deaths_labels ~ X+child_mort + income+ exports + health + imports + inflation + total_fer + pop_dens, 
            data = data1, method = "lda",trControl = t)
ld$results$Accuracy

#for QDA
qd <- train(covid_deaths_labels ~ X+child_mort + income+ exports + health + imports + inflation + total_fer + pop_dens, 
            data = data1, method = "qda",trControl = t)
qd$results$Accuracy

#for Logistic regression
lg <- train(covid_deaths_labels ~X+ child_mort + income+ exports + health + imports + inflation + total_fer + pop_dens, 
            data = data1, method ="multinom",trControl = t)
lg$results$Accuracy

