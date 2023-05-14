

library(dplyr)
library(ISLR)
library(MASS)
library("faraway")
library("nnet")
setwd('C:\\Users\\hp\\Desktop\\Masters\\Summer Term\\335- Modelling\\Final project\\code')
data<- read.csv("project_data.csv",header = TRUE)



summary(data$covid_deaths)
data1<-data %>% mutate(covid_deaths_labels=case_when(covid_deaths > 1900 ~ "Very High",
                                                     between(covid_deaths,900,1900) ~ "High",
                                                     between(covid_deaths,500,900)  ~ "Low",TRUE ~"Very Low"))

summary(as.factor(data1$covid_deaths_labels))
round(cor(data[,3:12]),2)


model2<-glm(covid_deaths_labels~.,data=data1[,4:12])
summary(model2)
step2<-step(model2,method="backward")

lda.fit=lda(covid_deaths_labels ~ child_mort+income+ exports + health + imports + inflation     +total_fer  + pop_dens , data=data1 )

qda.fit=qda(covid_deaths_labels ~ child_mort+income+ exports + health + imports + inflation     +total_fer  + pop_dens , data=data1 )

ldapredict <- predict(lda.fit)$class

qdapredict <- predict(qda.fit)$class


test <- multinom(covid_deaths_labels ~ child_mort+ income+ exports + health  + imports + inflation  +total_fer   + pop_dens,data=data1)
glmpredict <- predict(test)

mean(ldapredict==data1$covid_deaths_labels)
mean(qdapredict==data1$covid_deaths_labels)
mean(glmpredict==data1$covid_deaths_labels)


x1<-table(ldapredict , data1$covid_deaths_labels)
x2<-table(qdapredict , data1$covid_deaths_labels)
x3<-table(glmpredict , data1$covid_deaths_labels)


#k-fold cv validation with k=10
set.seed(123)

t <- trainControl(method = "cv", number = 10)
# Train the model
ld <- train(covid_deaths_labels ~ child_mort+income+ exports + health + imports + inflation+total_fer  + pop_dens, 
               data = data1, method = "lda",
               trControl = t)
ld$results$Accuracy
corrplot(cor(data1[,3:12]))
qd <- train(covid_deaths_labels ~ child_mort+income+ exports + health + imports + inflation+total_fer  + pop_dens, 
               data = data1, method = "qda",
               trControl = t)
qd$results$Accuracy


lg <- train(covid_deaths_labels ~child_mort+income+ exports + health + imports + inflation+total_fer  + pop_dens, 
               data = data1, method ="multinom",
               trControl = t)
lg$results$Accuracy
print(lg)

