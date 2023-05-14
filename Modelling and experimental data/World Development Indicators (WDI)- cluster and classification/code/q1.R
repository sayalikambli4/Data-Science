setwd('C:\\Users\\hp\\Desktop\\Masters\\Summer Term\\335- Modelling\\Final project\\code')
data<- read.csv("project_data.csv",header = TRUE)
attach(data)
#1
summary(data[,3:14])
plot(covid_deaths,income)
