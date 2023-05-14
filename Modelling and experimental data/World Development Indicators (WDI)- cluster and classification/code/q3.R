setwd('C:\\Users\\hp\\Desktop\\Masters\\Summer Term\\335- Modelling\\Final project\\code')
df<- read.csv("project_data.csv",header = TRUE)

#3 LG


mean(covid_deaths)

df<-df %>% mutate(covid_deaths_bin=ifelse(covid_deaths>654.798,1,0))
attach(df)
  summary(as.factor(df$covid_deaths_bin))
model=glm(covid_deaths_bin ~ X+child_mort + exports + health + imports + income + inflation + life_expec + total_fer + gdpp + pop_dens +X ,family = binomial,data=df)

model <- predict(lg , type="response" )

modelpredict = rep(0 ,148)
modelpredict[model>0.5]=1
mean(df$covid_deaths_bin==modelpredict)
table(covid_deaths_bin,modelpredict)



