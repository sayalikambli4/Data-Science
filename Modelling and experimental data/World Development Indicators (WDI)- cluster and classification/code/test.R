data ( "Smarket" , package = "ISLR" )

sample ( rnorm(25 , mean=0, sd=1) , 5)
 sample ( 1 : 10 , 10 , replace = TRUE)
 sample ( 1 : 10 , 10 , replace = FALSE)


index = sample ( 1 : nrow( data1 ) , 10 , replace = FALSE)
 subset = data1 [ index , ]
 subset

folds = cut ( seq ( 1 , nrow( data1 ) ) , breaks=10, labels=FALSE)

accuracy = vector(mode="numeric" , length=10)

for( i in 1 : 10 )
{
 ti <??? which ( folds==i , arr.ind=TRUE)
 dft = data1[ti, ]
 dftr <??? data1 [???ti , ]

 lda=lda(covid_deaths_labels ~ X+child_mort + income+ exports + health + imports + inflation +total_fer  + pop_dens , data=dftr )
 ldapredict <- predict(lda,dft[,-9])$class
 accuracy [i] = mean(ldapredict ==dft$covid_deaths_labels )

}
 mean( accuracy )
 