result = read.csv(file = "M:/A Master of Science in Marketing Sciences/Mathematical Models in Marketing (Kohli)/Latent Class Logit/Mathematical Models in Marketing (Kohli)/latent/results.csv",header = TRUE)

attach(result)

summary(result)
plot(number.of.classe[1:5],AIC[1:5],type = "b",xlim = c(0,6),ylim=c(2500,4000)
     ,xlab = "Number of Classes", ylab = "AIC", main = "The relationship between Number of Classes and AIC")

plot(number.of.classe[1:5],BIC[1:5],type="b",xlim = c(0,6),ylim=c(1500,4000)
     ,xlab = "Number of Classes", ylab = "BIC", main = "The relationship between Number of Classes and BIC")

plot(number.of.classe[1:5],rate.of.correct.predictions[1:5],type = "b",xlim = c(0,6),ylim=c(0.4,0.8) ,xlab = "Number of Classes", ylab = "Proportion of Correct Predictions", main = "The relationship between Number of Classes and Predictability")

