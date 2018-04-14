data = read.csv(file = "M:/A Master of Science in Marketing Sciences/Mathematical Models in Marketing (Kohli)/latent/data_ipad.csv",head = TRUE)
attach(data)
summary(data)

## Modelling Step 1: Set up Training Set and Dev Set
random_factor = sample(1:15,137,replace = TRUE)
determinant_factor = (1:137)*15 - 15
## The index for Dev Set is the summation of Random and Deterministic.
index = random_factor + determinant_factor
dev = data[which(choice_set_id  %in%  index) ,]
train = data[which(!(choice_set_id  %in%  index)) ,]

## Model 1: Fit an aggregate model without any segment
## In the training set,
X1_train = subset(train, alternative_id_in_set == 1)
X2_train = subset(train, alternative_id_in_set == 2)
X3_train = subset(train, alternative_id_in_set == 3)
## In the test set,``
X1_test = subset(dev, alternative_id_in_set == 1)
X2_test = subset(dev, alternative_id_in_set == 2)
X3_test = subset(dev, alternative_id_in_set == 3)
## The dependent variable
train_choice = X1_train$choice
test_choice = X1_test$choice

train_set = cbind(X1_train[,5:22],X2_train[,5:22],X3_train[,5:22])
test_set = cbind(X1_test[,5:22],X2_test[,5:22],X3_test[,5:22])

## The parameters for the first segment
par1 = rnorm(18)
## The parameters for the second segment
par2 = rnorm(18)
## The parameters for the third segment
par3 = rnorm(18)
## The parameters for the fourth segment
par4 = rnorm(18)
## The parameters for the fifth segment
par5 = rnorm(18)
## The parameter for the likelihood that a randomly chosen individual belongs to segment 1
p5 = rnorm(5)
## p5 = p5/sum(p5)
## The combined parameters are  
par_5 = t(cbind(t(par1),t(par2),t(par3),t(par4),t(par5),t(p5[1:4])))

N = 1918

ll_5 <- function(beta)
{
  res = 0
  ## To calculate the likelohood for the first segment
  M11 = as.matrix(train_set)[,1:18] %*% beta[1:18]
  M12 = as.matrix(train_set)[,19:36] %*% beta[1:18]
  M13 = as.matrix(train_set)[,37:54] %*% beta[1:18]
  M1 = cbind(exp(M11),exp(M12),exp(M13))
  ## The likelihood for each of the three alternatives in the first segment 
  M1 = M1 / rowSums(M1)
  
  M21 = as.matrix(train_set)[,1:18] %*% beta[19:36]
  M22 = as.matrix(train_set)[,19:36] %*% beta[19:36]
  M23 = as.matrix(train_set)[,37:54] %*% beta[19:36]
  M2 = cbind(exp(M21),exp(M22),exp(M23))
  ## The likelihood for each of the three alternatives in the second segment 
  M2 = M2 / rowSums(M2)
  
  M31 = as.matrix(train_set)[,1:18] %*% beta[37:54]
  M32 = as.matrix(train_set)[,19:36] %*% beta[37:54]
  M33 = as.matrix(train_set)[,37:54] %*% beta[37:54]
  M3 = cbind(exp(M31),exp(M32),exp(M33))
  ## The likelihood for each of the three alternatives in the third segment 
  M3 = M3 / rowSums(M3)
  
  M41 = as.matrix(train_set)[,1:18] %*% beta[55:72]
  M42 = as.matrix(train_set)[,19:36] %*% beta[55:72]
  M43 = as.matrix(train_set)[,37:54] %*% beta[55:72]
  M4 = cbind(exp(M41),exp(M42),exp(M43))
  ## The likelihood for each of the three alternatives in the fourth segment 
  M4 = M4 / rowSums(M4)
  
  M51 = as.matrix(train_set)[,1:18] %*% beta[73:90]
  M52 = as.matrix(train_set)[,19:36] %*% beta[73:90]
  M53 = as.matrix(train_set)[,37:54] %*% beta[73:90]
  M5 = cbind(exp(M51),exp(M52),exp(M53))
  ## The likelihood for each of the three alternatives in the fourth segment 
  M5 = M5 / rowSums(M5)
  
  ## Given that the consumer belongs to Segment 1, what is the probability for him to choose Alternative i?
  prob_1 = M1[cbind(seq(1,length(train_choice)),train_choice)]
  ## Given that the consumer belongs to Segment 2, what is the probability for him to choose Alternative i?
  prob_2 = M2[cbind(seq(1,length(train_choice)),train_choice)]
  ## Given that the consumer belongs to Segment 3, what is the probability for him to choose Alternative i?
  prob_3 = M3[cbind(seq(1,length(train_choice)),train_choice)]
  ## Given that the consumer belongs to Segment 4, what is the probability for him to choose Alternative i?
  prob_4 = M4[cbind(seq(1,length(train_choice)),train_choice)]
  ## Given that the consumer belongs to Segment 5, what is the probability for him to choose Alternative i?
  prob_5 = M5[cbind(seq(1,length(train_choice)),train_choice)]
  ## This transformation guarantees that the likelihood for each segment is within the range of (0,1)
  probability_1 = exp(beta[91])/(1+exp(beta[91])+exp(beta[92])+exp(beta[93])+exp(beta[94]))
  probability_2 = exp(beta[92])/(1+exp(beta[91])+exp(beta[92])+exp(beta[93])+exp(beta[94]))    
  probability_3 = exp(beta[93])/(1+exp(beta[91])+exp(beta[92])+exp(beta[93])+exp(beta[94]))
  probability_4 = exp(beta[94])/(1+exp(beta[91])+exp(beta[92])+exp(beta[93])+exp(beta[94]))
  
  for (i in 1:137)
  {
    MP = c()
    MP[i] =  probability_1 * cumprod(prob_1[(14*(i-1)+1):(14*i)])[14] + probability_2 * cumprod(prob_2[(14*(i-1)+1):(14*i)])[14] + probability_3 * cumprod(prob_3[(14*(i-1)+1):(14*i)])[14] + probability_4 * cumprod(prob_4[(14*(i-1)+1):(14*i)])[14] + (1 - probability_1 - probability_2 - probability_3 - probability_4) * cumprod(prob_4[(14*(i-1)+1):(14*i)])[14]
    
    res = res -log(MP[i])
  }  
  
  return (res)
}

ML_5 = nlm(ll_5,par_5,hessian = TRUE)
mode = ML_5$estimate
SE = sqrt(diag(solve(ML_5$hessian)))
Tvalue = mode/SE
ll = 2*ML_5$minimum
Result_5 = cbind(Estimate = mode, SE= SE, Tvalue = Tvalue, minusll = ll)
round(Result_5,2)

##  This is the estimates for pi_i(the market share of the first segment)
pi_1 = exp(ML_5$estimate[91])/(1+exp(ML_5$estimate[91])+exp(ML_5$estimate[92])+exp(ML_5$estimate[93])+exp(ML_5$estimate[94]))
pi_2 = exp(ML_5$estimate[92])/(1+exp(ML_5$estimate[91])+exp(ML_5$estimate[92])+exp(ML_5$estimate[93])+exp(ML_5$estimate[94]))
pi_3 = exp(ML_5$estimate[93])/(1+exp(ML_5$estimate[91])+exp(ML_5$estimate[92])+exp(ML_5$estimate[93])+exp(ML_5$estimate[94]))
pi_4 = exp(ML_5$estimate[94])/(1+exp(ML_5$estimate[91])+exp(ML_5$estimate[92])+exp(ML_5$estimate[93])+exp(ML_5$estimate[94]))

## Concise Expression
## The likelihood for each of the three alternatives in the first segment 
M11 = as.matrix(train_set)[,1:18] %*% ML_5$estimate[1:18]
M12 = as.matrix(train_set)[,19:36] %*% ML_5$estimate[1:18]
M13 = as.matrix(train_set)[,37:54] %*% ML_5$estimate[1:18]
M1 = cbind(exp(M11),exp(M12),exp(M13))
M1 = M1 / rowSums(M1)
## M11 is the probability for individual i's actual choice in Segment 1
M11 = M1[cbind(1:(137*14),train_choice[1:(137*14)])]

## The likelihood for each of the three alternatives in the second segment 
M21 = as.matrix(train_set)[,1:18] %*% ML_5$estimate[19:36]
M22 = as.matrix(train_set)[,19:36] %*% ML_5$estimate[19:36]
M23 = as.matrix(train_set)[,37:54] %*% ML_5$estimate[19:36]
M2 = cbind(exp(M21),exp(M22),exp(M23))
M2 = M2 / rowSums(M2)
## M11 is the probability for individual i's actual choice in Segment 2
M12 = M2[cbind(1:(14*137),train_choice[1:(14*137)])]

## The likelihood for each of the three alternatives in the third segment 
M31 = as.matrix(train_set)[,1:18] %*% ML_5$estimate[37:54]
M32 = as.matrix(train_set)[,19:36] %*% ML_5$estimate[37:54]
M33 = as.matrix(train_set)[,37:54] %*% ML_5$estimate[37:54]
M3 = cbind(exp(M31),exp(M32),exp(M33))
M3 = M3 / rowSums(M3)
## M11 is the probability for individual i's actual choice in Segment 2
M13 = M3[cbind(1:(14*137),train_choice[1:(14*137)])]

## The likelihood for each of the three alternatives in the fourth segment 
M41 = as.matrix(train_set)[,1:18] %*% ML_5$estimate[55:72]
M42 = as.matrix(train_set)[,19:36] %*% ML_5$estimate[55:72]
M43 = as.matrix(train_set)[,37:54] %*% ML_5$estimate[55:72]
M4 = cbind(exp(M41),exp(M42),exp(M43))
M4 = M4 / rowSums(M4)
## M11 is the probability for individual i's actual choice in Segment 2
M14 = M4[cbind(1:(14*137),train_choice[1:(14*137)])]

## The likelihood for each of the three alternatives in the fourth segment 
M51 = as.matrix(train_set)[,1:18] %*% ML_5$estimate[55:72]
M52 = as.matrix(train_set)[,19:36] %*% ML_5$estimate[55:72]
M53 = as.matrix(train_set)[,37:54] %*% ML_5$estimate[55:72]
M5 = cbind(exp(M51),exp(M52),exp(M53))
M5 = M5 / rowSums(M5)
## M11 is the probability for individual i's actual choice in Segment 2
M15 = M5[cbind(1:(14*137),train_choice[1:(14*137)])]

## The individual-level estimates of segment membership using Bayes Rule
## What is the orobability for each individual to belong to Segment 1?
prob <- rep(0,137*4)
prob = matrix(prob, 137,4)
for (i in 1:137)
{
  prob[i,1] = (pi_1 * (cumprod(M11[(1+(i-1)*14):(14 * i)])[14]))/((pi_1 * cumprod(M11[(1+(i-1)*14):(14 * i)])[14]) + (pi_2 * cumprod(M12[(1+(i-1)*14):(14 * i)])[14]) + (pi_3 * cumprod(M13[(1+(i-1)*14):(14 * i)])[14]) + (pi_4 * cumprod(M14[(1+(i-1)*14):(14 * i)])[14]) + ((1 - pi_2 - pi_1 - pi_3 - pi_4) * cumprod(M15[(1+(i-1)*14):(14 * i)])[14]))
  
  prob[i,2] = (pi_2 * (cumprod(M12[(1+(i-1)*14):(14 * i)])[14]))/((pi_1 * cumprod(M11[(1+(i-1)*14):(14 * i)])[14]) + (pi_2 * cumprod(M12[(1+(i-1)*14):(14 * i)])[14]) + (pi_3 * cumprod(M13[(1+(i-1)*14):(14 * i)])[14]) + (pi_4 * cumprod(M14[(1+(i-1)*14):(14 * i)])[14]) + ((1 - pi_2 - pi_1 - pi_3 - pi_4) * cumprod(M15[(1+(i-1)*14):(14 * i)])[14]))
  
  prob[i,3] = (pi_3 * (cumprod(M13[(1+(i-1)*14):(14 * i)])[14]))/((pi_1 * cumprod(M11[(1+(i-1)*14):(14 * i)])[14]) + (pi_2 * cumprod(M12[(1+(i-1)*14):(14 * i)])[14]) + (pi_3 * cumprod(M13[(1+(i-1)*14):(14 * i)])[14]) + (pi_4 * cumprod(M14[(1+(i-1)*14):(14 * i)])[14]) + ((1 - pi_2 - pi_1 - pi_3 - pi_4) * cumprod(M15[(1+(i-1)*14):(14 * i)])[14]))
  
  prob[i,4] = (pi_4 * (cumprod(M14[(1+(i-1)*14):(14 * i)])[14]))/((pi_1 * cumprod(M11[(1+(i-1)*14):(14 * i)])[14]) + (pi_2 * cumprod(M12[(1+(i-1)*14):(14 * i)])[14]) + (pi_3 * cumprod(M13[(1+(i-1)*14):(14 * i)])[14]) + (pi_4 * cumprod(M14[(1+(i-1)*14):(14 * i)])[14]) + ((1 - pi_2 - pi_1 - pi_3 - pi_4) * cumprod(M15[(1+(i-1)*14):(14 * i)])[14]))
}
## The vector prob is the probability for each individual to choose Segment 1

##########################################
# ## Another Method which is theoretical correct but practically infeasible
# cumprod(M11[(1+(2-1)*14):(14+(2-1)*14)])[14]
# cum_M11 = cumprod(M11)
# cum_M12 = cumprod(M12)
# cumprod(M11)[14]/(cumprod(M11)[14]+cumprod(M12)[14])
# 
# a <- 1:(137*14)
# b_11 <- cum_M11[seq(14, 137*14, 14)]
# b_11_tmp <- t(cbind(1,t(b_11)))
# b_11_tem = b_11_tmp[1:137]
# b_11/b_11_tem
##########################################

## ***********************************************  
## Then I am going to do the Cross-Validation
##  The expected probability for each alternative in Segment 1
## Segment 1 -- Choice 1
X1_predict_11 = as.matrix(X1_test)[,5:22] %*% ML_5$estimate[1:18] 
## Segment 1 -- Choice 2
X2_predict_12 = as.matrix(X2_test)[,5:22] %*% ML_5$estimate[1:18] 
## Segment 1 -- Choice 3
X3_predict_13 = as.matrix(X3_test)[,5:22] %*% ML_5$estimate[1:18]

##  The expected probability for each alternative in Segment 2
## Segment 2 -- Choice 1
X1_predict_21 = as.matrix(X1_test)[,5:22] %*% ML_5$estimate[19:36] 
## Segment 2 -- Choice 2
X2_predict_22 = as.matrix(X2_test)[,5:22] %*% ML_5$estimate[19:36] 
## Segment 2 -- Choice 3
X3_predict_23 = as.matrix(X3_test)[,5:22] %*% ML_5$estimate[19:36]
##  The expected probability for each alternative in Segment 3
## Segment 3 -- Choice 1
X1_predict_31 = as.matrix(X1_test)[,5:22] %*% ML_5$estimate[37:54] 
## Segment 3 -- Choice 2
X2_predict_32 = as.matrix(X2_test)[,5:22] %*% ML_5$estimate[37:54] 
## Segment 3 -- Choice 3
X3_predict_33 = as.matrix(X3_test)[,5:22] %*% ML_5$estimate[37:54]
##  The expected probability for each alternative in Segment 4
## Segment 4 -- Choice 1
X1_predict_41 = as.matrix(X1_test)[,5:22] %*% ML_5$estimate[55:72] 
## Segment 4 -- Choice 2
X2_predict_42 = as.matrix(X2_test)[,5:22] %*% ML_5$estimate[55:72] 
## Segment 4 -- Choice 3
X3_predict_43 = as.matrix(X3_test)[,5:22] %*% ML_5$estimate[55:72]
##  The expected probability for each alternative in Segment 5
## Segment 5 -- Choice 1
X1_predict_51 = as.matrix(X1_test)[,5:22] %*% ML_5$estimate[73:90] 
## Segment 5 -- Choice 2
X2_predict_52 = as.matrix(X2_test)[,5:22] %*% ML_5$estimate[73:90] 
## Segment 5 -- Choice 3
X3_predict_53 = as.matrix(X3_test)[,5:22] %*% ML_5$estimate[73:90]

predict_51 = cbind(exp(X1_predict_11),exp(X2_predict_12),exp(X3_predict_13))
predict_52 = cbind(exp(X1_predict_21),exp(X2_predict_22),exp(X3_predict_23))
predict_53 = cbind(exp(X1_predict_31),exp(X2_predict_32),exp(X3_predict_33))
predict_54 = cbind(exp(X1_predict_41),exp(X2_predict_42),exp(X3_predict_43))
predict_55 = cbind(exp(X1_predict_51),exp(X2_predict_52),exp(X3_predict_53))

## predict_51 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 1
predict_51 = predict_51/rowSums(predict_51)
## predict_52 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 2
predict_52 = predict_52/rowSums(predict_52)
## predict_53 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 3
predict_53 = predict_53/rowSums(predict_53)
## predict_54 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 4
predict_54 = predict_54/rowSums(predict_54)
## predict_55 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 5
predict_55 = predict_54/rowSums(predict_55)

## The vector prob is the probability for each individual to choose Segment 1
## We can use the posterior segment membership probability to estimate the probability that individual i chooses alternative j
predict_5 = prob[,1] * predict_51 + prob[,2] * predict_52 + prob[,3] * predict_53 + prob[,4] * predict_54 + (1 - prob[,1] - prob[,2] - prob[,3] - prob[,4]) * predict_55
prediction_5 = max.col(predict_5)

## The performance of the model without segments is evaluated by the proportion of correct predictions. 
performance_5 = sum(prediction_5 == test_choice)/length(test_choice)
performance_5
Result_5
