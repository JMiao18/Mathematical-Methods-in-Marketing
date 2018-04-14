library(stats)
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
# 
# ## Multi-Nomial Logit Estimation
# par = rnorm(18)
# N = 1918
# 
# ll <- function(beta)
# {
#   res = 0
#   
#   M1 = as.matrix(train_set)[,1:18] %*% beta
#   M2 = as.matrix(train_set)[,19:36] %*% beta
#   M3 = as.matrix(train_set)[,37:54] %*% beta
#   
#   ## Each row of M is the probability for each individual to choose each alternative
#   M = cbind(exp(M1),exp(M2),exp(M3))
#   M = M / rowSums(M)
#   ## Construct the matrix cbind(seq(1,length(train_choice)),train_choice) to select the choice for each individual
#   MP = M[cbind(seq(1,length(train_choice)),train_choice)]
#   ## Maximize Likelihood ==> Maximize log-likelihood 
#   ## ==> Minimize minus log-likelihood
#   res = res + sum(-log(MP))
#   
#   return (res)
# }
# 
# ML = nlm(ll,par,hessian = TRUE)
# ## ML$estimate is the estimated value for each of the 18 parameters
# mode = ML$estimate
# SE = sqrt(diag(solve(ML$hessian)))
# Tvalue = mode/SE
# ll = 2*ML$minimum
# Result = cbind(Estimate = mode, SE= SE, Tvalue = Tvalue, minusll = ll)
# round(Result,2)
# ## The predicted relative utility for each of the three alternatives 
# X1_predict = as.matrix(X1_test)[,5:22] %*% ML$estimate 
# X2_predict = as.matrix(X2_test)[,5:22] %*% ML$estimate 
# X3_predict = as.matrix(X3_test)[,5:22] %*% ML$estimate 
# ## The prediction is the maximum utility among the three alternatives.
# prediction = max.col(cbind(X1_predict,X2_predict,X3_predict))
# 
# ## The performance of the model without segments is evaluated by the proportion of correct predictions. 
# performance_0 = sum(prediction == test_choice)/length(test_choice)

## The second stage: Latent Class Logit Model with Two Segments
## The parameters for the first segment
par1 = rnorm(18)
## The parameters for the second segment
par2 = rnorm(18)
## The parameters for the third segment
par3 = rnorm(18)
## The parameter for the likelihood that a randomly chosen individual belongs to segment 1
p3 = rnorm(3)
p3 = p3/sum(p3)
## The combined parameters are  
par_3 = t(cbind(t(par1),t(par2),t(par3),t(p3[1:2])))

N = 1918
ll_3 <- function(beta)
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
  ## The likelihood for each of the three alternatives in the first segment 
  M2 = M2 / rowSums(M2)

  M31 = as.matrix(train_set)[,1:18] %*% beta[37:54]
  M32 = as.matrix(train_set)[,19:36] %*% beta[37:54]
  M33 = as.matrix(train_set)[,37:54] %*% beta[37:54]
  M3 = cbind(exp(M31),exp(M32),exp(M33))
  ## The likelihood for each of the three alternatives in the first segment 
  M3 = M3 / rowSums(M3)
  
    ## Given that the consumer belongs to Segment 1, what is the probability for him to choose Alternative i?
  prob_1 = M1[cbind(seq(1,length(train_choice)),train_choice)]
    ## Given that the consumer belongs to Segment 2, what is the probability for him to choose Alternative i?
  prob_2 = M2[cbind(seq(1,length(train_choice)),train_choice)]
    ## Given that the consumer belongs to Segment 2, what is the probability for him to choose Alternative i?
  prob_3 = M3[cbind(seq(1,length(train_choice)),train_choice)]
  ## This transformation guarantees that the likelihood for each segment is within the range of (0,1)
  probability_1 = 1/(1 + exp(beta[55]))
  probability_2 = 1/(1 + exp(beta[56]))
  
  for (i in 1:137)
  {
    MP = c()
    MP[i] =   probability_1 * cumprod(prob_1[(14*(i-1)+1):(14*i)])[14] + 
              probability_2 * cumprod(prob_2[(14*(i-1)+1):(14*i)])[14] + 
      (1 - probability_2 - probability_1) * cumprod(prob_3[(14*(i-1)+1):(14*i)])[14]
    
    res = res - log(MP[i])
  }  
  return (res)
}

ML_3 = optim(par_3,ll_3,method = "Nelder-Mead", hessian = TRUE)
ML_3 = nlm(ll_3,par_3,hessian = TRUE)
mode = ML_3$estimate
SE = sqrt(diag(solve(ML_3$hessian)))
Tvalue = mode/SE
ll = 2*ML_3$minimum
Result_3 = cbind(Estimate = mode, SE= SE, Tvalue = Tvalue, minusll = ll)
round(Result_3,2)

##  This is the estimates for pi_i(the market share of the first segment)
pi_1 = 1/(1 + exp(ML_3$estimate[55]))
pi_2 = 1/(1 + exp(ML_3$estimate[56]))

# ## NExt, I am going to obtain the individual-level estimate of segment membership using Bayes rule.
# ## USing the first individual as an example
# M11 = as.matrix(train_set)[1:14,1:18] %*% ML_2$estimate[1:18]
# M12 = as.matrix(train_set)[1:14,19:36] %*% ML_2$estimate[1:18]
# M13 = as.matrix(train_set)[1:14,37:54] %*% ML_2$estimate[1:18]
# M1 = cbind(exp(M11),exp(M12),exp(M13))
# ## The likelihood for Indivual 1 to choose each of the three alternatives in the first segment 
# M1 = M1 / rowSums(M1)
# M11 = M1[cbind(1:14,train_choice[1:14])]
# 
# M21 = as.matrix(train_set)[1:14,1:18] %*% ML_2$estimate[19:36]
# M22 = as.matrix(train_set)[1:14,19:36] %*% ML_2$estimate[19:36]
# M23 = as.matrix(train_set)[1:14,37:54] %*% ML_2$estimate[19:36]
# M2 = cbind(exp(M21),exp(M22),exp(M23))
# ## The likelihood for each of the three alternatives in the first segment 
# M2 = M2 / rowSums(M2)
# M12 = M2[cbind(1:14,train_choice[1:14])]
# cumprod(M11)[14]/(cumprod(M11)[14]+cumprod(M12)[14])

## Concise Expression
## The likelihood for each of the three alternatives in the first segment 
M11 = as.matrix(train_set)[,1:18] %*% ML_2$estimate[1:18]
M12 = as.matrix(train_set)[,19:36] %*% ML_2$estimate[1:18]
M13 = as.matrix(train_set)[,37:54] %*% ML_2$estimate[1:18]
M1 = cbind(exp(M11),exp(M12),exp(M13))
M1 = M1 / rowSums(M1)
## M11 is the probability for individual i's actual choice in Segment 1
M11 = M1[cbind(1:(137*14),train_choice[1:(137*14)])]

## The likelihood for each of the three alternatives in the second segment 
M21 = as.matrix(train_set)[,1:18] %*% ML_2$estimate[19:36]
M22 = as.matrix(train_set)[,19:36] %*% ML_2$estimate[19:36]
M23 = as.matrix(train_set)[,37:54] %*% ML_2$estimate[19:36]
M2 = cbind(exp(M21),exp(M22),exp(M23))
M2 = M2 / rowSums(M2)
## M11 is the probability for individual i's actual choice in Segment 2
M12 = M2[cbind(1:(14*137),train_choice[1:(14*137)])]

## The likelihood for each of the three alternatives in the third segment 
M31 = as.matrix(train_set)[,1:18] %*% ML_2$estimate[37:54]
M32 = as.matrix(train_set)[,19:36] %*% ML_2$estimate[37:54]
M33 = as.matrix(train_set)[,37:54] %*% ML_2$estimate[37:54]
M3 = cbind(exp(M31),exp(M32),exp(M33))
M3 = M3 / rowSums(M3)
## M11 is the probability for individual i's actual choice in Segment 2
M13 = M3[cbind(1:(14*137),train_choice[1:(14*137)])]

## The individual-level estimates of segment membership using Bayes Rule
## What is the orobability for each individual to belong to Segment 1?
prob <- rep(0,137*2)
prob = matrix(prob, 137,2)
for (i in 1:137)
{
  prob[i,1] = (pi_1 * (cumprod(M11[(1+(i-1)*14):(14*i)])[14]))/(  (pi_1 * cumprod(M11[(1+(i-1)*14):(14+(i-1)*14)])[14]) + (pi_2 * cumprod(M12[(1+(i-1)*14):(14+(i-1)*14)])[14]) + ((1 - pi_2 - pi_1) * cumprod(M13[(1+(i-1)*14):(14+(i-1)*14)])[14]))
  
  prob[i,2] = (pi_2 * (cumprod(M12[(1+(i-1)*14):(14+(i-1)*14)])[14])) / ( (pi_1 * cumprod(M11[(1+(i-1)*14):(14+(i-1)*14)])[14]) + (pi_2 * cumprod(M12[(1+(i-1)*14):(14+(i-1)*14)])[14]) + ((1 - pi_2 - pi_1) * cumprod(M13[(1+(i-1)*14):(14+(i-1)*14)])[14]) )  
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


## Then I am going to do the Cross-Validation
##  The expected probability for each alternative in Segment 1
X1_predict_11 = as.matrix(X1_test)[,5:22] %*% ML_3$estimate[1:18] 
X2_predict_12 = as.matrix(X2_test)[,5:22] %*% ML_3$estimate[1:18] 
X3_predict_13 = as.matrix(X3_test)[,5:22] %*% ML_3$estimate[1:18]
##  The expected probability for each alternative in Segment 2
X1_predict_21 = as.matrix(X1_test)[,5:22] %*% ML_3$estimate[19:36] 
X2_predict_22 = as.matrix(X2_test)[,5:22] %*% ML_3$estimate[19:36] 
X3_predict_23 = as.matrix(X3_test)[,5:22] %*% ML_3$estimate[19:36]
##  The expected probability for each alternative in Segment 3
X1_predict_31 = as.matrix(X1_test)[,5:22] %*% ML_3$estimate[37:54] 
X2_predict_32 = as.matrix(X2_test)[,5:22] %*% ML_3$estimate[37:54] 
X3_predict_33 = as.matrix(X3_test)[,5:22] %*% ML_3$estimate[37:54]

predict_31 = cbind(exp(X1_predict_11),exp(X2_predict_12),exp(X3_predict_13))
predict_32 = cbind(exp(X1_predict_21),exp(X2_predict_22),exp(X3_predict_23))
predict_33 = cbind(exp(X1_predict_31),exp(X2_predict_32),exp(X3_predict_33))

## predict_21 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 1
predict_31 = predict_31/rowSums(predict_31)
## predict_22 is the predicted probability for each individual to choose each alternative given that he belongs to Segment 2
predict_32 = predict_32/rowSums(predict_32)
predict_33 = predict_33/rowSums(predict_33)

## The vector prob is the probability for each individual to choose Segment 1
## We can use the posterior segment membership probability to estimate the probability that individual i chooses alternative j
predict_3 = prob[,1] * predict_31 + prob[,2] * predict_32 + (1 - prob[,1] - prob[,2]) ^ predict_33
prediction_3 = max.col(predict_3)

## The performance of the model without segments is evaluated by the proportion of correct predictions. 
performance_3 = sum(prediction_3 == test_choice)/length(test_choice)
performance_3