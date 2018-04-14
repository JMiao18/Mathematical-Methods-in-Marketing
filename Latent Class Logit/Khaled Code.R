

                  #####################
                  # DATA MANIPULATION #
                  #####################
setwd("C:/Users/kboughanmi18/Dropbox/Busienss School TAship/2017_fall_Rajeev_MathMethods/latent_class")
data <- read.csv(file = "data_ipad.csv")
rownames(data) <- c()

# data per individual
Data <- list()
for(i in 1:137){
  tmp  <- data[which(data$consumer_id == i),]
  alt  <- tmp$alternative_id_in_set
  y    <- tmp$choice[which(alt == 1)]
  tmp  <- tmp[,5:22]
  X1   <- as.matrix(tmp[which(alt == 1),])
  X2   <- as.matrix(tmp[which(alt == 2),])
  X3   <- as.matrix(tmp[which(alt == 3),])
  
  data_i <- list(X1 = X1, X2 = X2, X3 = X3, y = y)
  Data[[i]] <- data_i
}

                  #####################
                  #    ESTIMATION     #
                  #####################

# likelihood for an individual 
L_i <- function(beta, data_i){
  #the choice probabilities of all the profiles
  V1  <- exp(as.matrix(data_i$X1) %*% beta)
  V2  <- exp(as.matrix(data_i$X2) %*% beta)
  V3  <- exp(as.matrix(data_i$X3) %*% beta)
  num <- matrix(cbind(V1,V2,V3),15,3)
  den <- c(matrix(1/(V1+V2+V3), 15,1))
  P   <- num * den 
  
  #return only the likelihood of the chosen profiles
  return(prod(P[cbind(1:15,data_i$y)]))
  
}

# likelihood for all the data
LL <- function(par, S){
  #compute the segments weights
  alphas     <- exp(c(0,par[1:(S-1)]))
  sum_alphas <- sum(alphas)
  pis        <- alphas/sum_alphas
  
  if(S == 1) {
    pis        <- 1
  }
  
  #compute the segments preferences
  betas <- par[S:length(par)]
  Beta  <- matrix(betas, 18, S)
  
  res <- 0
  for(i in 1:137){
    #the likelihood of each consumer
    Lik_i  <- 0
    data_i <- Data[[i]]
    for(s in 1:S){
      #the likelihood of the consumer conditional on the segment
      li_s  <- L_i(beta = Beta[,s], data_i = data_i) 
      Lik_i <- Lik_i + pis[s] * li_s
    }
    res <- res - log(Lik_i)
  }
  return(res)
}

# generic function for any number of segments
Estimate_LC <- function(S){
  #compute the number of paramters needed
  num_par <- (18 * S) + (S - 1)
  par     <- rep(0,num_par)
  
  #rmaximizing the likelihood
  ML      <- nlm(f = LL, p = par, S = S)
  par     <- ML$estimate
  
  #compute the segments weights
  alphas     <- exp(c(0,par[1:(S-1)]))
  sum_alphas <- sum(alphas)
  pis        <- alphas/sum_alphas
  if(S == 1){pis <- 1}
  
  #compute the segments preferences
  betas <- par[S:length(par)]
  Beta  <- matrix(betas, 18, S)
  
  #return the results
  results <- list(pis     = pis, 
                  Beta    = Beta, 
                  num_par = num_par, 
                  LL      = ML$minimum,
                  BIC     = log(137 * 15) * num_par + 2 *  ML$minimum,
                  AIC     = 2 * num_par + 2 *  ML$minimum )
  
  print(c("Estimation done for segments:",S))
  return(results)
}

# Estimation for all possible C segments
C <- 10
Est <- list()
for(k in 1:C){
  Est[[k]] <- Estimate_LC(k)
}


par(mfrow=c(2,2))
#####################
#    COMPARISON     #
#####################
BIC <- c()
AIC <- c()
LL <- c()
for(s in 1:C){
  BIC[s] <- Est[[s]]$BIC
  AIC[s] <- Est[[s]]$AIC
  LL[s] <- Est[[s]]$LL
}

plot(BIC, type = 'b', col ='red', 
     ylim = c(min(c(AIC,BIC)),max(c(AIC,BIC))), 
     pch = 19, 
     ylab = 'AIC/BIC',
     xlab = 'Number of segments')

points(AIC, type = 'b', col ='blue', pch = 19)

legend("bottomleft", 
       legend = c("AIC", "BIC"), 
       col = c('blue','red'), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

plot(LL , type = 'b', 
     col ='blue',
     pch = 19, 
     ylab = "Log-likelihood",
     xlab = 'Number of segments')


##
# Prediction
#

psi <- function(Beta, pis, i){
  data_i <-Data[[i]]
  S <- dim(Beta)[2]
  num <- rep(0, S)
  for(s in 1:S){
    lis <- L_i(Beta[,s], data_i)
    num[s] <- lis * pis[s]
  }
  
  return(num/sum(num))
}

pred <- function(i){
  data_i <-Data[[i]]
  
  P <- matrix(0, 15, 3)
  for(s in 1:S){
    beta <-c(Beta[,s]) 
    #the choice probabilities of all the profiles
    V1  <- exp(as.matrix(data_i$X1) %*% beta)
    V2  <- exp(as.matrix(data_i$X2) %*% beta)
    V3  <- exp(as.matrix(data_i$X3) %*% beta)
    num <- matrix(cbind(V1,V2,V3),15,3)
    den <- c(matrix(1/(V1+V2+V3), 15,1))
    P   <- P + round(num * den * PSI[i,s] ,3)
  }
  
  return(mean(P[cbind(1:15, data_i$y)]))
}


PSI_S <- list()
P_bar <- c()
for(s in 1:C){
  
  Est1 <- Est[[s]]
  Beta <- Est1$Beta
  pis  <- Est1$pis
  
  
  S <- dim(Beta)[2]
  PSI <- matrix(0, 137, dim(Beta)[2])
  for(i in 1:137){
    PSI[i,] <- psi(Beta, pis, i)
  }
  
  PSI_S[[s]] <- PSI
  
  pbar <- 0
  for(i in 1:137){
    pbar <- pbar+ pred(i) / 137
  }
  
  P_bar[s] <- pbar
  
}


plot(P_bar, 
     type="b", 
     pch = 19, 
     col = 'red', 
     ylim = c(0.41,max(P_bar)+ 0.05), 
     ylab= "Hit rate",
     xlab = 'Number of segments')

for(k in 1:length(P_bar)){
  text(k, P_bar[k] + 0.02, round(P_bar[k],2)) 
}


# Entropy measurement
ENS <-c()
for(s in 1:C){
  alpha <- PSI_S[[s]]
  EN <- 0
  for(i in 1:dim(alpha)[1]){
    for(s in 1:dim(alpha)[2]){
      EN <- EN + alpha[i,s] * log(alpha[i,s])
    }
  }
  ENS[s] <- -EN
}


plot(ENS, 
     type ="b", 
     pch  = 19, 
     col  = 'red', 
     ylab = "Entropy",
     xlab = 'Number of segments')




