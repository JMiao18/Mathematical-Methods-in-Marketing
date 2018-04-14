library("arm")
library("numDeriv")

NX = 2 #number of parameters per segment
NS = 2 #number of consumer segments
NJ = 2 #number of brands
NT = 10 #number of periods
NI = 300 #number of people

#Data File data2.CSV
# Col1= Customer ID
# Col2 = Time period
# Col3 = choice 1 if choice brand A and 2 if chose brand B
# Col4-5 = prices for brand A and brand B, respectively

#============================================================================
# Reading in data

data=matrix(scan("data2.txt"),5,NI*NT)
data=t(data)

# 1 segment1  
coef.vec = rep(1,3*NS)
seg.shr = rep(1,(NS-1))
coefs <- c(coef.vec,seg.shr)

log.lik <- function (coefs,data,ns,nj,nt,ni,nx)    #likelihood function
{  
  coef.vec = coefs[1:(3*ns)]
  seg.shr = c(0,coefs[-(1:(3*ns))])
  prob = matrix(0,ns,nj)
  variab = matrix(0,ns,nj)
  total = 0
  total_ind = 1
  total_s = 0
  
  # calculating segment proportions - #shares
  denum = 0
  for(sh in seg.shr){
    denum = denum + exp(sh)
  }
  shares = rep(NA,ns)
  for(l in 1:ns){
    shares[l] = exp(seg.shr[l])/denum
  }
  
  for(i in 1:ni){
    total_ind = 1
    for(h in 1:ns){
      for(k in 1:nt){
        
        row = (i-1)*NT+k
        
        if (k != 1) { # i.e. if we are not dealing with the first element
          
          variab[h,1] = coef.vec[(3*h-2)] + coef.vec[(3*h-1)]*data[row,4] + coef.vec[(3*h)]*as.numeric(data[(row-1),3]==1) #the systematic utility for 1
          variab[h,2] = coef.vec[(3*h-1)]*data[row,5]  + coef.vec[(3*h)]*as.numeric(data[(row-1),3]==2)                #the systematic utility for 2
          
        } else {
          
          variab[h,1] = coef.vec[(3*h-2)] + coef.vec[(3*h-1)]*data[row,4] + coef.vec[(3*h)]*0 #the systematic utility for 1
          variab[h,2] = coef.vec[(3*h-1)]*data[row,5]       + coef.vec[(3*h)]*0             #the systematic utility for 2
          
        }
        
        prob[h,1] = exp(variab[h,1])/(exp(variab[h,1])+exp(variab[h,2]))                                # logit for 1
        prob[h,2] = exp(variab[h,2])/(exp(variab[h,1])+exp(variab[h,2]))                               # logit for 2
        
        choice = data[row,3]
        # multiplying probabilities across time within segment to get L
        total_ind = prob[h,choice]*total_ind
      }
      #weighing L's by segment weigh and adding up
      total_s = total_ind*shares[h] + total_s
      total_ind = 1
    }
    # deriving LL
    total = total + log(total_s)
    total_s = 0
  }
  return(-total)
}

# log.lik <- function (coef.vec,data,ns,nj,nt,ni,nx)    #likelihood function
# {  
#   prob = array(NA,nj)
#   
#   total = 0
#   for(i in 1:ni){
#     for(k in 1:nt)
#     {
#       row = (i-1)*NT+k
#       V1 = coef.vec[1]+coef.vec[2]*data[row,4] #the systematic utility for 1
#       V2 = coef.vec[2]*data[row,5]                    #the systematic utility for 2
#       
#       prob[1] = exp(V1)/(exp(V1)+exp(V2))                             # logit for 1
#       prob[2] = exp(V2)/(exp(V1)+exp(V2))                             # logit for 2
#       choice = data[row,3]
#       
#       total = total +log(prob[choice])        
#       
#     }
#   } 
#   return(-total)
# }

# optimization procedure to calculate the MLE estimates

mle <- nlm(log.lik,coefs,data=data,ns=NS,nj=NJ,nt=NT,ni=NI,nx=NX, hessian=TRUE) 

extr_est_and_prop <- function(mle,ns=NS){
  
  regr_fact = mle$estimate[1:(3*ns)]
  segm = c(0,mle$estimate[-(1:(3*ns))])
  
  denum = 0
  for(s in segm){
    denum = denum + exp(s)
  }
  
  segm_prop = rep(NA,ns)
  for(l in 1:ns){
    segm_prop[l] = exp(segm[l])/denum
  }
  regr_fact = matrix(regr_fact,ns,3,byrow=TRUE)
  matr = cbind(regr_fact,matrix(segm_prop,ns,1))
  colnames(matr)<-c("b1","b2","b3","proportion")
  matr <- data.frame(matr)
  return(matr)
}

# extracting coefficients and segment proportions

extr_est_and_prop(mle)

# calculating the Hessian to obtain stdev
mode = mle$estimate                              # output parameter estimates
SE = sqrt(diag(solve(mle$hessian)))     # output parameter SEs
Tvalue = mode/SE                                        # output parameter T-values
ll = 2*mle$minimum                              # -2*log-likelihood
np = length(coefs)                    # number of parameters
AIC = 2*(mle$minimum+np)                                 # calculates AIC
n = sum(NI*NT)                           # number of observations
BIC = 2*mle$minimum+np*log(n)            # calculates BIC

list(Estimate=mode,SE=SE,Tvalue=Tvalue,minus2ll=ll,AIC=AIC,BIC=BIC) 


#########################
result <- matrix(0,0,3)

for(NS in 1:5){
  coef.vec = rep(1,3*NS)
  seg.shr = rep(1,(NS-1))
  coefs <- c(coef.vec,seg.shr)
  
  mle <- nlm(log.lik,coefs,data=data,ns=NS,nj=NJ,nt=NT,ni=NI,nx=NX, hessian=TRUE)
  
  matr<-extr_est_and_prop(mle)
  
  mode = mle$estimate                            # output parameter estimates
  SE = sqrt(diag(solve(mle$hessian)))   # output parameter SEs
  Tvalue = mode/SE                                      # output parameter T-values
  ll = 2*mle$minimum                            # -2*log-likelihood
  np = length(coefs)                    # number of parameters
  AIC = 2*(mle$minimum+np)                               # calculates AIC
  n = sum(NI*NT)                           # number of observations
  BIC = 2*mle$minimum+np*log(n)            # calculates BIC
  
  result <- rbind(result,c(NS,AIC,BIC))
  colnames(result)<-c("Segment.Num","AIC","BIC")
}
result1<- result

result1
###########################
for(NS in 1:3){
  coef.vec = rep(1,3*NS)
  seg.shr = rep(1,(NS-1))
  coefs <- c(coef.vec,seg.shr)
  
  mle <- nlm(log.lik,coefs,data=data,ns=NS,nj=NJ,nt=NT,ni=NI,nx=NX, hessian=TRUE)
  
  matr<-extr_est_and_prop(mle)
  
  mode = mle$estimate                    # output parameter estimates
  SE = sqrt(diag(solve(mle$hessian)))   # output parameter SEs
  Tvalue = mode/SE                                      # output parameter T-values
  ll = 2*mle$minimum                            # -2*log-likelihood
  np = length(coefs)                    # number of parameters
  AIC = 2*(mle$minimum+np)                               # calculates AIC
  n = sum(NI*NT)                           # number of observations
  BIC = 2*mle$minimum+np*log(n)            # calculates BIC
  
  result <- rbind(result,c(NS,AIC,BIC))
  colnames(result)<-c("Segment.Num","AIC","BIC")
}

# # calculating the Hessian to obtain stdev
#  mode = mle$estimate                                   # output parameter estimates
#  SE = sqrt(diag(solve(mle$hessian)))  # output parameter SEs
#  Tvalue = mode/SE                                     # output parameter T-values
#  ll = 2*mle$minimum                           # -2*log-likelihood
#  np = length(coef.vec)                    # number of parameters
#  AIC = 2*(mle$minimum+np)                              # calculates AIC
#  n = sum(NI*NT)                           # number of observations
#  BIC = 2*mle$minimum+np*log(n)            # calculates BIC
# 
# list(Estimate=mode,SE=SE,Tvalue=Tvalue,minus2ll=ll,AIC=AIC,BIC=BIC) 

# to estimate the purchase probability for the brands based on their mean price.
V1 = mode[1]+mode[2]*mean(data[,4]) #the systematic utility for 1
V2 = mode[2]*mean(data[,5])
prob = array(NA,2)
prob[1] = exp(V1)/(exp(V1)+exp(V2))
prob[2] = exp(V2)/(exp(V1)+exp(V2))
