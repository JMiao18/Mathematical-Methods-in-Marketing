ll<-function(par)
{
  pi<-par[1:(s-1)]
  beta<-matrix(1,nrow =s,ncol=18)
  for (i in 1:s)
  {
    beta[i,]=par[(s+18*i-18):(s+18*i-1)]
  }
  ln=0
  for (i in 1:137)
  {
    ind<-ipad_test[c((42*i-41):(42*i)),]
    pr=0
    for (k in 1:s)
    {
      pro=1
      for (j in 1:14)
      {
        U<-c()
        U[1]<-as.matrix(ind[(3*j-2),5:22])%*%(beta[k,])
        U[2]<-as.matrix(ind[(3*j-1),5:22])%*%(beta[k,])
        U[3]<-as.matrix(ind[(3*j),5:22])%*%(beta[k,])
        prob<-exp(U[(ind[(3*j),4])])/sum(exp(U))
        pro=pro*prob
      }
      if (k==s)
      {
        pr=pr+pro*(1-sum(pi))
      }
      else
      {
        pr=pr+pro*pi[k]
      }
    }
    ln=ln-log(pr)
  }
  
  ret=(-2)*ln-19*s*log(14*137)
  return(ret)
}

pi<-rep(1/s,s-1)
b<-rnorm(18*s)