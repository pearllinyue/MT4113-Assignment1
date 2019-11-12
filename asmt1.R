#I confirm that the attached is my own work, except where clearly indicated in the text.

#Part 1
#create the function my.rnorm
#give the default to parameter mean=0, sd=1
my.rnorm<-function(n,mean=0,sd=1){
  #invalid message when input n is not numeric
  if(!is.numeric(n)){
    stop("invalid message")
  }
  #set result.rnorm as a numeric vector
  result.rnorm<-vector("numeric",n)
  i<-1
  #deliver result.rnorm when n is an even number
  if(n%%2==0){
    for(i in 1:n/2){
      A<-runif(1,0,1)
      B<-runif(1,0,1)
      X1<-sin(2*pi*A)*sqrt(-2*log(B))*sd+mean
      X2<-cos(2*pi*A)*sqrt(-2*log(B))*sd+mean
      result.rnorm[2*i-1]<-X1
      result.rnorm[2*i]<-X2
    }
  }
  #deliver result.rnorm when n is an odd number
  else{
    for(i in 1:n%/%2){
      A<-runif(1,0,1)
      B<-runif(1,0,1)
      X1<-sin(2*pi*A)*sqrt(-2*log(B))*sd+mean
      X2<-cos(2*pi*A)*sqrt(-2*log(B))*sd+mean
      result.rnorm[2*i-1]<-X1
      result.rnorm[2*i]<-X2
    }
    #generate the last value for odd n by a combination of the last pair
    A<-runif(1,0,1)
    B<-runif(1,0,1)
    X1<-sin(2*pi*A)*sqrt(-2*log(B))*sd+mean
    X2<-cos(2*pi*A)*sqrt(-2*log(B))*sd+mean
    #X1,X2~N(mu,square sigma)
    #X1-X2~N(0,2*square sigma)
    #(X1-X2)/square root~N(0,square sigma)
    #therefore, (X1-X2)/square root+mu~N(mu,square sigma)
    result.rnorm[n]<-(X1-X2)/sqrt(2)+mean
  }
  return(result.rnorm)
}

#in order to test, generate a vector of 1000 items with mean 1 and sd 2
#also calculate the mean and standard deviation to compare with 1 and 2 respectively
check.rnorm<-my.rnorm(1000,1,2)
mean(check.rnorm)
sd(check.rnorm)


#Part 2
#create the function my.rchisq
#give the default to parameter n, df
my.rchisq<-function(n,df=1){
  #invalid message when input n is not numeric
  if(!is.numeric(n)){
    stop("invalid message")
  }
  #set result.rchisq as a numeric vector
  result.rchisq<-vector("numeric",n)
  i<-1
  #calculate result.rchisq
  for(i in 1:n){
    result.rchisq[i]<-sum(my.rnorm(df)^2)
  }
  return(result.rchisq)
}

#in order to test, generate a vector of 1000 items with df 10
#also calculate the mean and variance to compare as variance should be twice as much as mean in chi-square distribution
check.rechisq<-my.rchisq(1000,10)
mean(check.rechisq)
var(check.rechisq)


#Part 3
#create the function my.rnorm
#give the default to parameter n, df
my.rt=function(n,df=1){
  #invalid message when input n is not numeric
  if(!is.numeric(n)){
    stop("invalid message")
  }
  #set result.rchisq as a numeric vector
  result.rt<-vector("numeric",n)
  i<-1
  for(i in 1:n){
    #Z~N(0,1)
    Z<-my.rnorm(1,0,1)
    #U~chi-square(df)
    U<-my.rchisq(1,df)
    #calculate result.rt
    result.rt[i]<-Z/sqrt(U/df)
  }
  return(result.rt)
}

#in order to test, generate a vector of 1000 items with df 10
#also calculate the mean and variance to compare as mean and variance should be 0 and df/(df-2) respectively in t distribution
check.rt<-my.rt(1000,10)
mean(check.rt)
var(check.rt)
10/(10-2)