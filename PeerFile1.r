# I confirm that the attached is my own work, except where clearly indicated in the text.


inputCheck <- function(input, int = FALSE, nonNeg = FALSE){
  if (!is.numeric(input) | length(input) !=1) {
    stop("invalid arguments")
  }
  if (int == TRUE & input != round(input)){
    stop("invalid arguments")
  }
  if (nonNeg == TRUE & input < 0){
    stop("invalid arguments")
  }
}


my.rnorm <- function(n, mean = 0, sd = 1){
  #Purpose: random generation of n deviates from the normal distribution with mean equal to
  #   mean and standard deviation equal to sd.
  
  #Inputs:
  # n - a non-negative integer scalar
  # mean - the mean of the normal distribution from which the random samples are taken from
  # sd - the standard deviation of the normal distribution from which the random samples are
  #   taken from
  
  #Outputs: a vector of length n containing the randomly generated normal deviates

  #Input tests
  inputCheck(n,TRUE,TRUE)
  inputCheck(mean,FALSE,FALSE)
  inputCheck(sd,FALSE,TRUE)
  
  
  if (n%%2==0) {size <- n/2}
  #if n is odd generate an extra pair with the second number to be discarded later
  else {size <- (n/2 + 1)}


  A <- runif(n = size)
  B <- runif(n = size)
  X1 <- sin(2*pi*A) * sqrt(-2*log(B))
  X2 <- cos(2*pi*A) * sqrt(-2*log(B))
  #combine the pairs of variates into a single vector

  X3 <- c(X1,X2)
  #remove last number in vector if n is odd
  X3 <- X3[1:n]
  #transform to the correct mean and standard deviation 
  X3 <- X3*sd + mean
  return(X3)
}


my.rchisq <- function(n, df=1){
  #Purpose: random generation of n deviates from the chi-squared distribution with degrees of
  #   freedom equal to df
  
  #Inputs:
  # n - a non-negative integer scalar of the number of deviates to generate
  # df - a non-negative integer of the degrees of freedom of the generated chi-squared deviates
  
  #Outputs: a vector of length n containing deviates randomly generated from the chi-squared 
  #distribution

  inputCheck(n,TRUE,TRUE)
  inputCheck(df,TRUE,TRUE)
  
  #since df deviates from the normal distribution are required for each chi-squared number 
  #generated, n*df numbers are generated overall
  size <- n*df

  normVec <- my.rnorm(size)
  normVec2 <- normVec*normVec
  #a sum is applied over the squared normal deviates in groups of size df

  chisq <- tapply(normVec2,rep(1:n, each=df), sum)
  return(chisq)
}


my.rt <- function(n,df=1){
  #Purpose: random generation of n deviates from the student's t-distribution with degrees of
  #   freedom equal to df
  
  #Inputs:
  # n - a non-negative integer scalar of the number of deviates to generate
  # df - a non-negative integer of the degrees of freedom of the generated deviates from the
  #t distribution
  
  #Outputs: a vector of length n containing deviates randomly generated from the t distribution

  inputCheck(n,TRUE,TRUE)
  inputCheck(df,TRUE,TRUE)
 
  #create length n vectors of both randomly generated normal deviates and chi-squared deviates
  N <- my.rnorm(n)
  U <- my.rchisq(n,df)
  #use vector division to generate length n vector of randomly generated t distribution deviates
  my.T <- N/sqrt(U/df)
  return(my.T)
}


my.rnorm.test <- function(){
  #Purpose: Testing the my.rnorm funtion
  #Input: none
  #output: a plot, as well as errors if the function fails the tests
  #notes: produces 3 plots and changes the graphical parameter to show a row of 3 plots
  
  par(mfrow = c(1,3))
  
  y <- my.rnorm(100)
  #should produce a strait line
  qqnorm(y); qqline(y, col = 2)
  
  #produces comparative histograms to manually check accuracy
  hist(rnorm(2000))
  hist(my.rnorm(2000))
  
  #check size of output matched the size n specified
  test.lengths <- c(1:10, 55,100)
  for(i in test.lengths){
    if(length(my.rnorm(i)) != i){
      stop("output size and input n specified to not match")
    }
  }

 
  #checks the correct errors are presented for incorrect inputs
  my.error <- try(my.rnorm(-1),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject negative n")}
  my.error <- try(my.rnorm(1,0,-1),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject negative sd")}
  my.error <- try(my.rnorm(0.5),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject non-integer n")}
  my.error <- try(my.rnorm(4,c(2,4,6,8),sd=0.5),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject vector of means")}
  

  test.means <- c(-50, -1.5, -2:2, 1.5, 50)
  test.sd <- c(1, 2.5, 5)
  for(M in test.means){
    for(SD in test.sd){
      #generate samples
      y <- my.rnorm(1000,M,SD)
      x <- rnorm(1000,M,SD)
      
      #two-sample t test to verify mean is accurate
      sx <- var(x)
      sy <- var(y)
      pooledNumerator <- (1000-1)*sx^2 + (1000-1)*sy^2
      pooledDenominator <- 2*1000 -2
      pooledVariance <- pooledNumerator/pooledDenominator
      my.t <- (mean(x) - mean(y)) / (sqrt(pooledVariance) * sqrt(2/1000))
      meanProb <- pt(my.t, 2*1000 -2)
      if(meanProb<0.01){
        stop("mean of generated deviates differs significantly from that specified")
      }
      
      #F test to verify the variance is accurate
      my.F <- var(y)/var(x)
      if(pf(my.F,1000-1,1000-1)<0.01){
        stop("variance of generated deviates differs significantly from that specified")
      }
    }
  }
}


my.rchisq.test <- function(){
  #Purpose: Testing the my.rnorm funtion
  #Input: none
  #output: a plot, as well as errors if the function fails the tests
  #notes: produces 3 plots and changes the graphical parameter to show a row of 3 plots
  
  par(mfrow = c(1,3))
  y <- my.rchisq(1000, df = 3)
  ## Q-Q plot for Chi^2 data against true theoretical distribution:
  qqplot(qchisq(ppoints(500), df = 3), y,
         main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]),xlab = "theoretical distribution")
  qqline(y, distribution = function(p) qchisq(p, df = 3),
         prob = c(0.1, 0.6), col = 2)
  
  #produces comparative histograms to manually check accuracy
  hist(rchisq(2000,3))
  hist(my.rchisq(2000,3))
  
  #check size of output matched the size n specified
  test.lengths <- c(1:10, 55,100)
  for(i in test.lengths){
    if(length(my.rchisq(i)) != i){
      stop("output size and input n specified to not match")
    }
  }
  
  #checks the correct errors are presented for incorrect inputs
  my.error <- try(my.rchisq(-1),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject negative n")}
  my.error <- try(my.rchisq(1,-1),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject negative degrees of freedom")}
  my.error <- try(my.rchisq(0.5),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject non-integer n")}
  my.error <- try(my.rchisq(1,0.5),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject non-integer degrees of freedom")}
  my.error <- try(my.rchisq(4,c(2,4,6,8)),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject a vector of degrees of freedom")}
}

my.rt.test <- function(){
  #Purpose: Testing the my.rnorm funtion
  #Input: none
  #output: a plot, as well as errors if the function fails the tests
  #notes: produces 3 plots and changes the graphical parameter to show a row of 3 plots
  
  par(mfrow = c(1,3))
  y <- my.rt(1000, df = 3)
  ## Q-Q plot for t distribution data against true theoretical distribution:
  qqplot(qt(ppoints(500), df = 3), y,
         main = expression("Q-Q plot for" ~~ t[nu == 3]),xlab = "theoretical distribution")
  qqline(y, distribution = function(p) qt(p, df = 3),
         prob = c(0.1, 0.6), col = 2)
  
  #produces comparative histograms to manually check accuracy
  hist(rt(2000,3))
  hist(my.rt(2000, 3))
  
  #check size of output matched the size n specified
  test.lengths <- c(1:10, 55,100)
  for(i in test.lengths){
    if(length(my.rt(i)) != i){
      stop("output size and input n specified to not match")
    }
  }
  
  #checks the correct errors are presented for incorrect inputs
  my.error <- try(my.rt(-1),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject negative n")}
  my.error <- try(my.rt(1,-1),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject negative degrees of freedom")}
  my.error <- try(my.rt(0.5),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject non-integer n")}
  my.error <- try(my.rt(1,0.5),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject non-integer degrees of freedom")}
  my.error <- try(my.rt(4,c(2,4,6,8)),silent = TRUE)
  if (class(my.error) != "try-error"){stop("Failed to reject a vector of degrees of freedom")}
}

