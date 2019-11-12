# I confirm that the attached is my own work, except where clearly indicated
# in the text.


# my.rnorm function:


my.rnorm <- function(n, mean = 0, sd = 1) { 
  # Purpose: Uses the Box-Muller algorithm to produce a pair of independent
  # normally distributed deviates, with mean 0 and standard deviation 1, and  
  # transforms them into values with mean and standard deviation as given in  
  # input by multiplying by the standard deviation (sd) and adding on the mean 
  # (mean)

  # Inputs:
  #   scalar values: n, mean and sd, where n is the total number of observations,
  #   mean is the mean of the observations and sd is the standard deviation of
  #   the observations
  # Outputs:
  #   a vector of pseudo-random values from a normal distribution with n entries

  #n, mean and sd cannot be negative
  if (n < 0) stop("invalid arguments")
  if (mean < 0) stop("invalid arguments")
  if (sd < 0) stop("invalid arguments")
  
  # n, mean and sd should be numeric values
  if (!is.numeric(n)) stop("invalid arguments")
  if (!is.numeric(mean)) stop("invalid arguments")
  if (!is.numeric(sd)) stop ("invalid arguments")

  # creating an empty vector
  norm.deviates <- vector()



  # for case when n is equal to one
  a <- runif(1)                     
  b <- runif(1)                      
  x1 <- sin(2 * pi * a) * sqrt(-2 * log(b)) 
  x2 <- cos(2 * pi * a) * sqrt(-2 * log(b))
  n.is.one <- x1 * sd + mean  
  
  # for case when n is greater than or equal to two
  for (i in 1:n) {
    for (j in 1:n) {
      #Simulating 0.5 * n random deviates from a unifom distribution. 0.5 * n 
      #rather than n because the Box-Muller algorithm produces a pair of 
      #random deviates
      a <- runif(0.5 * n) 
      b <- runif(0.5 * n)  
      x1 <- sin(2 * pi * a) * sqrt(-2 * log(b)) 
      x2 <- cos(2 * pi * a) * sqrt(-2 * log(b)) 
      result1 <- x1 * sd + mean               
      result2 <- x2 * sd + mean 
      boxmuller <- c(result1, result2)
      norm.deviates[i] <- boxmuller[1]
      norm.deviates[j] <- boxmuller[2]
    }
  }
  
  if (n == 1) {
    return(n.is.one)
  } else {
    return(norm.deviates)
  }
}




# my.rchisq function:

my.rchisq <- function(n, df = 1) {
  # Purpose: Creates random chi-squared distributed deviates using random normal 
  # deviates which were created using the  my.rnorm function. Using the fact that 
  # the sum of the squares of normal random variables squared is equivalent to 
  # the chi-squared distribution.
  # Inputs:
  #   scalar values: n and df, where n is the total number of observations, and 
  #   df is the degrees of freedom of the distribution 
  # Outputs:
  #   a vector of pseudo-random values from a chi-squared distribution with 
  #   n entries

  # n and df cannot be negative
  if (n < 0) stop("invalid arguments")
  if (df < 0) stop("invalid arguments")
  
  # n and df should be numeric values
  if (!is.numeric(n)) stop("invalid arguments")
  if (!is.numeric(df)) stop("invalid arguments")
  
  # creating an empty vector
  chisq.deviates <- vector()

  
  for (i in 1:n) {
    standard.norm <- my.rnorm(df, 0, 1)
    #sum of standard normal distribution random deviates squared
    total <- sum(standard.norm ^ 2)
    chisq.deviates[i] <- total
  }
  return(chisq.deviates)
}




# my.rt function:

my.rt <- function(n, df = 1) {
  # Purpose: Creates random deviates with a Student's t distribution using random  
  # normal deviates and random chi-squared deviates which were created using the 
  # my.rnorm and my.rchisq functions. Using the fact that if z has a standard 
  # normal distribution and u has a chi-squared distribution with df degrees of 
  # freedom, then t = z / sqrt(u / df) is distributed with a Student's t 
  # distribution with df degrees of freedom.
  # Inputs:
  #   scalar values: n and df, where n is the total number of observations, and 
  #   df is the degrees of freedom of the distribution 
  # Outputs:
  #   a vector of pseudo-random values from a Student's t distribution with n
  #   entries

  # n and df cannot be negative
  if (n < 0) stop("invalid arguments")
  if (df < 0) stop("invalid arguments")
  
  # n and df should be numeric values
  if (!is.numeric(n)) stop("invalid arguments")
  if (!is.numeric(df)) stop("invalid arguments")
  
  # creating an empty vector
  t.deviates <- vector()
  for (i in 1:n) {
    # setting z equal to random standard normal deviate
    z <- my.rnorm(1, 0, 1)
    #setting u equal to random chi-squared deviate
    u <- my.rchisq(1, df)

    
    t <- (z / sqrt(u / df))
    t.deviates[i] <- t
  }
  return(t.deviates)
}



# Testing functions that check whether my.rnorm, my.rchisq and my.rt
# return vectors containing n numbers. Each function returns TRUE if this is  
# the case and FALSE if not
# Inputs:
#   same input values as for my.rnorm, my.rchisq and my.rt
# Outputs:
#   TRUE if vector contains n numbers, FALSE if not

length.norm <- function(n, mean, sd) {
  x <- my.rnorm(n, mean, sd)
  pass.test <- (length(x) == n)
  return(pass.test)
}

length.chisq <- function(n, df) {
  x <- my.rchisq(n, df)
  pass.test <- (length(x) == n)
  return(pass.test)
}

length.t <- function(n, df) {
  x <- my.rnorm(n, df)
  pass.test <- (length(x) == n)
  return(pass.test)
}

