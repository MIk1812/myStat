## Other functions ##

cor <- function(set1, set2){
  n <- length(set1)
  mean1 <- mean(set1)
  sd1 <- sd(set1)
  sd2 <- sd(set2)
  mean2 <- mean(set2)
  
  covar <- 1/(n-1)*sum((set1-mean1)*(set2-mean2))
  cor <- covar/(sd1*sd2)
  
  return(cor)
}

R2 <- function(x,y){
  R2=cor(x,y)^2
  
  cat(
    "R^2 =", R2,
    "\n"
  )
  
  return(R2)
}

sigLevCovnert <- function(p){
  alpha = 1-p
  tup = 1-alpha/2
  tdown = alpha/2
  
  cat(
    paste(
      "alpha = ", alpha, " = ", alpha*100, "%", 
      "\n1-alpha/2 = ", tup, " = ", tup*100, "%",
      "\nalpha/2 = ", tdown, " = ", tdown*100, "%",
      sep=""
    )
  )
}

criticalValueT <- function(alpha, df){
  c <- qt(1-alpha/2, df)
  
  cat(
    "Critical value =", c,
    "\n"
  )
  
  return(c)
}

critivalValueChi <- function(alpha, df){
  c <- qchisq(1-alpha, df)
  
  cat(
    "Critical value =", c,
    "\n"
  )
  
  return(c)
}

critivalValueF <- function(alpha, n, k){
  c <- qf(1-alpha, k-1, n-k)
  
  cat(
    "Critical value =", c,
    "\n"
  )
  
  return(c)
}

vectorToMatrix <- function(x, nrow, ncol){
  t(matrix(x, ncol, nrow))
}


#############################################
## INFO FUNCTIONS
#############################################

icor <- function(...){
  cat(
    "Calculate correlation between two vectors of equal size",
    "\n\n(v1, v2)",
    "\nv = vector of observations",
    "\n\nreturns correlation coefficient"
  )
  
  imgs <- c("res/covariance.png","res/correlation.png")
  printImages(imgs, 2, 1)
}

iR2 <- function(...){
  cat(
    "Calculate explained variance (R^2) between two vectors of equal size",
    "\n\n(v1, v2)",
    "\nv = vector of observations",
    "\n\nreturns R^2 as decimal"
  )
  
  imgs <- c("res/R2.png", "res/R2Cor.png")
  printImages(imgs, 2, 1)
}

isigLevCovnert <- function(...){
  cat(
    "Calculates relevant quantities from the given significance level",
    "\n\n(p)",
    "\np = significance level as decimal"
  )
  
}

icriticalValueT <- function(...){
  cat(
    "Calculate critical value from certain alpha for a given T-dist",
    "\n\n(alpha, df)",
    "\nalpha = significance level (as decimal)",
    "\ndf = degrees of freemdom",
    "\n\nreturns critical value"
  )
}

icriticalValueChi <- function(...){
  cat(
    "Calculate critical value from certain alpha for a given Chi-dist",
    "\n\n(alpha, df)",
    "\nalpha = significance level (as decimal)",
    "\ndf = degrees of freemdom",
    "\n\nreturns critical value"
  )
}

icriticalValueF <- function(...){
  cat(
    "Calculate critical value from certain alpha for a given F-dist",
    "\n\n(alpha, n, k)",
    "\nalpha = significance level (as decimal)",
    "\nn = number of observations",
    "\nk = number of groups",
    "\n\nreturns critical value"
  )
}

ivectorToMatrix <- function(...){
  cat(
    "Converts c() vector to matrix",
    "\n\n(x, nrow, ncol)",
    "\nx = vector of obserations with groups as colums like",
    
    "\n\nA \tB \tC",
    "\n2.8 \t5.5 \t5.8", 
    "\n3.6 \t6.3 \t8.3",
    "\n3.4 \t6.1 \t6.9",
    
    "\n\nnrow = number of rows",
    "\nncol = number of colums",
    "\n\nreturns matrix"
  )
}
