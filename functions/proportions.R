## Proportions ##

oneSamplePropConf <- function(n, phat, alpha){
  z <- qnorm(1-alpha/2)
  se <- sqrt((phat*(1-phat))/n)
  delta <- z*se
  
  from <- phat-delta
  to <- phat+delta
  
  cat(
    paste(
      "z = ", z,
      "\nse = ", se,
      "\ndelta = ", delta,
      "\n[", from, "; ", to, "]",
      "\n",
      sep=""
    )
  )
  
  return(delta)
}

oneSamplePropHyp <- function(x, n, p0, alpha){
  zobs <- (x-n*p0)/(sqrt(n*p0*(1-p0)))
  p <- 2*(1-pnorm(abs(zobs)))
  c <- qnorm(1-alpha/2)
  
  cat(
    "zobs =", zobs,
    "\np-value =", p,
    "\nc-value =", c,
    "\n"
  )
  
  return(c(zobs,p,c))
}

twoSamplePropConf <- function(n1, n2, phat1, phat2, alpha){
  z <- qnorm(1-alpha/2)
  se <- sqrt(((phat1*(1-phat1))/(n1))+((phat2*(1-phat2))/(n2)))
  delta <- z*se
  diff <- phat1-phat2
  
  from <- diff-delta
  to <- diff+delta
  
  cat(
    paste(
      "diff = ", diff,
      "\nz = ", z,
      "\nse = ", se,
      "\ndelta = ", delta,
      "\n[", from, "; ", to, "]",
      "\n",
      sep=""
    )
  )
  
  return(delta)
}

twoSamplePropHyp <- function(x1, x2, n1, n2, alpha){
  phat1 <- x1/n1
  phat2 <- x2/n2
  phat <- (x1+x2)/(n1+n2)
  zobs <- (phat1-phat2)/(sqrt(phat*(1-phat)*(1/n1+1/n2)))
  p <- 2*(1-pnorm(abs(zobs)))
  c <- qnorm(1-alpha/2)
  
  cat(
    "phat =", phat,
    "\nzobs =", zobs,
    "\np-value =", p,
    "\nc-value =", c,
    "\n"
  )
  
  return(c(phat,zobs,p,c))
}

propMeanVar <- function(p, n){
  var <- (p*(1-p))/n
  
  cat(
    "mean = ", p,
    "\nvar =", var,
    "\n"
  )
}

#############################################
## INFO FUNCTIONS
#############################################

ioneSamplePropConf <- function(...){
  cat(
    "One proportion confidence interval",
    "\n\n(n, phat, alpha)",
    "\nn = number of observations",
    "\nphat = estimated proportion (as decimal)",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns delta"
  )
  
  img <- c(paste("res", SEPERATOR, "onePropConf.png", sep=""))
  printImages(img, 1, 1)
}

ioneSamplePropHyp <- function(...){
  cat(
    "One proportion hypothesis testing",
    "\n\n(x, n, p0, alpha)",
    "\nx = number of successes",
    "\nn = number of observations",
    "\np0 = hypothetical p (H0) (as decimal)",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns c(zobs, p-value, critical value)"
  )
  
  img <- c(paste("res", SEPERATOR, "onePropHyp.png", sep=""))
  printImages(img, 1, 1)
}

itwoSamplePropConf <- function(...){
  cat(
    "Two proportions confidence interval",
    "\n\n(n1, n2, phat1, phat2, alpha)",
    "\nn = number of observations",
    "\nphat = estimated proportion (as decimal)",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns delta"
  )
  
  img <- c(paste("res", SEPERATOR, "twoPropConf.png", sep=""))
  printImages(img, 1, 1)
}

itwoSamplePropHyp <- function(...){
  cat(
    "Two proportions hypothesis testing",
    "\n\n(x1, x2, n1, n2, alpha)",
    "\nx = number of successes",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns c(phat, zobs, p-value, critical value)"
  )
  
  img <- c(paste("res", SEPERATOR, "twoPropHyp.png", sep=""))
  printImages(img, 1, 1)
}

ipropMeanVar <- function(...){
  cat(
    "Calculates mean and variance of given proportion",
    "\n\n(p, n)",
    "\np = proportion (as decimal)",
    "\nn = number of observations"
  )
  
  img <- c(paste("res", SEPERATOR, "propMeanVar.png", sep=""))
  printImages(img, 1, 1)
}

iprop <- function(...){
  print("printing...")
  imgs <- c(paste("res", SEPERATOR, "propMeanVar.png", sep=""), 
            paste("res", SEPERATOR, "twoPropHyp.png", sep=""), 
            paste("res", SEPERATOR, "twoPropConf.png", sep=""), 
            paste("res", SEPERATOR, "onePropHyp.png", sep=""), 
            paste("res", SEPERATOR, "onePropConf.png", sep=""))
  printImages(imgs, 2,3)
}