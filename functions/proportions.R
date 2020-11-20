## Proportions ##

onePropConf <- function(n, phat, alpha){
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

onePropHyp <- function(x, n, p0, alpha){
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

twoPropConf <- function(n1, n2, phat1, phat2, alpha){
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

twoPropHyp <- function(x1, x2, n1, n2, alpha, df){
  phat1 <- x1/n1
  phat2 <- x2/n2
  phat <- (x1+x2)/(n1+n2)
  zobs <- (phat1-phat2)/(sqrt(phat*(1-phat)*(1/n1+1/n2)))
  p <- 1-pchisq(zobs, df)
  c <- qchisq(1-alpha, df)
  
  cat(
    "phat = ", phat,
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

ionePropConf <- function(...){
  cat(
    "One proportion confidence interval",
    "\n\n(n, phat, alpha)",
    "\nx = number of observations",
    "\nphat = estimated proportion (as decimal)",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns delta"
  )
  
  img <- c("res/onePropConf.png")
  printImages(img, 1, 1)
}

ionePropHyp <- function(...){
  cat(
    "One proportion hypothesis testing",
    "\n\n(x, n, p0, alpha)",
    "\nx = number of successes",
    "\nn = number of observations",
    "\np0 = hypothetical p (H0)",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns c(zobs, p-value, critical value)"
  )
  
  img <- c("res/onePropHyp.png")
  printImages(img, 1, 1)
}

itwoPropConf <- function(...){
  cat(
    "Two proportions confidence interval",
    "\n\n(n1, n2, phat1, phat2, alpha)",
    "\nx = number of observations",
    "\nphat = estimated proportion (as decimal)",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns delta"
  )
  
  img <- c("res/twoPropConf.png")
  printImages(img, 1, 1)
}

itwoPropHyp <- function(...){
  cat(
    "Two proportions hypothesis testing",
    "\n\n(x1, x2, n1, n2, alpha, df)",
    "\nx = number of successes",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndf = degrees of freedom",
    "\n\nreturns c(phat, zobs, p-value, critical value)"
  )
  
  img <- c("res/twoPropHyp.png")
  printImages(img, 1, 1)
}

ipropMeanVar <- function(...){
  cat(
    "Calculates mean and variance of given proportion",
    "\n\n(p, var)",
    "\np = proportion (as decimal)",
    "\nn = number of observations"
  )
  
  img <- c("res/propMeanVar.png")
  printImages(img, 1, 1)
}

iprop <- function(...){
  print("printing...")
  imgs <- c("res/propMeanVar.png", "res/twoPropHyp.png", "res/twoPropConf.png", "res/onePropHyp.png", "res/onePropConf.png")
  printImages(imgs, 2,3)
}
