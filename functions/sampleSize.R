## Sample size ##

oneSampleSizeConf <- function(ME, sigma, alpha){
  z <- qnorm(1-(alpha/2))
  n <- ((z*sigma)/ME)^2
  
  cat(
    "n =", n,
    "\nz =", z,
    "\n"
  )
  
  return(n)
}

oneSampleSizePower <- function(mu0, mu1, sigma, alpha, beta){
  z1 <- qnorm(1-beta)
  z2 <- qnorm(1-(alpha/2))
  n <- (sigma*((z1+z2)/(mu0-mu1)))^2
  
  cat(
    "n =", n,
    "\nz1 =", z1,
    "\nz2 =", z2,
    "\n"
  )
  
  return(n)
}

onePropSizeConf <- function(p, ME, alpha){
  z <- qnorm(1-(alpha/2))
  n <- p*(1-p)*(z/ME)^2
  
  cat(
    "n =", n,
    "\nz =", z,
    "\n"
  )
  
  return(n)
}

#############################################
## INFO FUNCTIONS
#############################################

ioneSampleSizeConf <- function(...){
  cat(
    "Calculates needed sample size (n) from parameters",
    "\n\n(ME, sigma, alpha)",
    "\nME = Margin of error",
    "\nsigma = standard deviation",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns n"
  )
  
  img <- c(paste("res", SEPERATOR, "oneSampleSizeConf.png", sep=""))
  printImages(img, 1, 1)
}

ioneSampleSizePower <- function(...){
  cat(
    "Calculates needed sample size (n) from parameters",
    "\n\n(mu0, mu1, sigma, alpha, power)",
    "\nmu = mean",
    "\nsigma = standard deviation",
    "\nalpha = significance level (as decimal)",
    "\nbeta = 1-power (as decimal)",
    "\n\nreturns n"
  )
  
  img <- c(paste("res", SEPERATOR, "oneSampleSizePower.png", sep=""))
  printImages(img, 1, 1)
}

ionePropSizeConf <- function(...){
  cat(
    "Calculates needed sample size (n) from parameters",
    "\n\n(p, ME, alpha)",
    "\np = proportion (as decimal)",
    "\nME = Margin of error",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns n"
  )
  
  img <- c(paste("res", SEPERATOR, "onePropSizeConf.png", sep=""))
  printImages(img, 1, 1)
}

isampleSize <- function(...){
  print("printing...")
  imgs <- c(paste("res", SEPERATOR, "oneSampleSizeConf.png", sep=""), 
            paste("res", SEPERATOR, "oneSampleSizePower.png", sep=""), 
            paste("res", SEPERATOR, "onePropSizeConf.png", sep=""))
  printImages(imgs, 2,2)
}