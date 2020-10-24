library(png)
library(grid)
library(gridExtra)

ROOT = "/Users/mikkeldanielsen/myStat/"

## Confidence intervals ##

oneSampleConf <- function(x, s, n, alpha){
  t <- qt(1-alpha/2, n-1)
  delta <- t*s/sqrt(n)
  
  from <- x-delta
  to <- x+delta
  
  cat(
    "t =", t,
    "\ndelta =", delta,
    "\nfrom =", from,
    "\nto =", to
    )
}

ioneSampleConf <- function(...){
  img <- c("oneSampleConf.png")
  printImages(img, 1, 1)
  
  cat(
    "One sample confidence interval",
    "\n\n(x, s, n, alpha)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)"
    )
}

twoSampleConf <- function(x1, s1, n1, x2, s2, n2, alpha){
  v <- ((s1^2/n1)+(s2^2/n2))^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
  t <- qt(1-alpha/2, v)
  delta <- t*sqrt((s1^2/n1)+(s2^2/n2))
  
  from <- (x1-x2)-delta
  to <- (x1-x2)+delta
 
  cat(
    "t =", t,
    "\ndf =", v,
    "\ndelta =", delta,
    "\nfrom =", from,
    "\nto =", to
  )
}

itwoSampleConf <- function(...){
  img <- c("twoSampleConf.png")
  printImages(img, 1, 1)
  
  cat(
    "Two sample confidence interval",
    "\n\n(x1, s1, n1, x2, s2, n2, alpha)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)"
  )
}

## Hypothesis testing ##

oneSampleHyp <- function(x, s, n, alpha, delta){
  tobs <- (x-delta)/(s/sqrt(n))
  p <- 2*(1-pt(abs(tobs), n-1))
  c <- qt(1-alpha/2, n-1)
  
  cat(
    "tobs =", tobs,
    "\np-value =", p,
    "\nc-value =", c 
  )
}

ioneSampleHyp <- function(...){
  cat(
    "One sample hypothesis testing",
    "\n\n(x, s, n, alpha, delta)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical mean"
  )
  img <- c("oneSampleHyp.png")
  printImages(img, 1, 1)
}


twoSampleHyp <- function(x1, s1, n1, x2, s2, n2, alpha, delta){
  tobs <- ((x1-x2)-delta)/(sqrt((s1^2/n1)+(s2^2/n2)))
  v <- ((s1^2/n1)+(s2^2/n2))^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
  p <- 2*(1-pt(abs(tobs), v))
  c <- qt(1-alpha/2, v)
  
  cat(
    "tobs =", tobs,
    "\ndf =", v,
    "\np-value =", p,
    "\nc-value =", c
    )
}

itwoSampleHyp <- function(...){
  cat(
    "Two sample hypothesis testing",
    "\n\n(x1, s1, n1, x2, s2, n2, alpha, delta)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical difference"
    )
  img <- c("twoSampleHyp.png")
  printImages(img, 1, 1)
  
}

## Bootstrapping ##

oneSampleBoot <- function(set, k , func){
  simSamples <- replicate(k, sample(set, replace = TRUE))
  simDist <- apply(simSamples, 2, func)
  return(simDist)
}

ioneSampleBoot <- function(...){
  cat(
    "One sample non-parametric bootstrapping",
    "\n\n(set, k, func)",
    "\nset = vector of observations",
    "\nk = number of simulations",
    "\nfunc = function to extract feature of interst",
    "\n\nreturns simulated distribution (as a vector)"
    )
  img <- c("oneSampleBoot.png")
  printImages(img, 1, 1)
}

twoSampleBoot <- function(set1, set2, k, func){
  simSamplesOne <- replicate(k, sample(set1, replace = TRUE))
  simSamplesTwo <- replicate(k, sample(set2, replace = TRUE))
  simDist <- apply(simSamplesOne, 2, func) - apply(simSamplesTwo, 2, func)
  return(simDist)
}

itwoSampleBoot <- function(...){
  cat(
    "Two sample non-parametric bootstrapping",
    "\n\n(set1, set2, k, func)",
    "\nset = vector of observations",
    "\nk = number of simlations",
    "\nfunc = function to extract feature of interst",
    "\n\nreturns simulated distribution (as a vector)"
  )
  img <- c("twoSampleBoot.png")
  printImages(img, 1, 1)
}

oneSampleParaBoot <- function(k, func, n, model, ...){
  
  args <- list(n, ...)
  rmodel <- paste("r", model, sep="")
  simSamples <- replicate(k, do.call(rmodel, args))
  simDist <- apply(simSamples, 2, func) 
  return(simDist)
}

ioneSampleParaBoot <- function(...){
  cat(
    "One sample parametric bootstrapping",
    "\n\n(k, func, n, model, args...)",
    "\nk = number of simlations",
    "\nfunc = function to extract feature of interst",
    "\nn = size of sample",
    "\nmodel = assumed distribution (as string)",
    "\nargs... = model parameters",
    "\n\nreturns simulated distribution (as a vector)"
  )
  img <- c("oneSampleParaBoot.png")
  printImages(img, 1, 1)
}

twoSampleParaBoot <- function(k, func, n1, n2, model, listArgs1, listArgs2){
  rmodel <- paste("r", model, sep="")
  simSamplesOne <- replicate(k, do.call(rmodel, listArgs1))
  simSamplesTwo <- replicate(k, do.call(rmodel, listArgs2))
  simDist <- apply(simSamplesOne, 2, func) - apply(simSamplesTwo, 2, func) 
  return(simDist)
}

itwoSampleParaBoot <- function(...){
  cat(
    "Two sample parametric bootstrapping",
    "\n\n(k, func, n1, n2, model, listArgs1, listArgs2)",
    "\nk = number of simlations",
    "\nfunc = function to extract feature of interst",
    "\nn = size of sample",
    "\nmodel = assumed distribution (as string)",
    "\nlistArgs = model parameters (as list)",
    "\n\nreturns simulated distribution (as a vector)"
  )
  img <- c("twoSampleParaBoot.png")
  printImages(img, 1, 1)
}


## Correlation ##

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

icor <- function(...){
  cat("Calculate correlation between two sets of equal size",
      "\n\n(set1, set2)",
      "\nset = vector of observations",
      "\n\nreturn correlation coefficient")
  
  imgs <- c("covariance.png","correlation.png")
  printImages(imgs, 2, 1)
}


#############################################
## PLOT FUNCTIONS 
#############################################


#############################################
## INFO FUNCTIONS
#############################################

iconf <- function(...){
  imgs <- c("oneSampleConf.png","oneSampleVarSd.png","twoSampleConf.png")
  printImages(imgs, 2, 2)
}

ihyp <- function(...){
  imgs <- c("oneSampleHyp.png","twoSampleHyp.png")
  printImages(imgs, 1,2)
}

iboot <- function(...){
  imgs <- c("oneSampleBoot.png","twoSampleBoot.png", "oneSampleParaBoot.png", "twoSampleParaBoot.png")
  printImages(imgs, 2,2)
}

ipropConvert <- function(...){
  cat(
    "P(X<3) = P(X≤2) = F(2)",
    "\nP(X>3) = 1−P(≤3) = 1−F(3)",
    "\nP(X≥3) = 1−P(X≤2) = 1−F(2)",
    "\nP(≤3) = F(3)"
    )
}



#############################################
## HELP FUNCTIONS 
#############################################

printImages <- function(files, nRow, nCol){
  
  # Add images to list
  imgs <- list()
  for(file in files){
    img <- rasterGrob(readPNG(paste(ROOT, file, sep="")))
    imgs <- c(imgs, list(img))
  }
  
  # Print images
  do.call("grid.arrange", c(imgs, nrow=nRow, ncol=nCol))
}





