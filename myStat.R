library(png)
library(grid)
library(gridExtra)

ROOT = "/Users/mikkeldanielsen/myStat/"

## Linear Regression ##

linReg <- function(x, y){
  
  Sxx <- sum((x - mean(x))^2)
  
  beta1hat <- sum((x - mean(x))*(y - mean(y))) / Sxx
  beta0hat <- mean(y) - beta1hat * mean(x)
  
  cat(
    "beta0 =", beta0hat,
    "\nbeta1 =", beta1hat,
    "\n"
  )
  
  return(c(beta0hat, beta1hat))
}

ilinReg <- function(...){
  cat(
    "Leasts squares linear regression",
    "\n\n(x, y)",
    "\nx, y = vectors of observations",
    "\n\nreturns c(beta0, beta1)"
  )
  
  img <- c("linReg.png")
  printImages(img, 1, 1)
}

linStdErr <- function(x, y){

  sumErr = sum(linResiduals(x,y)^2)
  StdErr = sqrt(sumErr/(length(x)-2))
  
  cat(
    "Residual std error =", StdErr,
    "\n"
  )
  
  return(StdErr)
}

ilinStdErr <- function(...){
  cat(
    "Calculate residual standard error for linear relation",
    "\n\n(x, y)",
    "\nx, y = vectors of observations",
    "\n\nreturns redidual standard error"
  )
  
  img <- c("linStdErr.png")
  printImages(img, 1, 1)
}

linResiduals <- function(x,y){
  
  sink("null"); params <- linReg(x,y); sink()
  beta0 <- params[1]
  beta1 <- params[2]

  return(c(y-(beta0+beta1*x)))
}

ilinResiduals <- function(...){
  cat(
    "Calculate residuals",
    "\n\n(x, y)",
    "\nx, y = vector of observations",
    "\n\nreturns vector of residuals"
  )
  img <- c("linResiduals.png")
  printImages(img, 1, 1)
}

linStdErrBeta <- function(x,y){
  
  sink("null"); StdErr <- linStdErr(x,y); sink()
  Sxx <- sum((x - mean(x))^2)
  n <- length(x)
  
  StdErrBeta0 = StdErr*sqrt(1/n+mean(x)^2/Sxx)
  StdErrBeta1 = StdErr*sqrt( 1/( sum( (x-mean(x))^2 ) ) )
  
  cat(
    "std error beta0 = ", StdErrBeta0,
    "\nstd error beta1 = ", StdErrBeta1,
    "\n"
  )
  
  return(c(StdErrBeta0, StdErrBeta1))
}

ilinStdErrBeta <- function(...){
  cat(
    "Calculate standard error for beat0 and 1 in linear relation",
    "\n\n(x, y)",
    "\nx, y = vectors of observations",
    "\n\nreturns c(StdErrBeta0, StdErrBeta1)"
  )
  
  img <- c("stdErrBetas.png")
  printImages(img, 1, 1)
}

linHypBeta <- function(beta, stdErr, n, alpha, delta){
  tobs <- (beta-delta)/(stdErr)
  p <- 2*(1-pt(abs(tobs), n-2))
  c <- qt(1-alpha/2, n-2)
  
  cat(
    "tobs =", tobs,
    "\np-value =", p,
    "\nc-value =", c,
    "\n"
  )
  
  return(c(tobs,p,c))
}

ilinHypBeta <- function(...){
  cat(
    "Hypothesis test for beta in linear regression",
    "\n\n(beta, stdErr, n, alpha, delta)",
    "\nbeta = beta value",
    "\nstdErr = standard error of beta",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical beta value",
    "\n\nreturns c(tobs, p-value, critical value)"
  )
  
  img <- c("linHypBeta.png")
  printImages(img, 1, 1)
}

linConfBeta <- function(beta, stdErr, n, alpha){
  t <- qt(1-alpha/2, n-2)
  delta <- t*stdErr
  
  from <- beta-delta
  to <- beta+delta
  
  cat(
    "t =", t,
    "\ndelta =", delta,
    "\n", paste("[", from, "; ", to, "]", sep=""),
    "\n"
  )
  
  return(delta)
}

ilinConfBeta <- function(...){
  cat(
    "Confidence interval for beta in linear regression",
    "\n\n(beta, stdErr, n, alpha)",
    "\nx = mean",
    "\nstdErr = standard error of beta",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\n\n returns delta"
  )
  
  img <- c("linConfBeta.png")
  printImages(img, 1, 1)
}

linConf <- function(x, y, x0, alpha){
  
  sink("null")
  params <-linReg(x,y); 
  stdErr <- linStdErr(x,y)
  sink()
  
  beta0 <- params[1]
  beta1 <- params[2]
  y0 <- beta0+beta1*x0
  
  n <- length(x)
  t <- qt(1-alpha/2, n-2)
  Sxx <- sum((x - mean(x))^2)
  
  delta <- t*stdErr*sqrt(1/n+(((x0-mean(x))^2)/Sxx))
  from <- y0+delta
  to <- y0-delta
  
  cat(
    "beta0 =", beta0,
    "\nbeta1 =", beta1,
    "\ny0 =", y0,
    "\nt =", t,
    "\ndelta =", delta,
    "\n", paste("[", from, "; ", to, "]", sep=""),
    "\n"
  )
  
  return(c(y0, delta))
}

ilinConf <- function(...){
  cat(
    "Confidence interval for yhat in linear regression",
    "\n\n(x, y, x0, alpha)",
    "\nx, y = vectors of observations",
    "\nx0 = x-value of interest",
    "\nalpha = significance level (as decimal)",
    "\n\n returns c(y0, delta)"
  )
  
  img <- c("linConf.png")
  printImages(img, 1, 1)
}

linPredict <- function(x, y, x0, alpha){
  
  sink("null")
  params <-linReg(x,y); 
  stdErr <- linStdErr(x,y)
  sink()
  
  beta0 <- params[1]
  beta1 <- params[2]
  y0 <- beta0+beta1*x0
  
  n <- length(x)
  t <- qt(1-alpha/2, n-2)
  Sxx <- sum((x - mean(x))^2)
  
  delta <- t*stdErr*sqrt(1+1/n+(((x0-mean(x))^2)/Sxx))
  from <- y0+delta
  to <- y0-delta
  
  cat(
    "beta0 =", beta0,
    "\nbeta1 =", beta1,
    "\ny0 =", y0,
    "\nt =", t,
    "\ndelta =", delta,
    "\n", paste("[", from, "; ", to, "]", sep=""),
    "\n"
  )
  
  return(c(y0, delta))
}

ilinPredict <- function(...){
  cat(
    "Prediction interval for yhat in linear regression",
    "\n\n(x, y, x0, alpha)",
    "\nx, y = vectors of observations",
    "\nx0 = x-value of interest",
    "\nalpha = significance level (as decimal)",
    "\n\n returns c(y0, delta)"
  )
  
  img <- c("linPredict.png")
  printImages(img, 1, 1)
}

## Confidence intervals ##

oneSampleConf <- function(x, s, n, alpha){
  t <- qt(1-alpha/2, n-1)
  delta <- t*s/sqrt(n)
  
  from <- x-delta
  to <- x+delta
  
  cat(
    "t =", t,
    "\ndelta =", delta,
    "\n", paste("[", from, "; ", to, "]", sep=""),
    "\n"
    )
  
  return(delta)
}

ioneSampleConf <- function(...){
  cat(
    "One sample confidence interval",
    "\n\n(x, s, n, alpha)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\n\n returns delta"
    )
  
  img <- c("oneSampleConf.png")
  printImages(img, 1, 1)
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
    "\n", paste("[", from, "; ", to, "]", sep=""),
    "\n"
  )
  
  return(delta)
}

itwoSampleConf <- function(...){
  cat(
    "Two sample confidence interval",
    "\n\n(x1, s1, n1, x2, s2, n2, alpha)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns delta"
  )
  
  img <- c("twoSampleConf.png")
  printImages(img, 1, 1)
}

## Hypothesis testing ##

oneSampleHyp <- function(x, s, n, alpha, delta){
  tobs <- (x-delta)/(s/sqrt(n))
  p <- 2*(1-pt(abs(tobs), n-1))
  c <- qt(1-alpha/2, n-1)
  
  cat(
    "tobs =", tobs,
    "\np-value =", p,
    "\nc-value =", c,
    "\n"
  )
  
  return(c(tobs,p,c))
}

ioneSampleHyp <- function(...){
  cat(
    "One sample hypothesis testing",
    "\n\n(x, s, n, alpha, delta)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical mean",
    "\n\nreturns c(tobs, p-value, critical value)"
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
    "\nc-value =", c,
    "\n"
    )
  
  return(c(tobs,v,p,c))
}

itwoSampleHyp <- function(...){
  cat(
    "Two sample hypothesis testing",
    "\n\n(x1, s1, n1, x2, s2, n2, alpha, delta)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical difference",
    "\n\nreturns c(tobs, df, p-value, critical value)"
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
  simSamplesOne <- replicate(k, do.call(rmodel, c(n1, listArgs1)))
  simSamplesTwo <- replicate(k, do.call(rmodel, c(n2, listArgs2)))
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
    "\nlistArgs = model parameters (as a list)",
    "\n\nreturns simulated distribution (as a list)"
  )
  
  img <- c("twoSampleParaBoot.png")
  printImages(img, 1, 1)
}


## Correlation ##

cor <- function(v1, v2){
  n <- length(set1)
  
  mean1 <- mean(v1)
  sd1 <- sd(v1)
  
  sd2 <- sd(v2)
  mean2 <- mean(v2)
  
  covar <- 1/(n-1)*sum((v1-mean1)*(v2-mean2))
  cor <- covar/(sd1*sd2)
  
  return(cor)
}

icor <- function(...){
  cat(
    "Calculate correlation between two vectors of equal size",
    "\n\n(v1, v2)",
    "\nv = vector of observations",
    "\n\nreturn correlation coefficient"
  )
  
  imgs <- c("covariance.png","correlation.png")
  printImages(imgs, 2, 1)
}


#############################################
## PLOT FUNCTIONS 
#############################################


#############################################
## INFO FUNCTIONS
#############################################

ilinRegInfo <- function(...){
  print("printing...")
  imgs <- c("linReg.png", "linStdErr.png", "stdErrBetas.png", "linHypBeta.png", "linConfBeta.png", "linConf.png", "linPredict.png")
  printImages(imgs, 4, 2)
}

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

icalcRules <- function(...){
  imgs <- c("linearOne.png","linearMore.png", "nonLinearVar.png", "nonLinearVar2.png")
  printImages(imgs, 2,2)
}

## Distributions ##

idists <- function(...){
  cat(
    "binom(n, p)",
    "\npois(lambda)",
    "\nhyper(n, a, N)",
    "\nnorm(mu, sigma)",
    "\nlnorm(alpha, beta)",
    "\nexp(lambda)",
    "\nunif(alpha, beta)",
    "\nt(v)",
    "\nchisq(v)",
    "\nf(?)"
  )
} 

iexpDist <- function(...){
  imgs <- c("expDist.png")
  printImages(imgs, 1,1)
}

ilogNormDist <- function(...){
  imgs <- c("logNormDist.png")
  printImages(imgs, 1,1)
}

inormDist <- function(...){
  imgs <- c("normDist.png")
  printImages(imgs, 1,1)
}

iuniDist <- function(...){
  imgs <- c("uniDist.png")
  printImages(imgs, 1,1)
}

ipoisDist <- function(...){
  imgs <- c("poisDist.png")
  printImages(imgs, 1,1)
}

ihypgeoDist <- function(...){
  imgs <- c("hypgeoDist.png")
  printImages(imgs, 1,1)
}

ibinomDist <- function(...){
  imgs <- c("binomDist.png")
  printImages(imgs, 1,1)
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





