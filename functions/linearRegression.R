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

linStdErr <- function(x, y){
  sumErr = sum(linResiduals(x,y)^2)
  StdErr = sqrt(sumErr/(length(x)-2))
  
  cat(
    "Residual std error =", StdErr,
    "\n"
  )
  
  return(StdErr)
}

linResiduals <- function(x,y){
  sink("null"); params <- linReg(x,y); sink()
  beta0 <- params[1]
  beta1 <- params[2]
  
  return(c(y-(beta0+beta1*x)))
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

linHypBeta <- function(beta, stdErrBeta, n, alpha, delta){
  tobs <- (beta-delta)/(stdErrBeta)
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

linConfBeta <- function(beta, stdErrBeta, n, alpha){
  t <- qt(1-alpha/2, n-2)
  delta <- t*stdErrBeta
  
  from <- beta-delta
  to <- beta+delta
  
  cat(
    paste(
      "t = ", t,
      "\ndelta = ", delta,
      "\n[", from, "; ", to, "]", 
      "\n", 
      sep=""
    )
  )
  
  return(delta)
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
  from <- y0-delta
  to <- y0+delta
  
  cat(
    paste(
      "beta0 = ", beta0,
      "\nbeta1 = ", beta1,
      "\ny0 = ", y0,
      "\nt = ", t,
      "\ndelta = ", delta,
      "\n[", from, "; ", to, "]",
      "\n",
      sep=""
    )
  )
  
  return(c(y0, delta))
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
  from <- y0-delta
  to <- y0+delta
  
  cat(
    paste(
      "beta0 = ", beta0,
      "\nbeta1 = ", beta1,
      "\ny0 = ", y0,
      "\nt = ", t,
      "\ndelta = ", delta,
      "\n[", from, "; ", to, "]", 
      "\n",
      sep=""
    )
  )
  
  return(c(y0, delta))
}


#############################################
## INFO FUNCTIONS
#############################################

ilinReg <- function(...){
  cat(
    "Leasts squares linear regression",
    "\n\n(x, y)",
    "\nx, y = vectors of observations",
    "\n\nreturns c(beta0, beta1)"
  )
  
  img <- c("res/linReg.png")
  printImages(img, 1, 1)
}

ilinStdErr <- function(...){
  cat(
    "Calculate residual standard error for linear relation",
    "\n\n(x, y)",
    "\nx, y = vectors of observations",
    "\n\nreturns redidual standard error"
  )
  
  img <- c("res/linStdErr.png")
  printImages(img, 1, 1)
}

ilinResiduals <- function(...){
  cat(
    "Calculate all residuals",
    "\n\n(x, y)",
    "\nx, y = vector of observations",
    "\n\nreturns vector of residuals"
  )
  img <- c("res/linResiduals.png")
  printImages(img, 1, 1)
}

ilinStdErrBeta <- function(...){
  cat(
    "Calculate standard error for beat0 and 1 in linear regression",
    "\n\n(x, y)",
    "\nx, y = vectors of observations",
    "\n\nreturns c(StdErrBeta0, StdErrBeta1)"
  )
  
  img <- c("res/stdErrBetas.png")
  printImages(img, 1, 1)
}

ilinHypBeta <- function(...){
  cat(
    "Hypothesis test for beta in linear regression",
    "\n\n(beta, stdErrBeta, n, alpha, delta)",
    "\nbeta = beta value",
    "\nstdErr = standard error of beta",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical beta value (H0)",
    "\n\nreturns c(tobs, p-value, critical value)"
  )
  
  img <- c("res/linHypBeta.png")
  printImages(img, 1, 1)
}

ilinConfBeta <- function(...){
  cat(
    "Confidence interval for beta in linear regression",
    "\n\n(beta, stdErrBeta, n, alpha)",
    "\nx = mean",
    "\nstdErr = standard error of beta",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\n\n returns delta"
  )
  
  img <- c("res/linConfBeta.png")
  printImages(img, 1, 1)
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
  
  img <- c("res/linConf.png")
  printImages(img, 1, 1)
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
  
  img <- c("res/linPredict.png")
  printImages(img, 1, 1)
}

ilinRegInfo <- function(...){
  print("printing...")
  imgs <- c("res/linReg.png", "res/linStdErr.png", "res/stdErrBetas.png", "res/linHypBeta.png", "res/linConfBeta.png", "res/linConf.png", "res/linPredict.png")
  printImages(imgs, 4, 2)
}
