## Confidence intervals ##

oneSampleConf <- function(x, s, n, alpha){
  t <- qt(1-alpha/2, n-1)
  delta <- t*s/sqrt(n)
  
  from <- x-delta
  to <- x+delta
  
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

twoSampleConf <- function(x1, s1, n1, x2, s2, n2, alpha){
  v <- ((s1^2/n1)+(s2^2/n2))^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
  t <- qt(1-alpha/2, v)
  delta <- t*sqrt((s1^2/n1)+(s2^2/n2))
  
  from <- (x1-x2)-delta
  to <- (x1-x2)+delta
  
  cat(
    paste(
      "t = ", t,
      "\ndf = ", v,
      "\ndelta = ", delta,
      "\n[", from, "; ", to, "]",
      "\n",
      sep=""
    )
  )
  
  return(delta)
}

oneSampleVarConf <- function(n, var, alpha){
  v <- n-1
  chi1 <- qchisq(1-(alpha/2), v)
  chi2 <- qchisq(alpha/2, v)
  
  from <- ((n-1)*var)/chi1
  to <- ((n-1)*var)/chi2
  
  cat(
    paste(
      "chi1 = ", chi1,
      "\nchi2 = ", chi2,
      "\ndf = ", v,
      "\n[", from, "; ", to, "]",
      "\n",
      sep=""
    )
  )
}

oneSampleSdConf <- function(n, sd, alpha){
  v <- n-1
  chi1 <- qchisq(1-(alpha/2), v)
  chi2 <- qchisq(alpha/2, v)
  
  from <- sqrt(((n-1)*sd^2)/chi1)
  to <- sqrt(((n-1)*sd^2)/chi2)
  
  cat(
    paste(
      "chi1 = ", chi1,
      "\nchi2 = ", chi2,
      "\ndf = ", v,
      "\n[", from, "; ", to, "]",
      "\n",
      sep=""
    )
  )
}

#############################################
## INFO FUNCTIONS
#############################################

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
  
  img <- c(paste("res", SEPERATOR, "oneSampleConf.png", sep=""))
  printImages(img, 1, 1)
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
  
  img <- c(paste("res", SEPERATOR, "twoSampleConf.png", sep=""))
  printImages(img, 1, 1)
}

ioneSampleVarConf <- function(...){
  cat(
    "One sample confidence interval for variance",
    "\n\n(n, var, alpha)",
    "\nn = number of observations",
    "\nvar = variance",
    "\nalpha = significance level (as decimal)"
  )
  
  img <- c(paste("res", SEPERATOR, "oneSampleVarSdConf.png", sep=""))
  printImages(img, 1, 1)
}

ioneSampleSdConf <- function(...){
  cat(
    "One sample confidence interval for standard deviation",
    "\n\n(n, sd, alpha)",
    "\nn = number of observations",
    "\nsd = standard deviation",
    "\nalpha = significance level (as decimal)"
  )
  
  img <- c(paste("res", SEPERATOR, "oneSampleVarSdConf.png", sep=""))
  printImages(img, 1, 1)
}

iconf <- function(...){
  imgs <- c(paste("res", SEPERATOR, "oneSampleConf.png", sep=""),
            paste("res", SEPERATOR, "oneSampleVarSd.png", sep=""),
            paste("res", SEPERATOR, "twoSampleConf.png", sep=""),
            paste("res", SEPERATOR, "oneSampleVarSdConf.png", sep=""))
  printImages(imgs, 2, 2)
}