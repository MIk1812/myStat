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

iconf <- function(...){
  imgs <- c(paste("res", SEPERATOR, "oneSampleConf.png", sep=""),
            paste("res", SEPERATOR, "oneSampleVarSd.png", sep=""),
            paste("res", SEPERATOR, "twoSampleConf.png", sep=""))
  printImages(imgs, 2, 2)
}