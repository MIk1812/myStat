## ANOVA ##

anovaSST <- function(x, nrow, ncol){
  data <- t(matrix(x, ncol, nrow))
  
  SST <- 0
  mean <- mean(data)
  for(i in 1:ncol){
    SST = SST + sum((data[,i]-mean)^2) 
  }
  
  cat(
    "SST =", SST,
    "\n"
  )
  
  return(SST)
}

anovaSSE <- function(x, nrow, ncol){
  data <- t(matrix(x, ncol, nrow))
  
  SSE <- 0
  for(i in 1:ncol){
    mean <- mean(data[,i])
    SSE = SSE + sum((data[,i]-mean)^2) 
  }
  
  cat(
    "SSE =", SSE,
    "\n"
  )
  
  return(SSE)
}

anovaSSTr <- function(x, nrow, ncol){
  data <- t(matrix(x, ncol, nrow))
  
  SSTr <- 0
  for(i in 1:ncol){
    col <- data[,i]
    SSTr = SSTr + length(col)*(mean(col)-mean(data))^2
  }
  
  cat(
    "SSTr =", SSTr,
    "\n"
  )
  
  return(SSTr)
}

anovaMSTr <- function(SSTr, k){
  MSTr <- SSTr/(k-1)
  
  cat(
    "MSTr =", MSTr,
    "\n"
  )
  
  return(MSTr)
}

anovaMSE <- function(SSE, n, k){
  MSE <- SSE/(n-k)
  
  cat(
    "MSE =", MSE,
    "\n"
  )
  
  return(MSE)
}

anovaHyp <- function(SSTr, SSE, n, k, alpha){
  fobs <- ((SSTr)/(k-1))/((SSE)/(n-k))
  p <- 1-pf(fobs, k-1, n-k)
  c <- qf(1-alpha, n-1, n-k)
  
  cat(
    "fobs =", fobs,
    "\np-value =", p,
    "\nc-value =", c,
    "\n"
  )
  
  return(c(fobs,p,c))
}

postAnovaHyp <- function(y1, n1, y2, n2, n, k, MSE, alpha){
  tobs <- (y1-y2)/(sqrt(MSE*(1/n1+1/n2)))
  p <- 2*(1-pt(abs(tobs), n-k))
  c <- qt(1-alpha/2, v)
  
  cat(
    "tobs =", tobs,
    "\np-value =", p,
    "\nc-value =", c,
    "\n"
  )
  
  return(c(tobs,p,c))
}

postAnovaConf <- function(y1, n1, y2, n2, n, k, MSE, alpha){
  t <- qt(1-alpha/2, n-k)
  delta <- t*sqrt(MSE*(1/n1+1/n2))
  
  from <- (y1-y2)-delta
  to <- (y1-y2)+delta
  
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

#############################################
## INFO FUNCTIONS
#############################################

ianovaSST <- function(...){
  cat(
    "Calculates STT (total variance)",
    "\n\n(x, nrow, ncol)",
    "\nx = vector of obserations with groups as colums like",
    
    "\n\nA \tB \tC",
    "\n2.8 \t5.5 \t5.8", 
    "\n3.6 \t6.3 \t8.3",
    "\n3.4 \t6.1 \t6.9",
    
    "\n\nnrow = number of rows",
    "\nncol = number of colums",
    "\n\nreturns SST"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaCalc.png", sep=""))
  printImages(img, 1, 1)
}

ianovaSSE <- function(...){
  cat(
    "Calculates SSE ",
    "\n\n(x, nrow, ncol)",
    "\nx = vector of obserations with groups as colums like",
    
    "\n\nA \tB \tC",
    "\n2.8 \t5.5 \t5.8", 
    "\n3.6 \t6.3 \t8.3",
    "\n3.4 \t6.1 \t6.9",
    
    "\n\nnrow = number of rows",
    "\nncol = number of colums",
    "\n\nreturns SST"
  )

  img <- c(paste("res", SEPERATOR, "anovaCalc.png", sep=""))
  printImages(img, 1, 1)
}

ianovaSSTr <- function(...){
  cat(
    "Calculates SSTr ",
    "\n\n(x, nrow, ncol)",
    "\nx = vector of obserations with groups as colums like",
    
    "\n\nA \tB \tC",
    "\n2.8 \t5.5 \t5.8", 
    "\n3.6 \t6.3 \t8.3",
    "\n3.4 \t6.1 \t6.9",
    
    "\n\nnrow = number of rows",
    "\nncol = number of colums",
    "\n\nreturns SST"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaCalc.png", sep=""))
  printImages(img, 1, 1)
}

ianovaMSTr <- function(...){
  cat(
    "Calculates MSTr",
    "\n\n(SSTr, k)",
    "\nSSTr = SSTr",
    "\nk = number of groups",
    "\n\nreturns MSTr"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaTable.png", sep=""))
  printImages(img, 1, 1)
}

ianovaMSE <- function(...){
  cat(
    "Calculates MSE",
    "\n\n(SSE, n, k)",
    "\nSSE = SSE",
    "\nn = total number of obervations",
    "\nk = number of groups",
    "\n\nreturns MSE"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaTable.png", sep=""))
  printImages(img, 1, 1)
}

ianovaHyp <- function(...){
  cat(
    "ANOVA hypothesis testing if H0 : a1=a2=a3...",
    "\n\n(SSTr, SSE, n, k, alpha)",
    "\nSSTr = SSTR",
    "\nSSE = SSE",
    "\nn = total number of obervations",
    "\nk = number of groups",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns c(fobs, p-value, critical value)"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaF.png", sep=""))
  printImages(img, 1, 1)
}

ipostAnovaHyp <- function(...){
  cat(
    "Post two sample hypothesis testing - ANOVA",
    "\n\n(y1, n1, y2, n2, n, k, MSE, alpha)",
    "\ny = mean",
    "\nn = number of observations",
    "\nk = number of groups",
    "\nMSE = MSE",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns c(tobs, p-value, critical value)"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaPostHyp.png", sep=""))
  printImages(img, 1, 1)
}

ipostAnovaConf <- function(...){
  
  cat(
    "Post two sample confidence interval - ANOVA",
    "\n\n(y1, n1, y2, n2, n, k, MSE, alpha)",
    "\ny = mean",
    "\nn = number of observations",
    "\nk = number of groups",
    "\nMSE = MSE",
    "\nalpha = significance level (as decimal)",
    "\n\nreturns delta"
  )
  
  img <- c(paste("res", SEPERATOR, "anovaPostConf.png", sep=""))
  printImages(img, 1, 1)
}

ianova <- function(...){
  print("printing...")
  imgs <- c(paste("res", SEPERATOR, "anovaCalc.png", sep=""), 
            paste("res", SEPERATOR, "anovaF.png", sep=""), 
            paste("res", SEPERATOR, "anovaTable.png", sep=""), 
            paste("res", SEPERATOR, "anovaPostHyp.png", sep=""), 
            paste("res", SEPERATOR, "anovaPostConf.png", sep=""))
  printImages(imgs, 3,2)
}

