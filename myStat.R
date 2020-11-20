library(png)
library(grid)
library(gridExtra)

ROOT <- "/Users/mikkeldanielsen/myStat/"
setwd(paste(ROOT, "functions", sep="/"))
file.sources = list.files(pattern="*.R")
sapply(file.sources, source, .GlobalEnv)

#############################################
## OTHER FUNCTIONS
#############################################

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
  cat(
    "Calculate correlation between two vectors of equal size",
    "\n\n(v1, v2)",
    "\nv = vector of observations",
    "\n\nreturns correlation coefficient"
  )
  
  imgs <- c("res/covariance.png","res/correlation.png")
  printImages(imgs, 2, 1)
}

R2 <- function(x,y){
  R2=cor(x,y)^2
  
  cat(
    "R^2 =", R2,
    "\n"
  )
  
  return(R2)
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

isigLevCovnert <- function(...){
  cat(
    "Calculates relevant quantities from the given significance level",
    "\n\n(p)",
    "\np = significance level as decimal"
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

icriticalValueT <- function(...){
  cat(
    "Calculate critical value from certain alpha for a given T-dist",
    "\nalpha = significance level (as decimal)",
    "\ndf = degrees of freemdom",
    "\n\nreturns critical value"
  )
  
  img <- c("res/twoSampleHyp.png")
  printImages(img, 1, 1)
}

#############################################
## INFO FUNCTIONS
#############################################

iprobConvert <- function(...){
  cat(
    "P(X<3) = P(X≤2) = F(2)",
    "\nP(X>3) = 1−P(≤3) = 1−F(3)",
    "\nP(X≥3) = 1−P(X≤2) = 1−F(2)",
    "\nP(≤3) = F(3)"
    )
}

icalcRules <- function(...){
  imgs <- c("res/linearOne.png","res/linearMore.png", "res/nonLinearVar.png", "res/nonLinearVar2.png")
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
  imgs <- c("res/expDist.png")
  printImages(imgs, 1,1)
}

ilogNormDist <- function(...){
  imgs <- c("res/logNormDist.png")
  printImages(imgs, 1,1)
}

inormDist <- function(...){
  imgs <- c("res/normDist.png")
  printImages(imgs, 1,1)
}

iuniDist <- function(...){
  imgs <- c("res/uniDist.png")
  printImages(imgs, 1,1)
}

ipoisDist <- function(...){
  imgs <- c("res/poisDist.png")
  printImages(imgs, 1,1)
}

ihypgeoDist <- function(...){
  imgs <- c("res/hypgeoDist.png")
  printImages(imgs, 1,1)
}

ibinomDist <- function(...){
  imgs <- c("res/binomDist.png")
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





