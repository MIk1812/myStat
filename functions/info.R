## Just info ##

iprobConvert <- function(...){
  cat(
    "P(X<3) = P(X≤2) = F(2)",
    "\nP(X>3) = 1−P(≤3) = 1−F(3)",
    "\nP(X≥3) = 1−P(X≤2) = 1−F(2)",
    "\nP(≤3) = F(3)"
  )
}

icalcRules <- function(...){
  imgs <- c(paste("res", SEPERATOR, "linearOne.png", sep=""),
            paste("res", SEPERATOR, "linearMore.png", sep=""), 
            paste("res", SEPERATOR, "nonLinearVar.png", sep=""), 
            paste("res", SEPERATOR, "nonLinearVar2.png", sep=""))
  printImages(imgs, 2,2)
}

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
  imgs <- c(paste("res", SEPERATOR, "expDist.png", sep=""))
  printImages(imgs, 1,1)
}

ilogNormDist <- function(...){
  imgs <- c(paste("res", SEPERATOR, "logNormDist.png", sep=""))
  printImages(imgs, 1,1)
}

inormDist <- function(...){
  imgs <- c(paste("res", SEPERATOR, "normDist.png", sep=""))
  printImages(imgs, 1,1)
}

iuniDist <- function(...){
  imgs <- c(paste("res", SEPERATOR, "uniDist.png", sep=""))
  printImages(imgs, 1,1)
}

ipoisDist <- function(...){
  imgs <- c(paste("res", SEPERATOR, "poisDist.png", sep=""))
  printImages(imgs, 1,1)
}

ihypgeoDist <- function(...){
  imgs <- c(paste("res", SEPERATOR, "hypgeoDist.png", sep=""))
  printImages(imgs, 1,1)
}

ibinomDist <- function(...){
  imgs <- c(paste("res", SEPERATOR, "binomDist.png", sep=""))
  printImages(imgs, 1,1)
}
