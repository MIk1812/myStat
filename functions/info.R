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
  imgs <- c("res/linearOne.png","res/linearMore.png", "res/nonLinearVar.png", "res/nonLinearVar2.png")
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
