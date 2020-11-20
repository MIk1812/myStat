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

#############################################
## INFO FUNCTIONS
#############################################

ioneSampleHyp <- function(...){
  cat(
    "One sample hypothesis testing",
    "\n\n(x, s, n, alpha, delta)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical mean (H0)",
    "\n\nreturns c(tobs, p-value, critical value)"
  )
  
  img <- c("res/oneSampleHyp.png")
  printImages(img, 1, 1)
}

itwoSampleHyp <- function(...){
  cat(
    "Two sample hypothesis testing",
    "\n\n(x1, s1, n1, x2, s2, n2, alpha, delta)",
    "\nx = mean",
    "\ns = standard deviation",
    "\nn = number of observations",
    "\nalpha = significance level (as decimal)",
    "\ndelta = hypothetical difference (H0)",
    "\n\nreturns c(tobs, df, p-value, critical value)"
  )
  
  img <- c("res/twoSampleHyp.png")
  printImages(img, 1, 1)
}

ihyp <- function(...){
  imgs <- c("res/oneSampleHyp.png","res/twoSampleHyp.png")
  printImages(imgs, 1,2)
}