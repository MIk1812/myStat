## Bootstrapping ##

oneSampleBoot <- function(set, k , func){
  simSamples <- replicate(k, sample(set, replace = TRUE))
  simDist <- apply(simSamples, 2, func)
  
  return(simDist)
}

twoSampleBoot <- function(set1, set2, k, func){
  simSamplesOne <- replicate(k, sample(set1, replace = TRUE))
  simSamplesTwo <- replicate(k, sample(set2, replace = TRUE))
  simDist <- apply(simSamplesOne, 2, func) - apply(simSamplesTwo, 2, func)
  
  return(simDist)
}

oneSampleParaBoot <- function(k, func, n, model, ...){
  
  args <- list(n, ...)
  rmodel <- paste("r", model, sep="")
  simSamples <- replicate(k, do.call(rmodel, args))
  simDist <- apply(simSamples, 2, func) 
  
  return(simDist)
}

twoSampleParaBoot <- function(k, func, n1, n2, model, listArgs1, listArgs2){
  rmodel <- paste("r", model, sep="")
  simSamplesOne <- replicate(k, do.call(rmodel, c(n1, listArgs1)))
  simSamplesTwo <- replicate(k, do.call(rmodel, c(n2, listArgs2)))
  simDist <- apply(simSamplesOne, 2, func) - apply(simSamplesTwo, 2, func) 
  
  return(simDist)
}

#############################################
## INFO FUNCTIONS
#############################################

ioneSampleBoot <- function(...){
  cat(
    "One sample non-parametric bootstrapping",
    "\n\n(set, k, func)",
    "\nset = vector of observations",
    "\nk = number of simulations",
    "\nfunc = function to extract feature of interst",
    "\n\nreturns simulated distribution (as a vector)"
  )
  
  img <- c(paste("res", SEPERATOR, "oneSampleBoot.png", sep=""))
  printImages(img, 1, 1)
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
  
  img <- c(paste("res", SEPERATOR, "twoSampleBoot.png", sep=""))
  printImages(img, 1, 1)
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
  
  img <- c(paste("res", SEPERATOR, "oneSampleParaBoot.png", sep=""))
  printImages(img, 1, 1)
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
  
  img <- c(paste("res", SEPERATOR, "twoSampleParaBoot.png", sep=""))
  printImages(img, 1, 1)
}

iboot <- function(...){
  print("printing...")
  imgs <- c(paste("res", SEPERATOR, "oneSampleBoot.png", sep=""),
            paste("res", SEPERATOR, "twoSampleBoot.png", sep=""), 
            paste("res", SEPERATOR, "oneSampleParaBoot.png", sep=""), 
            paste("res", SEPERATOR, "twoSampleParaBoot.png", sep=""))
  printImages(imgs, 2,2)
}
