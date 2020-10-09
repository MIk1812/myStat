library(png)
library(grid)
library(gridExtra)

PATH = "/Users/mikkeldanielsen/myStat/"

twoSampleHyp <- function(x1, s1, n1, x2, s2, n2, alpha){
  tobs <- abs(((x1-x2)-0)/(sqrt((s1^2/n1)+(s2^2/n2))))
  v <- ((s1^2/n1)+(s2^2/n2))^2/(((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1)))
  p <- 2*(1-pt(tobs, v))
  c <- qt(1-alpha/2, v)
  
  cat("tobs =", tobs,
      "\ndf =", v,
      "\np-value =", p,
      "\nc-value =", c)
}

itwoSampleHyp <- function(...){
  
  img <- c("twoSampleHyp.png")
  printImages(img, 1, 1)
  
  cat("(x1, s1, n1, x2, s2, n2)",
      "\nx = mean",
      "\ns = standard deviation",
      "\nn = number of observations")
}

iConf <- function(...){
  imgs <- c("oneSampleConf.png","oneSampleVarSd.png","twoSampleConf.png")
  printImages(imgs, 2, 2)
}

printImages <- function(files, nRow, nCol){
  
  # Add images to list
  imgs <- list()
  for(file in files){
    img <- rasterGrob(readPNG(paste(PATH, file, sep="")))
    imgs <- c(imgs, list(img))
  }
  
  # Print images
  do.call("grid.arrange", c(imgs, nrow=nRow, ncol=nCol))
}





