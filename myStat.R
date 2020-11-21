library(png)
library(grid)
library(gridExtra)

ROOT <- "/Users/mikkeldanielsen/myStat/"
setwd(paste(ROOT, "functions", sep="/"))
file.sources = list.files(pattern="*.R")
sapply(file.sources, source, .GlobalEnv)

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