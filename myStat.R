# library(png)
# library(grid)
# library(gridExtra)
# 
# ROOT <- "/Users/mikkeldanielsen/myStat/"
# setwd(paste(ROOT, "functions", sep="/"))
# file.sources = list.files(pattern="*.R")
# sapply(file.sources, source, .GlobalEnv)
# 
# printImages <- function(files, nRow, nCol){
#   
#   # Add images to list
#   imgs <- list()
#   for(file in files){
#     img <- rasterGrob(readPNG(paste(ROOT, file, sep="")))
#     imgs <- c(imgs, list(img))
#   }
#   
#   # Print images
#   do.call("grid.arrange", c(imgs, nrow=nRow, ncol=nCol))
# }
# 
# listFunctions <- function(filename) {
#   temp.env <- new.env()
#   sys.source(filename, envir = temp.env)
#   functions <- lsf.str(envir=temp.env)
#   rm(temp.env)
#   return(functions)
# }
# 
# printFunctions <- function(){
#   for(file in file.sources){
#     for(func in listFunctions(file))
#       print(func)
#   }
# }
# 
# printFunctions()
#  
# funcs = listFunctions("anova.R")
print(funcs)
