pollutantmean <- function(directory, pollutant, id) {
  setwd("C:\\Users\\djuka\\Desktop\\R course")
  specdata <- file(specdata)
  
  names <- list.files("specdata")
  
  file_name <- names[id] #inserts id value
  
  setwd("specdata") #inserts the directory name
  
  
  full_sum <- 0
  full_length <- 0
  for (i in file_name){
    data <- read.csv(i)
    datar <- data[complete.cases(data), ]
    full_sum <- full_sum + sum(datar[[pollutant]]) #inserts pollutant type
    full_length <- full_length + length(datar[[pollutant]]) #insert pollutant type here
  }
  
  full_sum / full_length
}