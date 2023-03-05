  # in order for this code to work for you, you need to adjust directory locations
complete <- function(directory, id = 1:322) {  
  setwd("C:\\Users\\djuka\\Desktop\\R course") # location of folder
  
  specdata <- file(specdata)
  
  names <- list.files("specdata")
  
  file_name <- names[id] #insert id value here
  
  setwd("specdata") #insert the directory name here # go in folder
  
  all_count <- c()
  for (i in file_name){
    data <- read.csv(i)
    datar <- data[complete.cases(data), ]
    row_num <- nrow(datar)
    all_count = c(all_count, row_num)
  }
  
  
  final_vector <- data.frame(c(id), all_count)
  
  col_names <- colnames(final_vector)
  names(final_vector)[names(final_vector) == col_names[1]] <- "id"
  names(final_vector)[names(final_vector) == col_names[2]] <- "nobs"
  
  final_vector
  
}