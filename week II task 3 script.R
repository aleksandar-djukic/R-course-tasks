# in order for this code to work for you, you need to adjust directory locations
corr <- function(directory, treshold=0) {  
  setwd("C:\\Users\\djuka\\Desktop\\R course")
  
  specdata <- file("specdata")
  
  names <- list.files("specdata")
  
  file_name <- names[1:322] #insert id value here
  
  setwd("specdata") #insert the directory name here
  
  
  d <- data.frame()
  for (i in file_name){
    data <- read.csv(i)
    
    d <- rbind(d, data)
  }
  
  split_ID <-split(d, d$ID)
  
  
  final_cors <- c()
  for (i in 1:322){
    n <- nrow(split_ID[[i]][complete.cases(split_ID[[i]]), ])
    if (n > treshold){ 
      refined_df <- split_ID[[i]][complete.cases(split_ID[[i]]), ]
      sul_ni <- refined_df[2:3]
      cor <- cor(sul_ni[1], sul_ni[2])
      final_cors <- c(final_cors, cor)
    }
  }
  
  exam <- head(final_cors)
  exam
  
}