rankall <- function(outcome, rank="best"){
  
  #This function first sorts values in selected column ("heart attack", "heart failure", "pneumonia") per country, 
  #and on request, returns one hospital name per state of requested rank, e.g., rank 1 gives best hospital per state
  
  outcome_file <- read.csv("C:\\Users\\djuka\\Desktop\\Python\\Courses\\1 R Programming\\Module IV\\outcome-of-care-measures.csv", colClasses = "character")
  state_list <- unique(outcome_file$"State")
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  
  
  split_state <- split(outcome_file, outcome_file$"State") #file loaded and split by states
  
  
  #ordering done within each state; if statement used to select column for chosen outcome
  df <- data.frame()
  for (i in split_state){
    if (outcome == "heart attack"){
      i <- subset(i, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
      i <- i[order(as.numeric(i$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), decreasing = FALSE, i$Hospital.Name), ]
      df <- rbind(df, i)
    } else if (outcome == "heart failure"){
      i <- subset(i, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
      i <- i[order(as.numeric(i$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), decreasing = FALSE, i$Hospital.Name), ]
      df <- rbind(df, i)
    } else if (outcome == "pneumonia"){
      i <- subset(i, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
      i <- i[order(as.numeric(i$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), decreasing = FALSE, i$Hospital.Name), ]
      df <- rbind(df, i)
    }
  }
  
  # now we can split reordered data by states again
  split_state_II <- split(df, df$"State") #ordered file split by states
  

  
  df <- data.frame() 
  for (i in split_state_II){
    
    if (rank == "best"){
      rank <- 1
      request <- i[rank, ]  
      df <- rbind(df, request)
    } else if (rank == "worst") {
      print(nrow(i))
      rank <- nrow(i)
      request <- i[rank, ]   
      df <- rbind(df, request)
    } else {
      request <- i[rank, ]   
      df <- rbind(df, request)
    }
}

  data <- data.frame(df[2], df[7], df[23]) #last DF column needs to be changed by hand now, DELET LATER
  data
}