rankhospital <- function(state, outcome, rank){
  
  outcome_file <- read.csv("C:\\Users\\djuka\\Desktop\\Python\\Courses\\1 R Programming\\Module IV\\outcome-of-care-measures.csv", colClasses = "character")
  state_list <- unique(outcome_file$"State")
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% state_list) & !(outcome %in% outcome_list)){
    stop("This is not a correct state name nor outcome")
  } else if (!(outcome %in% outcome_list)){
    stop("This is not a correct outcome")
  } else if (!(state %in% state_list)){
    stop("This is not a correct state name")
  }
  
  
  split_state <- split(outcome_file, outcome_file$"State") #file loaded and splitted by states
  
  #required state selected only
  for(i in 1:54){
    if (sum(rowSums(state == split_state[[i]][7])) > 0)  {  
      selected_state <- split_state[[i]]  
    }
  }
  
  # Choose the outcome first
  if (outcome == "heart attack"){
    selected_state_outcome <- selected_state[[11]]
  } else if (outcome == "heart failure"){
    selected_state_outcome <- selected_state[[17]]
  } else if (outcome == "pneumonia"){
    selected_state_outcome <- selected_state[[23]]
  }
  
  
  hospital_names <- selected_state[[2]]
  #use the selected outcome
  outcome_values <- selected_state_outcome 
  hospital_outcome <- data.frame(hospital_names, outcome_values) 
  hospital_outcome_clean <- subset(hospital_outcome, outcome_values != "Not Available")
  
  #the code below will sort df by values, and then by alphabet if there is tie in two values. as.numeric is used to avoid problewhen sorting numbers
  sorted_cleaned <- hospital_outcome_clean[order(as.numeric(hospital_outcome_clean$outcome_values), decreasing = FALSE, hospital_outcome_clean$hospital_names), ]
  sorted_cleaned$Ranking <- c(1:nrow(hospital_outcome_clean)) #not useful, but added as example had it as well
  
  if (rank == "worst"){
    rank <- nrow(sorted_cleaned)
  } else if (rank == "best"){
    rank <- 1
  } else if (rank > nrow(sorted_cleaned)){
    print("NA")
  }  
  
  sorted_cleaned$hospital_names[rank]
  
}