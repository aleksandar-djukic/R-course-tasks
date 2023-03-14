#now me make func of my code!!

best <- function(state, outcome){
  
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
  
  
  if (outcome == "heart attack") {
    selected_state[, 11] <- as.numeric(selected_state[, 11])
    true_min <- min(selected_state[, 11], na.rm=T) #finds min value in one state
    
    val_position <- c(0)
    val_pos <- c(0)
    for(i in selected_state[, 11]){
      val_position <- val_position + 1
      if (is.na(i) == TRUE) {
        next
      } else if (i == true_min) {
        val_pos <- c(val_pos, val_position)  #goal is to get possitions of all min values
      }
    }
    
    val_pos <- val_pos[-1] # removes 0 that is created by initiating empty vector
    
    
    state_names <- list()
    for (i in val_pos){
      state_names <- append(state_names, selected_state[i, 2]) # create a list of hospitals with min rate
    }
    
    hostitals_position <- data.frame(unlist(state_names), val_pos) # join hospital name with its position in CSV
    colnames(hostitals_position) <- c("Hospital", "ID") #make logical column names
    
    
    sorted_hospitals <- hostitals_position[order(hostitals_position$Hospital),] #df is now sorted alphabetically
    
    sorted_hospitals[1,1]
  }
  else if (outcome == "heart failure") {
    selected_state[, 17] <- as.numeric(selected_state[, 17])
    true_min <- min(selected_state[, 17], na.rm=T) #finds min value in one state
    
    val_position <- c(0)
    val_pos <- c(0)
    for(i in selected_state[, 17]){
      val_position <- val_position + 1
      if (is.na(i) == TRUE) {
        next
      } else if (i == true_min) {
        val_pos <- c(val_pos, val_position)  #goal is to get possitions of all min values
      }
    }
    
    val_pos <- val_pos[-1] # removes 0 that is created by initiating empty vector
    
    
    state_names <- list()
    for (i in val_pos){
      state_names <- append(state_names, selected_state[i, 2]) # create a list of hospitals with min rate
    }
    
    hostitals_position <- data.frame(unlist(state_names), val_pos) # join hospital name with its position in CSV
    colnames(hostitals_position) <- c("Hospital", "ID") #make logical column names
    
    
    sorted_hospitals <- hostitals_position[order(hostitals_position$Hospital),] #df is now sorted alphabetically
    
    sorted_hospitals[1,1]
  }
  else if (outcome == "pneumonia") {
    selected_state[, 23] <- as.numeric(selected_state[, 23])
    true_min <- min(selected_state[, 23], na.rm=T) #finds min value in one state
    
    val_position <- c(0)
    val_pos <- c(0)
    for(i in selected_state[, 23]){
      val_position <- val_position + 1
      if (is.na(i) == TRUE) {
        next
      } else if (i == true_min) {
        val_pos <- c(val_pos, val_position)  #goal is to get possitions of all min values
      }
    }
    
    val_pos <- val_pos[-1] # removes 0 that is created by initiating empty vector
    
    
    state_names <- list()
    for (i in val_pos){
      state_names <- append(state_names, selected_state[i, 2]) # create a list of hospitals with min rate
    }
    
    hostitals_position <- data.frame(unlist(state_names), val_pos) # join hospital name with its position in CSV
    colnames(hostitals_position) <- c("Hospital", "ID") #make logical column names
    
    
    sorted_hospitals <- hostitals_position[order(hostitals_position$Hospital),] #df is now sorted alphabetically
    
    sorted_hospitals[1,1]
  }
  
}