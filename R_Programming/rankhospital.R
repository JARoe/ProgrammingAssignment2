## The rankhospital function ranks the hospitals in a state for a given outcome
## based on ascending mortality rate for that outcome, and returns the name of 
## the hospital with a specific ranking.
## state is the two letter uppercase abbreviation for a state.
## outcome is the health condtion of interest, it can be "heart attack", "heart failure", or "pneumonia"
## num is the ranking desired, possible inputs are "best", "worst", and a numerical value which corresponds to a specific ranking.
rankhospital <- function(state, outcome, num = "best") {
  
  # Tie conditioncol to appropriate column for given outcome
  codes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  conditioncol <- as.numeric(codes[outcome])
  
  # Check that state and outcome are valid
  if(!is.element(state, state.abb)){
    stop("invalid state")
    return(geterrmessage())
  }
  if(is.na(conditioncol)){
    stop("invalid outcome")
    return(geterrmessage())    
  }
  
  # Read outcome data
  data <- read.csv("/Users/jule-alisaroessler/Downloads/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",  colClasses = "character")
  suppressWarnings(data[,conditioncol] <- as.numeric(data[,conditioncol])) #warnings suppressed, coercing to numeric
  
  ## Return hospital name in that state with the given rank & 30-day death rate
  data <- subset(data, data[,7] == state)             #trims set for state
  data <- subset(data, !is.na(data[,conditioncol] == TRUE)) #cuts out na for condition
  data <- data[,c(2,conditioncol)]                    #cuts off everything but name and conditioncol
  names(data)[2] <- "Rate"                            #renames conditioncol to "Rate"
  data <- data[order(data$Rate, data$Hospital.Name),] #sorts by rate, then name
  #  data$Rank <- 1:nrow(data)                           #adds "Rank" column
  
  #return if statements
  if(num == "best"){     #first entry
    return(head(data$Hospital,1))
  }
  if(num == "worst"){    #last entry
    return(tail(data$Hospital.Name,1))
  } 
  if(as.numeric(num) > nrow(data)){   #asking for nonexistent index
    return(NA)
  } else {               #normal return
    return(data[num,1])
  }
  
}


rankhospital("NC", "heart attack", "worst")

rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)


rankhospital("NY", "heart attack", 7)
