best <- function(state, outcome) {
  
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
  
  # Return hospital name in that state with lowest 30-day death rate
  data <- subset(data, data[,7] == state)
  minval <- min(data[,conditioncol], na.rm=TRUE)
  data[which(data[,conditioncol] == minval),2]
}

best("SC", "heart attack")

best("NY", "pneumonia")


best("AK", "pneumonia")
