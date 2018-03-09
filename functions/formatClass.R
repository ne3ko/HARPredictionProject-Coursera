formatClass <- function(data){
  #Iterate through each column in the data frame, and set all numbers to
  #"numeric" and everything else to "factor".
  for(i in 1:length(data)){
    if(class(data[[i]]) == "numeric" || class(data[[i]]) == "integer"){
      data[[i]] <- as.numeric(data[[i]])
    }
    else{
      data[[i]] <- as.factor(data[[i]])
    }
  }
  
  data
}