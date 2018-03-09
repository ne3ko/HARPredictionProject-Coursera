RemoveNAColumns <- function(old_df){
  #First we create a "mock" training data set for manipulation.  
  new_df <- old_df
  
  #Build a for-loop that iterates through the "training" dataset
  for(i in 1:length(old_df)){
    #If the specified column in the "training" data set is more than 66% NA,
    #then delete the column from the "new_train" dataset
    if(sum(is.na(old_df[[i]]))/length(old_df[[i]]) >= 0.66){
      new_df <- new_df[,-which(names(new_df) %in% colnames(old_df)[i])]
    }
  }
  
  #return the "new_train" dataset, as that is the dataset we manipulated.
  new_df
}