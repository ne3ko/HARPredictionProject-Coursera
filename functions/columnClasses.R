columnClasses <- function(data){
  #Create 2 lists for column names and column classes
  column <- c()
  class <- c()

  #Iterate through each column in the dataset, gathering the column names and
  #column classes, and add that data to each list
  for(i in 1:length(data)){
    column <- c(column,colnames(data)[i])
    class <- c(class,class(data[[i]]))
  }

  #Create a dataframe with two lists
  colclass <- data.frame(column, class)
  colnames(colclass) <- c("Variable", "Class")
  colclass
}