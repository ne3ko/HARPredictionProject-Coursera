###PACKAGES
library(caret)
library(randomForest)

###READ DATA
## Read the data from pml-testing.csv to be used for the prediction algorithm
data <- read.csv(file = "data/pml-training.csv", header = TRUE)
validation <- read.csv(file = "data/pml-testing.csv", header = TRUE)

  
##Partition data into training & testing
inTrain <- createDataPartition(y=data$classe, p=0.75, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#Set the overall seed for reproducibility
set.seed(2018)


###DATA CLEANING
##Remove the useless data
training <- training[,8:160]
    
## Convert all non-values in training set to NA
training[training == "NA"] <- NA
training[training == ""] <- NA
training[training == "#DIV/0!"] <- NA

##Remove all columns with over 66% NA values
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
training <- RemoveNAColumns(training)

##Check to find all Near-Zero Covariates to get rid of all predictors with little-to-no variability in them.
nzv <- nearZeroVar(training, saveMetrics = TRUE)
summary(nzv)
#All of the variables have variablity

##Lastly, remove all the highly correlated
#Finds the absolute value of the correlation of all variables besides the "classe" variable
corr <- abs(cor(training[,-53]))
#Searches through the correlation matrix ("coor") and tells you which variables
#to remove in order to reduce pair-wise correlations
corr_var <- findCorrelation(corr, 0.95, verbose = TRUE)
#Columns 10, 1, 8, and 33 are flagged for correlation over 95%, therefore we can remove them from the dataset.
training <- training[,-corr_var]

##Change the class of each number to "numeric" and everything else to "factor" 
formatClass <- function(df){
  #Iterate through each column in the data frame, and set all numbers to
  #"numeric" and everything else to "factor".
  for(i in 1:length(df)){
    if(class(df[[i]]) == "numeric" || class(df[[i]]) == "integer"){
      df[[i]] <- as.numeric(df[[i]])
    }
    else{
      df[[i]] <- as.factor(df[[i]])
    }
  }
  
  df
}
training <- formatClass(training)

##Check to see if there are any other NA values we need to consider.
sum(is.na(training))

###PREDICTION
##Random Forest
#First need to build the model
rf_model <- randomForest(classe ~ ., data = training)
rf_model

#Now we can use this model to predict the test set
rf_predict <- predict(rf_model,testing)##
rf_cm <- confusionMatrix(rf_predict,testing$classe)
rf_cm

#Lastly, now that we see our prediction model works, we can use it to predict
#the final set of data
validation <- read.csv(file = "data/pml-testing.csv", header = TRUE)
final_predict <- predict(rf_model, validation)
final_predict