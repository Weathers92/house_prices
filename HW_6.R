rm(list = ls())

library(data.table)
train <- fread("train.csv")
test <- fread("test.csv")

#Check for the missing variable in the test set.

trainVars <- colnames(train)
testVars <- colnames(test)

missingVar <- NULL
j = 1

for(i in 1:ncol(train))
{
 if(trainVars[i] %in% testVars){
 }else{
   missingVar[j] = trainVars[i]
   j = j+1
  }
}

missingVar
#SalePrice is in the training set but not in the test set.
#We want to be able to predict the sale price for the observations
#in the test set.

###################################
### DEALING WITH MISSING VALUES ###
###################################

#Determine where we are missing values.
#Creates a function that determines the percentage of missing values.
pMiss <- function(x){(sum(is.na(x)) / length(x)) * 100}

#Apply the above function to the columns of the training set.
(pMissCol <- apply(train, 2, pMiss))

#View the variables which have more than 5% missing values.
pMissCol[pMissCol > 5]
#Out of the 11 variables in this list, only one of them is a concern (LotFrontage).

#View the variables which have missing values, but only up to 5%.
pMissCol[0 < pMissCol & pMissCol < 5]
#Out of the 8 variables in this list, "MasVnrType", "MasVnrArea", "BsmtExposure", 
#"BsmtFinType2", and "Electrical" have missing values.

#Percentage of missing values for both "BsmtExposure" and "BsmtFinType2" is 0.06849315%.
#This is the same percentage as "Electrical".
percentMiss <- pMissCol[0 < pMissCol & pMissCol < 5][5] - pMissCol[0 < pMissCol & pMissCol < 5][4]

#There is only 1 row which has a basement but has a missing value for "BsmtExposure" and "BsmtFinType2".
nrow(train) * (percentMiss/100)

#This is the 949th observation.
train[which(!is.na(train$BsmtCond) & is.na(train$BsmtExposure))][[1]]

#The 1380th observations is the one row with a missing electrical value.
train[which(is.na(train$Electrical))][[1]]

#Before seeing how many missing values are in each row, we will want to recode
#the NA's in the variables which NA's should not be treated as missing values.



#
(pMissRow <- apply(train, 1, pMiss))
max(pMissRow)


library(mice)
md.pattern(train)
