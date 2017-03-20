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

#To make sure that GarageYrBlt is only missing where people don't have garages
#we will make sure the missing values in both GarageYrBlt and GarageType are found
#in the same observations.
count <- length(train[which(is.na(train$GarageType) & is.na(train$GarageYrBlt))])
(percentMiss1 <- count / nrow(train) * 100)
#Since the percentage of missing values for GarageYrBlt is the same as the percentage of 
#missing values where both GarageType and GarageYrBlt are missing at the same time, GarageYrBlt
#is missing its values in the same observations as GarageType. Namely where individuals don't
#have a garage.

#View the variables which have missing values, but only up to 5%.
pMissCol[0 < pMissCol & pMissCol < 5]
#Out of the 8 variables in this list, "MasVnrType", "MasVnrArea", "BsmtExposure", 
#"BsmtFinType2", and "Electrical" have missing values.

#Percentage of missing values for both "BsmtExposure" and "BsmtFinType2" is 0.06849315%.
#This is the same percentage as "Electrical".
percentMiss2 <- pMissCol[0 < pMissCol & pMissCol < 5][5] - pMissCol[0 < pMissCol & pMissCol < 5][4]

#There is only 1 row which has a basement but has a missing value for "BsmtExposure" and "BsmtFinType2".
nrow(train) * (percentMiss2/100)

#This is the 949th observation.
index1 <- train[which(!is.na(train$BsmtCond) & is.na(train$BsmtExposure))][[1]]

#The 1380th observations is the one row with a missing electrical value.
index2 <- train[which(is.na(train$Electrical))][[1]]

#Before seeing how many missing values are in each row, we will want to recode
#the NA's in the variables which NA's should not be treated as missing values.

train$Alley[is.na(train$Alley)] <- "none"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "none"
train$GarageType[is.na(train$GarageType)] <- "none"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$GarageFinish[is.na(train$GarageFinish)] <- "none"
train$GarageQual[is.na(train$GarageQual)] <- "none"
train$GarageCond[is.na(train$GarageCond)] <- "none"
train$PoolQC[is.na(train$PoolQC)] <- "none"
train$Fence[is.na(train$Fence)] <- "none"
train$MiscFeature[is.na(train$MiscFeature)] <- "none"
train$BsmtQual[is.na(train$BsmtQual)] <- "none"
train$BsmtCond[is.na(train$BsmtCond)] <- "none"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "none"

train$BsmtExposure[is.na(train$BsmtExposure)] <- "none"
train$BsmtExposure[[index1]] <- NA

train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "none"
train$BsmtFinType2[[index1]] <- NA

#Change the variables of class "character" to class "factor".
for(i in 1:ncol(train))
{
  if(class(train[[i]]) == "character")
  {
    train[[i]] <- as.factor(train[[i]])
  }
}

train$GarageYrBlt = as.numeric(train$GarageYrBlt)

#Apply the above function to the columns of the training set.
(pMissCol <- apply(train, 2, pMiss))

#View the variables which have more than 5% missing values.
pMissCol[pMissCol > 5]
#Now only LotFrontage shows up, as we wanted.

#View the variables which have missing values, but only up to 5%.
pMissCol[0 < pMissCol & pMissCol < 5]
#Now only the correct variables show up, namely, "MasVnrType", "MasVnrArea", 
#"BsmtExposure", "BsmtFinType2", and "Electrical" have missing values.
#We have a total of 6 variables we need to impute for.

#################################################
### RENAME VARIABLES THAT START WITH INTEGERS ###
#################################################

#Find the predictors whos names start with numbers
change <- grep('^[[:digit:]]', names(train), value = TRUE)

#Use these names to rename the 3 predictors that started with numbers
replacement <- c("sfFlr1", "sfFlr2", "sfPorch3s")

#For the three predictors that start with numbers, rename them with the new names we've created.
for(i in 1:length(change))
{
  names(train) <- gsub(change[i], replacement[i], x = names(train), fixed = TRUE)
}

#############################
### IMPUTE MISSING VALUES ###
#############################

#Imputed the missing values in the train data using random forests.
library(mice)
imputeTrain <- mice(train, m = 5, method = 'rf', seed = 0692)

completedTrain <- complete(imputeTrain, 1)

############################################
### LAUREN'S CODE FOR VARIABLE SELECTION ###
############################################

library(randomForest)
price.rf <- randomForest(SalePrice ~ ., importance = TRUE, data = completedTrain[, 2:81])
varImpPlot(price.rf, scale = FALSE)

