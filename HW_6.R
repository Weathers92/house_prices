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

################################
### SETTING UP THE TEST DATA ###
################################
#Apply the above function to the columns of the training set.
(pMissCol2 <- apply(test, 2, pMiss))

#Variables we need to impute for: MSZoning(C), LotFrontage, Utilities(C), Exterior1st(C), 
#Exterior2nd(C), MasVnrType, MasVnrArea, 

(below5 <- pMissCol2[0 < pMissCol2 & pMissCol2 <= 5])
(above5 <- pMissCol2[pMissCol2 > 5])

index3 <- test[which(!is.na(test$MasVnrArea) & is.na(test$MasVnrType))][[1]] - (nrow(test) + 1)

index4 <- test[which(!is.na(test$BsmtExposure) & is.na(test$BsmtCond))][[1]] - (nrow(test) + 1)

index5 <- test[which(is.na(test$BsmtFinSF1))][[1]] - (nrow(test) + 1)

index6 <- test[which(!is.na(test$BsmtFinSF1) & is.na(test$BsmtFullBath))][[1]] - (nrow(test) + 1)

index7 <- test[which(!is.na(test$GarageType) & is.na(test$GarageYrBlt) & is.na(test$GarageFinish) & is.na(test$GarageQual)& is.na(test$GarageCond))][[1]] - (nrow(test) +1)

test$Alley[is.na(test$Alley)] <- "none"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "none"
test$GarageType[is.na(test$GarageType)] <- "none"
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$GarageFinish[is.na(test$GarageFinish)] <- "none"
test$GarageQual[is.na(test$GarageQual)] <- "none"
test$GarageCond[is.na(test$GarageCond)] <- "none"
test$PoolQC[is.na(test$PoolQC)] <- "none"
test$Fence[is.na(test$Fence)] <- "none"
test$MiscFeature[is.na(test$MiscFeature)] <- "none"
test$BsmtQual[is.na(test$BsmtQual)] <- "none"
test$BsmtCond[is.na(test$BsmtCond)] <- "none"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "none"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "none"
test$Utilities[is.na(test$Utilities)] <- "none"
test$Exterior1st[is.na(test$Exterior1st)] <- "none"
test$Exterior2nd[is.na(test$Exterior2nd)] <- "none"
test$MasVnrType[is.na(test$MasVnrType)] <- "none"
test$MasVnrArea[is.na(test$MasVnrArea)] <- "none"
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtExposure[is.na(test$BsmtExposure)] <- "none"

test$GarageYrBlt[667] <- NA
test$GarageYrBlt[1117] <- NA
test$GarageFinish[667] <- NA
test$GarageFinish[1117] <- NA
test$GarageQual[667] <- NA
test$GarageQual[1117] <- NA
test$GarageCond[667] <- NA
test$GarageCond[1117] <- NA

test$MasVnrType[index3] <- NA

test$BsmtCond[index4[1]] <- NA
test$BsmtCond[index4[2]] <- NA
test$BsmtCond[index4[3]] <- NA

#Change the variables of class "character" to class "factor".
for(i in 1:ncol(test))
{
  if(class(test[[i]]) == "character")
  {
    test[[i]] <- as.factor(test[[i]])
  }
}

train$GarageYrBlt = as.numeric(train$GarageYrBlt)

##########################################
### IMPUTE MISSING VALUES FOR TEST SET ###
##########################################

#Imputed the missing values in the train data using random forests.

imputeTrain2 <- mice(test, m = 5, method = 'rf', seed = 0692)

completedTrain2 <- complete(imputeTrain2, 1)


