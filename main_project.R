#LIBRARIES NEEDED
install.packages("data.table")
install.packages("mltools")
install.packages("dummies")
source("outlier.r")
library("stringr")
library(caret)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(boot)
library(xgboost)
library(dummies)

#LOADING DATASETS
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
View(train)
View(test)

#SHOWING MAIN FEATURES OF DATASETS
dim(train)
dim(test)

head(train)
head(test)

str(train)
str(test)

summary(train)
summary(test)

#REMOVE RESPONSE VARIABLE FROM TRAIN DATA AND MERGE WITH TEST DATA FOR DATA CLEANING
all <- rbind(train[, -c(length(train))], test)
dim(all)

######### Exploring
#SalePrice
ggplot(data=all[!is.na(all$SalePrice),], aes(x=train$SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

###################CorrMatrix###################

data_num <- all[, (sapply(all, class) == "integer")]
cor_numVar <- cor(data_num, use="pairwise.complete.obs") #correlations of all numeric variables


#Correlation Matrix
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


##################################################
##OverallQuality#########

ggplot(data=train, aes(x=factor(OverallQual), y=train$SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)



#DIVIDE NUMERICAL AND CATEGORICAL VARIABLES
data_cat <- all[, (sapply(all, class) != "integer")]

View(data_cat)
View(data_num)

#NAs
na_count_num <-sapply(data_num, function(y) sum(is.na(y)))
na_count_cat <-sapply(data_cat, function(y) sum(is.na(y)))
na_counter_num <- data.frame(na_count_num)
na_counter_cat <- data.frame(na_count_cat)
View(na_counter_num)
View(na_counter_cat)

print(sum(is.na.data.frame(all)))
all <- all[, -which(colMeans(is.na(all)) > 0.15)]
data_cat <- data_cat[, -which(colMeans(is.na(data_cat)) > 0.15)]
data_num <- data_num[, -which(colMeans(is.na(data_num)) > 0.15)]
print(sum(is.na.data.frame(all)))

# Replace NAs with "None"
print(sum(is.na.data.frame(all)))
all[is.na(all)] <- "None"
print(sum(is.na.data.frame(all)))

##################################################################################################################################

#DROP NUMERICAL PREDICTORS
drop <- c("KitchenAbvGr", "YrSold", "MoSold", "MiscVal", "ScreenPorch", "X3SsnPorch", "BsmtHalfBath", "LowQualFinSF","OverallCond", "EnclosedPorch", "MSSubClass", "X1stFlrSF", "YearBuilt", "YearRemodAdd", "BsmtFinSF2", "BsmtFinSF1", "BsmtUnfSF", "PoolArea", "GarageYrBlt", "GarageCond", "GarageArea", "TotRmsAbvGrd")
all <- all[,!(names(all) %in% drop )]

#DROP CATEGORICAL PREDICTORS
drop<- c("Street", "LandContour", "Utilities", "LandSlope", "Condition2", "RoofMatl", "BsmtFinType2", "Electrical", "Condition1", "BldgType", "HouseStyle", "Exterior1st", "Exterior2nd", "Foundation", "CentralAir", "Functional", "SaleType", "SaleCondition", "RoofStyle", "GarageQual")
all <- all[,!(names(all) %in% drop )]

#Non droppo neighborhood

#################################################################################################################################

#ENCODING
all[["ExterQual"]] <- as.numeric(factor(all[["ExterQual"]], levels=c("None","Po","Fa", "TA", "Gd", "Ex")))
all[["ExterCond"]] <- as.numeric(factor(all[["ExterCond"]], levels=c("None","Po","Fa", "TA", "Gd", "Ex")))
all[["HeatingQC"]] <- as.numeric(factor(all[["HeatingQC"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
all[["BsmtFinType1"]] <- as.numeric(factor(all[["BsmtFinType1"]], levels=c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")))
all[["BsmtCond"]] <- as.numeric(factor(all[["BsmtCond"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
all[["BsmtQual"]] <- as.numeric(factor(all[["BsmtQual"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
all[["KitchenQual"]] <- as.numeric(factor(all[["KitchenQual"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
all[["BsmtExposure"]] <- as.numeric(factor(all[["BsmtExposure"]], levels=c("None","No", "Mn", "Av", "Gd")))
all[["GarageFinish"]] <- as.numeric(factor(all[["GarageFinish"]], levels=c("None", "Unf", "RFn", "Fin")))

# Replace 0 with 
all$MasVnrArea[all$MasVnrArea=="None"]<-0

# Miss Correlation Matrix and we notice that ExterCond is very bad in terms of correl. and BsmtCond has 0 var.
drop <- c("ExterCond","BsmtCond")
all <- all[,!(names(all) %in% drop )]

#NAs
na_count <-sapply(all, function(y) sum(is.na(y)))
na_counter <- data.frame(na_count)
View(na_counter)
print(sum(is.na.data.frame(all)))


########################################################################################################


#OUTLIERS
# Update DataCat and DataNum 
data_num <- all[, (sapply(all, class) == "integer")]
data_cat <- all[, (sapply(all, class) != "integer")]
# define a function to remove outliers

boxplot(all$LotArea)
outlier(all$LotArea,method = "boxplot")
new_all <- new_replace_outliers(data_num)
boxplot(new_all$LotArea)

new_all <- cbind(new_all,data_cat)

##############################


data_processed_train = new_all[1:1100,]
data_processed_train = cbind(data_processed_train, train["SalePrice"])
data_processed_test = new_all[1101:dim(all)[1], ]

#num data train new
num_data_processed_train_y <- data_processed_train[, (sapply(data_processed_train, class) == "integer")]
head(num_data_processed_train_y)

#Drop SalePrice
drop <- c("SalePrice")
num_data_processed_train <- num_data_processed_train_y[,!(names(num_data_processed_train_y) %in% drop )]

################################################################################################################
#Linear Regression

#CV for random forest

set.seed(12345)
inTrain <- createDataPartition(y = data_processed_train$SalePrice, p = .7, list = FALSE)

data.train <- data_processed_train[inTrain, ]
data.test <- data_processed_train[- inTrain, ]

fit.control <- caret::trainControl(method = "cv", number = 10)

rf.fit <- caret::train(SalePrice ~ .,
                       data = data.train,
                       method = "rf",
                       trControl = fit.control)

min(rf.fit$results$RMSE) #BEST RMSE 
rf.fit$bestTune    #BEST NUMBER OF VARIABLES

##################################################################################################################
#CV for linear regression
fit.control <- caret::trainControl(method = "cv", number = 10)

lm.fit <- caret::train(SalePrice ~ .,
                       data = data.train,
                       method = "lm",
                       trControl = fit.control)

min(lm.fit$results$RMSE) #BEST RMSE 

##################################################################################################################

#CV for gboosting    **This will take a little longer than the other models**
fit.control <- caret::trainControl(method = "cv", number = 10)

gb.fit <- caret::train(SalePrice ~ .,
                       data = data.train,
                       method = "xgbDART",
                       trControl = fit.control)

min(gb.fit$results$RMSE) #BEST RMSE 
#28821.76 
##################################################################################################################



