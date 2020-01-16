getwd()
setwd("/Users/jonathanhudgins/Desktop/Section 3")
library(data.table)
terror_data <- data.frame(fread("terror_data.csv"))

# packages
#install.packages("ISLR")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("randomForest")

# Library
library(AER)
library(tidyr)
library(dplyr)
library(class)
library(FNN)
library(ISLR)
library(tree)
library(randomForest)

# Analyzing the Data:
table(terror_data$success)
prop.table(table(terror_data$success))

hist(terror_data$nkillus)
summary(terror_data$nkillus)
table(terror_data$nkillus)

# about 35% of the nkillus have NA's
length(which(is.na(terror_data$nkillus)==TRUE))/nrow(terror_data)

# Very simple dataset for practice purposes 
td_simple <- terror_data[c("success", "eventid", "iyear", "region", "crit1", "crit2", "crit3", 
                           "attacktype1", "targtype1",
                            "weaptype1", "nkill", "INT_LOG", "INT_IDEO")]
sapply(td_simple,class)

## Cleaning observations by rows by deleting "NA's"  
td_simple_no_na <- na.omit(td_simple)

# Split Training and Test 

set.seed(2019)
test_obs              <- round(0.2 * nrow(td_simple_no_na))
train_obs             <- nrow(td_simple_no_na) - test_obs
test_train_vec        <- c(rep("test", test_obs),
                           rep("train", train_obs))
#?sample
test_train_vec        <- sample(test_train_vec, nrow(td_simple_no_na), replace = FALSE)
test_data             <- td_simple_no_na[which(test_train_vec == "test"),]
train_data            <- td_simple_no_na[which(test_train_vec == "train"),]

# Random Forest on Success 

randomforest_success <- randomForest(as.factor(success)~., data= data.frame(td_simple_no_na), 
                                     na.action=na.exclude, do.trace=TRUE)
randomforest_success$confusion
head(randomforest_success$predicted)

# F Score Function 
f1_score <- function(predicted_y, true_y) {
  library(dplyr)
  num_unique_y      <- length(unique(true_y))
  scores            <- vector(length = num_unique_y, mode = "double")
  
  for (i in 1:num_unique_y) {
    trans_pred      <- as.numeric(predicted_y == i-1)
    trans_true      <- as.numeric(true_y == i-1)
    df              <- cbind.data.frame(trans_pred, trans_true)
    colnames(df)    <- c("pred", "true")
    df              <- df %>%
      mutate(true_pos = ((pred == 1) & (true == 1)),
             true_neg = ((pred == 0) & (true == 0)),
             false_pos = ((pred == 1) & (true == 0)),
             false_neg = ((pred == 0) & (true == 1))) %>%
      summarise(true_pos = sum(true_pos),
                false_pos = sum(false_pos),
                false_neg = sum(false_neg))
    scores[i]       <- 2 * df$true_pos / (2 * df$true_pos + 
                                            df$false_neg + 
                                            df$false_pos)
    
  }
  F1                <- mean(scores)
  return(F1)
}


# Compute F Score 
f1_score(randomforest_success$predicted, td_simple_no_na$success)
# F1 score = 0.75 


# Predict nkillus 
td_simple_nkillus <- terror_data[c("nkillus", "eventid", "iyear", "region", "crit1", "crit2", "crit3", "attacktype1", "targtype1",
                           "weaptype1", "nkill", "INT_LOG", "INT_IDEO")]
sapply(td_simple,class)

## Cleaning observations by rows by deleting "NA's"  
td_simple_nkillus_no_na <- na.omit(td_simple_nkillus)

# Deleting 9/11 Observations 
outliers <- which(td_simple_nkillus_no_na$nkillus>200)
td_nkillus_sans911 <- td_simple_nkillus_no_na[-outliers,]

# td_nkillus_sans911 <- td_simple_nkillus_no_na[!(td_simple_nkillus_no_na$nkillus %in% c(1360,1327,182,38)),]

# Split Training and Test
set.seed(2019)
test_obs              <- round(0.2 * nrow(td_nkillus_sans911))
train_obs             <- nrow(td_nkillus_sans911) - test_obs
test_train_vec        <- c(rep("test", test_obs),
                           rep("train", train_obs))
#?sample
test_train_vec        <- sample(test_train_vec, nrow(td_simple_no_na), replace = FALSE)
test_data             <- td_nkillus_sans911[which(test_train_vec == "test"),]
train_data            <- td_nkillus_sans911[which(test_train_vec == "train"),]


# Random Forest on nkillus 
randomforest_nkillus <- randomForest(nkillus~., data= data.frame(td_nkillus_sans911), 
                                     na.action=na.exclude, do.trace=TRUE)
randomforest_nkillus$confusion

rf.train <- randomForest(nkillus~., data=data.frame(train_data), 
                          importance =TRUE, do.trace=TRUE)

yhat.rf <- predict (rf.train , newdata=test_data)

# MSE
mean((yhat.rf -test_data$nkillus)^2)
# MSE = 3.73 

getwd()
setwd("/Users/jonathanhudgins/Desktop/Section 3")
library(data.table)
terror_data_test <- data.frame(fread("terror_data_test.csv"))[,-1]

# then you can make predictions using your trained model 
# using the 'psuedo' code below
my_predict_success <- predict(randomforest_success, newdata = terror_data_test)
length(my_predict_success)
# this will generate a vector of length 20506 values for success/failure

my_predict_nkillus <- predict(randomforest_nkillus, newdata = terror_data_test )
# this will generate a vector of length 20506 values for nkillus

# Round the decimals to create a binary classification 
my_roundpredsucces.rf <- as.numeric(my_predict_success)-1
my_roundprednkillus.rf <- round(my_predict_nkillus, digits = 0)

# making sure the "ID"s for each observation is in the original order
# 110567, 68091, 31069, 147129, 81065, 38049 
head(terror_data_test$eventid)

# combine the ID's with the two predictions you generated, save them
my_prediction_final <- data.frame(cbind(terror_data_test$eventid, my_roundpredsucces.rf, my_roundprednkillus.rf))
colnames(my_prediction_final) <- c("Id", "Prediction_1", "Prediction_2")
View(my_prediction_final)

# save the data
write.csv(my_prediction_final, "Hudgins_prediction.csv")

