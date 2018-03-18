library(e1071)
library(rpart)
library(ggplot2)
library(lattice)
library(caret)
library(data.table)

#Getting the Data
train_dat= fread("balanced_data_new.csv")
test_dat = fread("test.csv")


####### PRE-PROCESSING OF TEST DATA  ########


#Getting the column names from train data
reqd_col = colnames(train_dat)

#Storing the test id for future use
t_id = as.integer(as.numeric(test_dat$id))
test_dat$id = NULL

#Removing the target column from the  list 
reqd_col = reqd_col[2:length(reqd_col)]

#Creating the test data
test_dat = subset(test_dat, select = reqd_col)

###########   HANDLING THE MISSING VALUES   ############ 

#Here -1 corresponds to missing values

#Getting the columns containing the missing values
col_miss=colSums(test_dat == -1)
col_miss_nam = names(col_miss[col_miss>0])


#Mode calculation
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Getting the mode for each column
mode_val = numeric(length(col_miss_nam))
mode_val[1] = Mode(test_dat$ps_ind_05_cat)
mode_val[2] = mean(test_dat$ps_reg_03[which(test_dat$ps_reg_03 != -1)])
mode_val[3] = Mode(test_dat$ps_car_01_cat)
mode_val[4] = Mode(test_dat$ps_car_07_cat)
mode_val[5] = mean(test_dat$ps_car_11[which(test_dat$ps_car_11 != -1)])
mode_val[6] = mean(test_dat$ps_car_14[which(test_dat$ps_car_14 != -1)])

#Replacing the missing values with the mode or mean(for continuous values) of the column 
test_dat[which(test_dat$ps_ind_05_cat == -1)] = mode_val[1]
test_dat[which(test_dat$ps_reg_03 == -1)] = mode_val[2]
test_dat[which(test_dat$ps_car_01_cat == -1)] = mode_val[3]
test_dat[which(test_dat$ps_car_07_cat == -1)] = mode_val[4]
test_dat[which(test_dat$ps_car_11 == -1)] = mode_val[5]
test_dat[which(test_dat$ps_car_14 == -1)] = mode_val[6]

#Uncomment for saving the files after pre-processing 
# 
# fwrite(test_dat, "final_test.csv")
# fwrite(train_dat, "final_train.csv")


######  CROSS VALIDATION #########
trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
set.seed(12)


#######  TRAINING THE SVM MODEL  #########
svm_model= train(target ~.-X, data = train_dat, method = "svmLinear",
                  trControl=trctrl,
                  tuneLength = 10)

#Using the model for prediction 
prediction= predict( svm_model, newdata = test_dat)
print(prediction)

#Creating the submission file for Kaggle
submission = cbind(t_id, prediction)
fwrite(submission, "SVM_submission.csv")



#Uncomment the below code if you want to test the accuracy of the model

#### OPTIONAL SECTION  ######

#test_dat = fread("balanced_test_new.csv")
#tar = test_dat$target
#tar = as.factor(tar)

#test_dat$target = NULL

#Cross validation
#trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
#set.seed(12)

#Training the model
#svm_model= train(target ~. , data = train_dat, method = "svmLinear",trControl=trctrl,tuneLength = 10)

#Using the model for prediction 
#prediction= predict( svm_model, newdata = test_dat)
#print(prediction)

#sum(prediction == test_dat$target)/length(prediction)    

