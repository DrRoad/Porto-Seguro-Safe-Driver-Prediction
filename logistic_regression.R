library(caTools)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)


#Getting the Data
train_dat= fread("balanced_data_new.csv", stringsAsFactors = T)
names(train_dat)


####### PRE-PROCESSING OF TEST DATA ########

#Putting the names from train data into array
reqd_col = colnames(train_dat)

#Getting the test data
test_dat = fread("test.csv" , stringsAsFactors = T)
cat_col = grep("_cat", names(test_dat), value = T)
bin_col = grep("_bin", names(test_dat), value = T)

#Converting the data into required datatype
test_dat %<>% mutate_at(bin_col, funs(factor(.)))
test_dat %<>% mutate_at(cat_col, funs(factor(.)))

#Storing the id for future use
t_id = as.integer(as.numeric(test_dat$id))

#Removing id 
test_dat$id = NULL

#Getting the same columns as train data
reqd_col = reqd_col[2:length(reqd_col)]
test_dat = subset(test_dat, select = reqd_col)

###########   HANDLING THE MISSING VALUES   ############ 

#(Here -1 corresponds to missing values)

#Getting the columns containing the missing values 
col_miss=colSums(test_dat == -1)
col_miss_nam = names(col_miss[col_miss>0])

#Mode calculation
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Replacing the missing values with the mode or mean(for continuous data) of the column
mode_val = numeric(length(col_miss_nam))
mode_val[1] = Mode(test_dat$ps_ind_05_cat)
mode_val[2] = mean(test_dat$ps_reg_03[which(test_dat$ps_reg_03 != -1)])
mode_val[3] = Mode(test_dat$ps_car_01_cat)
mode_val[4] = Mode(test_dat$ps_car_07_cat)
mode_val[5] = mean(test_dat$ps_car_11[which(test_dat$ps_car_11 != -1)])
mode_val[6] = mean(test_dat$ps_car_14[which(test_dat$ps_car_14 != -1)])

test_dat[which(test_dat$ps_ind_05_cat == -1)] = mode_val[1]
test_dat[which(test_dat$ps_reg_03 == -1)] = mode_val[2]
test_dat[which(test_dat$ps_car_01_cat == -1)] = mode_val[3]
test_dat[which(test_dat$ps_car_07_cat == -1)] = mode_val[4]
test_dat[which(test_dat$ps_car_11 == -1)] = mode_val[5]
test_dat[which(test_dat$ps_car_14 == -1)] = mode_val[6]

###### CREATING THE MODEL ######

#Training the Model
log_model= glm (target ~ ., data = train_dat, family = binomial)

#Prediction using the  model 
prediction_lr= predict(log_model, newdata = test_dat, type = "response")
print(prediction_lr)


#Creating the submission file 
submission = cbind(t_id, prediction_lr)
submission = as.data.frame(submission)
names(submission)= c("id", "target")
fwrite(submission , "LR_submission_file.csv")



#Uncomment the below code if you want to test the accuracy of the model

#### OPTIONAL SECTION  ######

#test_dat = fread("balanced_test_new.csv", stringAsFactors = T)
#test_id = test_dat$id

#tar = test_dat$target
#tar = as.factor = tar
#test_dat$target = NULL

#Training the Model
#log_model= glm (target ~ ., data = train_dat, family = binomial)

#Prediction using the  model 
#prediction_lr= predict(log_model, newdata = test_dat, type = "response")
#print(prediction_lr)

#Testing the accuracy of the model
# new_lab= numeric(length(prediction_lr))
# new_lab = ifelse(prediction_lr>median(prediction_lr),1,0)
# sum(new_lab == tar)/length(prediction_lr) #Gives the accuracy 
# new_lab


