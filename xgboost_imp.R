#xgboost to check the importance of features 
#Saniya Ambavanekar

library(caret)
library(data.table)
library(xgboost)

xtrain_data<-fread("cb_dataset.csv",stringsAsFactors = T)
View(xtrain_data)

str(xtrain_data)

smpl_size<-floor(0.75*nrow(xtrain_data))
set.seed(123)
indx <- sample(seq_len(nrow(xtrain_data)), size = smpl_size)

xtrain <- as.data.frame(xtrain_data[indx, ])
xtest <- as.data.frame(xtrain_data[-indx, ])

#Removing and saving the target


xtrain_target<-xtrain$target
xtrain$target<-NULL
#xtrain$ps_car_11_cat<-NULL

#For test data

xtest_target<-xtest$target
xtest$target<-NULL
#xtest$ps_car_11_cat<-NULL


#Getting the features which are categorical
colnames_cat<-grep("_cat", names(xtrain), value=TRUE)
colnames_bin<-grep("_bin",names(xtrain),value=TRUE)
colnames_cat
colnames_bin

#Converting target and categorical to numeric for xgboost
for(i in 1:length(colnames_cat))
{
  
  vec<-(xtrain[,colnames_cat[i]])
  vec<-as.numeric(levels(vec))[vec]
  xtrain[,colnames_cat[i]]<-vec
  
}

for(i in 1:length(colnames_bin))
{
  vec<-(xtrain[,colnames_bin[i]])
  vec<-as.numeric(levels(vec))[vec]
  xtrain[,colnames_bin[i]]<-vec
}


str(xtrain)

#Converting target to num
xtrain_target<-as.numeric(levels(xtrain_target))[xtrain_target]
xtest_target<-as.numeric(levels(xtest_target))[xtest_target]

#Applying xgboost
xtrain<-data.table(xtrain)
xtest<-data.table(xtest)
#dtrain<-xgb.DMatrix(xtrain,labels=xtrain_target)
#dtest<-xgb.DMatrix(xtest,labels=xtest_target)
dtrain<-data.matrix(xtrain)
dtest<-data.matrix(xtest)

#Selecting appropriate params

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
               gamma=5, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)





dtrain<-xgb.DMatrix(dtrain,label=xtrain_target)
dtest<-xgb.DMatrix(dtest,label=xtest_target)


#Cross Validation xgboost

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T, print_every_n = 10, 
                 early_stopping_rounds = 20, maximize = F)

min_test_error<-min(xgbcv$evaluation_log$test_error_mean)
xgbcv$best_iteration

#Implementing xgboost
xgb1 <- xgb.train (params = params, data =dtrain,nrounds = 200, 
                   watchlist = list(val=dtest,train=dtrain), print_every_n  = 10, 
                   early_stop_round = 10, maximize = F , eval_metric = "error")


m <- xgb.importance (feature_names = colnames(xtrain),model = xgb1)
xgb.plot.importance (importance_matrix = m[1:34]) 
