#xgboost implemented on selected 34 features obtained using random forest and output is taken 
#Saniya Ambavanekar

library(caret)
library(data.table)
library(xgboost)

xtrain_data<-fread("balanced_data_3.csv",stringsAsFactors = T)
View(xtrain_data)
str(xtrain_data)

smpl_size<-floor(0.75*nrow(xtrain_data))
set.seed(123)
indx <- sample(seq_len(nrow(xtrain_data)), size = smpl_size)

xtrain <- as.data.frame(xtrain_data[indx, ])
xtest <- as.data.frame(xtrain_data[-indx, ])

#Removind id and saving the target
xtrain$V1<-NULL
xtrain$id<-NULL
xtrain_target<-xtrain$target
xtrain$target<-NULL
xtrain$ps_car_11_cat<-NULL

#For test data
xtest$V1<-NULL
xtest$id<-NULL
xtest_target<-xtest$target
xtest$target<-NULL
xtest$ps_car_11_cat<-NULL


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

#Implementing xgboost
xgb1 <- xgb.train (params = params, data =dtrain,nrounds = 500, 
                   watchlist = list(val=dtest,train=dtrain), print_every_n  = 10, 
                   early_stop_round = 10, maximize = F , eval_metric = "error")

#Confusion Matrix
xgbpred<-predict(xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
xgbconf<-confusionMatrix(xgbpred,xtest_target)
print(xgbconf)

#Predicitng for real test dataset
realtest<-fread("test_new.csv",stringsAsFactors = T)
View(realtest)
str(realtest)
realtest<-as.data.frame(realtest)
realtest<-data.matrix(realtest)

realtest<-xgb.DMatrix(realtest)
xgbpred1<-predict(xgb1,realtest)
xgbpred1

xgbpred2<-round(xgbpred1,3)

#writing the submission file
testid<-read.csv("test_id.csv",header=T)
View(testid)
final_xg<-cbind(testid$x,xgbpred1)
colnames(final_xg)<-c("id","target")
write.csv(final_xg,"xgsubmission1.csv",row.names = F)
