#Data Preprocessing and Implementation of Random Forest
#Final Random Forest implemented on entire train and test dataset
#Saniya Ambavanekar

library(ROSE)

rtrain<-fread("train.csv",stringsAsFactors = T)
#View(rtrain)
#str(rtrain)

rtrain[rtrain==(-1)]<-NA
sum(is.na(rtrain))

sapply(rtrain,function(x) sum(is.na(x)))

#Removing Columns containing max NAs
rtrain<-rtrain[,-c("id","ps_car_05_cat","ps_car_03_cat","ps_reg_03")]

rtrain<-as.data.frame(rtrain)
colnames_cat<-grep("_cat", names(rtrain), value=TRUE)
colnames_cat


#converting to factor categories
for(i in 1:length(colnames_cat))
{
  rtrain[,colnames_cat[i]]<-as.factor(rtrain[,colnames_cat[i]])
}
str(rtrain)

#converting to factor binary
colnames_bin<-grep("_bin", names(rtrain), value=TRUE)
colnames_bin

for(i in 1:length(colnames_bin))
{
  rtrain[,colnames_bin[i]]<-as.factor(rtrain[,colnames_bin[i]])
}
str(rtrain)

#Replacing NA with mode and mean for train data

Mode <- function(x) 
{
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#Mode for categorical variable
for(i in 1:length(colnames_cat))
{
  vec<-rtrain[,colnames_cat[i]]
  rownum<-which(is.na(vec))
  nm<-Mode(vec[-rownum])
  vec[rownum]<-nm
  rtrain[,colnames_cat[i]]<-vec
}

#Model for binary variable
for(i in 1:length(colnames_bin))
{
  vec<-rtrain[,colnames_bin[1]]
  rownum<-which(is.na(vec))
  nm<-Mode(vec[-rownum])
  vec[rownum]<-nm
  rtrain[,colnames_bin[i]]<-vec
}


rtrain$target<-as.factor(rtrain$target)

#mean 
cat_cols<-c(colnames_bin,colnames_cat)
numcols<-sapply(rtrain,is.numeric)
nums<-names(which(numcols==TRUE))
for(i in 1:length(nums))
{
  vec<-rtrain[,nums[i]]
  rownum<-which(is.na(vec))
  nm<-vec[-rownum]
  avg<-mean(nm)
  vec[rownum]<-avg
  rtrain[,nums[i]]<-vec
}
#verify removal of NA
sum(is.na(rtrain))

#Balancing the dataset
rtrain_bal<-ovun.sample(target~.,data=rtrain,method="both",seed=123)$data
str(rtrain_bal)
rtrain_bal$ps_car_11_cat<-NULL

#convert target to factor
rtrain_bal_target<-rtrain_bal$target
rtrain_bal$target<-NULL



#prediction using random forest 
reg<-randomForest(x=rtrain_bal,y=rtrain_bal_target)

#Predicting 
rt<-fread("test_cleaned.csv",header = T,stringsAsFactors = T)
View(rt)
rt$ps_car_11_cat<-NULL
rpred<-predict(reg,rt)
rpred<-predict(reg,rt,type = "prob")

#Creating Submission File
testid<-read.csv("test_id.csv",header=T)
View(testid)
final_r<-cbind(testid$x,rpred)
colnames(final_xg)<-c("id","target")
write.csv(final_xg,"rsubmission1.csv",row.names = F)





#Feature selection using random forest (Already implemented)
#imp = data.frame(importance(reg, type = 2))
#print(imp)
#write.table(imp ,"importance4.txt")





#rfconf<-confusionMatrix(rpred,rtrain_bal_target)
#print(rfconf)
#rtrain_bal<-cbind(rtrain_bal_target,rtrain_bal)
#colnames(rtrain_bal)[1]<-"target"
#write.csv(rtrain_bal,"cb_dataset.csv",row.names = F)


#Creating a new dataset (Already created no need for final submission)
#nam<-read.table("importance4.txt",header=T,sep="")
#impf<-nam[order(nam$X.MeanDecreaseGini.,decreasing=TRUE),]
#impf

#impf<-impf[1:34,]
#colname_impf<-impf$X.Feature.
#colname_impf<-as.character(colname_impf)


#name.use<-names(rtrain_bal)[(names(rtrain_bal) %in% colname_impf)]
#rtrain_bal_new<-rtrain_bal[,name.use]
#str(rtrain_bal_new)
#rtrain_bal_new<-cbind(rtrain_bal_target,rtrain_bal_new)
#colnames(rtrain_bal_new)[1]<-"target"
#write.csv(rtrain_bal_new, "balanced_data_3.csv",row.names = F)



