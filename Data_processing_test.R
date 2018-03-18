#Data Preprocessing for test data

rtest<-fread("test.csv",stringsAsFactors = T)
#View(rtrain)
#str(rtrain)

rtest[rtest==(-1)]<-NA
sum(is.na(rtest))

sapply(rtest,function(x) sum(is.na(x)))

#Removing Columns containing max NAs
rtest_id<-rtest$id
write.csv(rtest_id,"test_id.csv",row.names = F)
rtest<-rtest[,-c("id","ps_car_05_cat","ps_car_03_cat","ps_reg_03")]

rtest<-as.data.frame(rtest)
colnames_cat<-grep("_cat", names(rtest), value=TRUE)
colnames_cat


#converting to factor categories
for(i in 1:length(colnames_cat))
{
  rtest[,colnames_cat[i]]<-as.factor(rtest[,colnames_cat[i]])
}
str(rtest)

#converting to factor binary
colnames_bin<-grep("_bin", names(rtest), value=TRUE)
colnames_bin

for(i in 1:length(colnames_bin))
{
  rtest[,colnames_bin[i]]<-as.factor(rtest[,colnames_bin[i]])
}
str(rtest)

#Replacing NA with mode and mean

Mode <- function(x) 
{
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#Mode for categorical variable
for(i in 1:length(colnames_cat))
{
  vec<-rtest[,colnames_cat[i]]
  rownum<-which(is.na(vec))
  nm<-Mode(vec[-rownum])
  vec[rownum]<-nm
  rtest[,colnames_cat[i]]<-vec
}

#Model for binary variable
for(i in 1:length(colnames_bin))
{
  vec<-rtest[,colnames_bin[1]]
  rownum<-which(is.na(vec))
  nm<-Mode(vec[-rownum])
  vec[rownum]<-nm
  rtest[,colnames_bin[i]]<-vec
}

#convert target to factor
#rtest$target<-as.factor(rtrain$target)

#mean 
cat_cols<-c(colnames_bin,colnames_cat)
numcols<-sapply(rtest,is.numeric)
nums<-names(which(numcols==TRUE))
for(i in 1:length(nums))
{
  vec<-rtest[,nums[i]]
  rownum<-which(is.na(vec))
  nm<-vec[-rownum]
  avg<-mean(nm)
  vec[rownum]<-avg
  rtest[,nums[i]]<-vec
}
#verify removal of NA
sum(is.na(rtest))
str(rtest)

write.csv(rtest,"test_cleaned.csv",row.names = F)


#Creating a new test dataset with 34 features(Already implemented)

# nam<-read.table("importance4.txt",header=T,sep=" ")
# impf<-nam[order(nam$X.MeanDecreaseGini.,decreasing=TRUE),]
# impf
# 
# impf<-impf[1:34,]
# colname_impf<-impf$X.Feature.
# colname_impf<-as.character(colname_impf)
# 
# 
# name.use<-names(rtest)[(names(rtest) %in% colname_impf)]
# rtest_new<-rtest[,name.use]
# str(rtest_new)
# write.csv(rtest_new, "test_new.csv",row.names = F)

