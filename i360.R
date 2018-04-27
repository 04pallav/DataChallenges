library(mice)
library(dplyr)
library(tidyr)
library(VIM)
library(pROC)
library(caret)

##reading the data
file1=read.csv('/Users/04pallav/Documents/i360/ProjectFiles/File1.csv',header=TRUE)
file2=read.csv('/Users/04pallav/Documents/i360/ProjectFiles/File2.csv',header=TRUE,na.strings ="NULL")
colnames(file2)
merged=merge(x=file1, y = file2, by = c("ID","State"), all = TRUE)
dim(merged)
summary(merged)
colnames(merged[,sapply(merged, is.factor)])


##cleaning
mean(is.na(merged))*100   #for whole dataframe
md.pattern(merged)
aggr_plot <- aggr(merged, col=c('navyblue','red'),numbers=TRUE, sortVars=TRUE, 
                  labels=names(merged), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(merged,2, pMiss)  # by column
apply(merged,1,pMiss)   # by row
drops <- c("f147","f119","f127","f122")
cleanDF=merged[ , !(names(merged) %in% drops)]   ##dropping columns
cleanDF=cleanDF[-which(rowMeans(is.na(cleanDF)) > 0.25),]  #dropping some rows 
mean(is.na(cleanDF))*100   #percent missing for whole dataframe

##imputation
numericsC=cleanDF[,sapply(cleanDF, is.numeric)]
numericsC$ID=NULL
NumImpute<- mice(numericsC, m=5,maxit=10,method='mean',seed=500,printFlag=FALSE)
numericsCImp=complete(NumImpute)
write.csv('/Users/04pallav/numericsCImp.csv')
numericsCImp=read.csv('/Users/04pallav/numericsCImp.csv')

factorsC=cleanDF[,sapply(cleanDF, is.factor)]
cleanDFNumImp=cbind(numericsCImp,factorsC)
FacImpute<- mice(cleanDFNumImp, m=1,maxit=10,method='cart',seed=500,printFlag=T)
cleanDFNFImp=complete(FacImpute)
write.csv('/Users/04pallav/cleanDFNFImp.csv')
cleanDFNFImp=read.csv('/Users/04pallav/cleanDFNFImp.csv')



##############Logistic Regression
set.seed(1)
dropsl32 <- c("State","f1","f115","f121")
mergedl32=cleanDFNFImp[ , !(names(cleanDFNFImp) %in% dropsl32)]
mergedl32$X=NULL
mydf=mergedl32
mydf$SPENDINGRESPONSE<-as.factor(ifelse(mydf$SPENDINGRESPONSE=='Reduce National Debt and Deficit',
                                        "Reduce","Spend"))
trainIndex <- createDataPartition(mydf$SPENDINGRESPONSE, p = .8,list = FALSE,times = 1)
train <- mydf[ trainIndex,]
test  <- mydf[-trainIndex,]

ctrl <- trainControl(method = "repeatedcv",number=5,repeats = 1,savePredictions = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)
mod_fit <- train(SPENDINGRESPONSE~., data=train,method="glm",metric ="ROC",family="binomial",trControl = ctrl)
mod_fit
varImpObj=varImp(mod_fit)
plot(varImpObj, main = "Variable Importance of Top 20", top = 20)
summary(mod_fit)

pred = predict(mod_fit, newdata=test,type='raw')
head(pred)
confusionMatrix(pred, test$SPENDINGRESPONSE)

##roc
predprob = predict(mod_fit, newdata=test,type='prob')
roc1=roc(test$SPENDINGRESPONSE,predprob[[2]])
plot(roc1, col = "red")
auc1=auc(roc1)
auc1

################################  Logistic with Ridge penalty
library(glmnet)


x_train <- model.matrix( ~ .-1, train[,-1])
y_train_std=as.factor(train[,1])
grid=10^seq(10,-2,length=100)
lm = cv.glmnet(x=x_train,y = as.factor(train[,1]), intercept=FALSE ,family ="binomial", 
               alpha=0, nfolds=7,lambda=grid)
best_lambda <- lm$lambda[which.min(lm$cvm)]
best_lambda
plot(lm)

#alpha=0 for the ridge penalty, 
std_ridge_logit <- glmnet(x_train, as.factor(train[,1]), family="binomial", alpha=0)
SRL_pred_train <- predict(std_ridge_logit, x_train, type="class", s=best_lambda)
confusionMatrix( SRL_pred_train,y_train_std)      ###training error

x_test <- model.matrix( ~ .-1, test[,-1])
y_test_std=as.factor(test[,1])
SRL_pred_test <- predict(std_ridge_logit, x_test, type="class", s=best_lambda)
confusionMatrix( SRL_pred_test,y_test_std)

##roc
predprob = predict(std_ridge_logit,x_test,type='response',s=best_lambda)
pred_std <- prediction(predprob, y_test_std)
perf_std <- performance(pred_std, measure = "tpr", x.measure = "fpr")

#true positive rate
tpr.points1 <- attr(perf_std, "y.values")[[1]]
#tpr.points

#false positive rate
fpr.points1 <- attr(perf_std,"x.values")[[1]]
#fpr.points

#area under the curve
auc1 <- attr(performance(pred_std, "auc"), "y.values")[[1]]
formatted_auc1 <- signif(auc1, digits=3)


roc.data1 <- data.frame(fpr=fpr.points1, tpr=tpr.points1, model="GLM")


ggplot(roc.data1, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", formatted_auc1))

############################################################ PLS
myDf=mergedl32[,sapply(mergedl32, is.numeric)]
myDf$ID=NULL
correlationMatrix <- cor(myDf)
# summarize the correlation matrix
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.95,T,T)
print(highlyCorrelated)

mydf=cleanDFNFImp

ctrl <- trainControl(method = "repeatedcv",number=3,repeats =1,savePredictions = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)
grid <- expand.grid(ncomp=seq(1,300,10))
grid
mod_fit <- train(SPENDINGRESPONSE~., data=train,method="pls",metric ="ROC",trControl = ctrl,tuneGrid=grid)


mod_fit
varImp(mod_fit)
summary(mod_fit)
pred = predict(mod_fit, newdata=test,type='raw')
head(pred)
confusionMatrix(pred, test$SPENDINGRESPONSE)
accuracy <- table(pred, test$SPENDINGRESPONSE)
accuracy
sum(diag(accuracy))/sum(accuracy)
plot(mod_fit)
##roc
predprob = predict(mod_fit, newdata=test,type='prob')

roc1=roc(test$SPENDINGRESPONSE,predprob[[2]])
plot(roc1, col = "blue")
auc1=auc(roc1)
auc1
coords(roc1, "b", ret=c("t","sen", "spec","accuracy"), best.method="closest.topleft")

############################################################# Random Forest
set.seed(1)
mydf=cleanDFNFImp
mydf$SPENDINGRESPONSE<-ifelse(mydf$SPENDINGRESPONSE=='Reduce National Debt and Deficit',0,1)
dmy <- dummyVars(" ~ .", data =mydf,fullRank=T)
trsf <- data.frame(predict(dmy, newdata = mydf))
trsf$X=NULL
mydf=trsf
train1 = sample(1:nrow(mydf), nrow(mydf)*0.8)
trainingdata=mydf[train1,]
testdata=mydf[-train1,]
mydf$SPENDINGRESPONSE=as.factor(mydf$SPENDINGRESPONSE)

rf1=randomForest(SPENDINGRESPONSE~.,data=mydf,mtry=30,ntree=1000)



################################################# SVM

library(kernlab) 
ctrl <- trainControl(method = "repeatedcv",number=5,repeats =1,savePredictions = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)
#grid <- expand.grid(C=c(0.2,0.5,1),sigma=c(1:5))
mod_fit <- train(SPENDINGRESPONSE~., data=train,method="svmRadial",metric ="ROC",trControl = ctrl,verbose = TRUE)#,tuneGrid=grid)

mod_fit
varImp(mod_fit)
summary(mod_fit)
pred = predict(mod_fit, newdata=test,type='raw')
head(pred)
confusionMatrix(pred, test$SPENDINGRESPONSE)
accuracy <- table(pred, test$SPENDINGRESPONSE)
accuracy
sum(diag(accuracy))/sum(accuracy)
plot(mod_fit)
##roc
predprob = predict(mod_fit, newdata=test,type='prob')

roc1=roc(test$SPENDINGRESPONSE,predprob[[2]])
plot(roc1, col = "green")
auc1=auc(roc1)
auc1


#########################Preparing the testdata for prediction

file3=read.csv('/Users/04pallav/Documents/i360/ProjectFiles/File3.csv',header=TRUE,na.strings ="NULL")
dim(file3)
colnames(file3)
drops <- c("f147","f119","f127","f122","ID")
cleanDF2=file3[ , !(names(file3) %in% drops)]   ##dropping columns
mean(is.na(cleanDF2))*100   #percent missing for whole dataframe
numericsC2=cleanDF2[,sapply(cleanDF2, is.numeric)]
numericsC2$ID=NULL
NumImpute<- mice(numericsC2, m=5,maxit=10,method='mean',seed=500,printFlag=FALSE)
numericsCImp2=complete(NumImpute)
write.csv('/Users/04pallav/numericsCImp2.csv')
numericsCImp2=read.csv('/Users/04pallav/numericsCImp2.csv')

factorsC2=cleanDF2[,sapply(cleanDF2, is.factor)]
cleanDFNumImp2=cbind(numericsCImp2,factorsC2)
FacImpute2<- mice(cleanDFNumImp2, m=1,maxit=10,method='cart',seed=500,printFlag=T)
cleanDFNFImp2=complete(FacImpute2)
write.csv(cleanDFNFImp2,'/Users/04pallav/cleanDFNFImp2.csv')
cleanDFNFImp2=read.csv('/Users/04pallav/cleanDFNFImp2.csv')
cleanDFNFImp2$X=NULL

#############################  Prediction on new data

dropsl32 <- c("f3","f1","f115","f121")
mergedl32=cleanDFNFImp[ , !(names(cleanDFNFImp) %in% dropsl32)]
mergedl32$X=NULL
mydf=mergedl32
mydf$SPENDINGRESPONSE<-as.factor(ifelse(mydf$SPENDINGRESPONSE=='Reduce National Debt and Deficit',
                                        "Reduce","Spend"))
train=mydf %>% select(SPENDINGRESPONSE, everything())

x_train <- model.matrix( ~ .-1, train[,-1])
y_train_std=as.factor(train[,1])
grid=10^seq(10,-2,length=100)
lm = cv.glmnet(x=x_train,y = as.factor(train[,1]), intercept=FALSE ,family ="binomial", 
               alpha=1, nfolds=7,lambda=grid)
best_lambda <- lm$lambda[which.min(lm$cvm)]
best_lambda
plot(lm)

#alpha=1 for the lasso penalty, 
std_lasso_logit <- glmnet(x_train, as.factor(train[,1]), family="binomial", alpha=1)
SRL_pred_train <- predict(std_lasso_logit, x_train, type="class", s=best_lambda)
confusionMatrix( SRL_pred_train,y_train_std)      ###training error

test=cleanDFNFImp2[,colnames(train[,-1])]

x_test <- model.matrix( ~ ., test)
SRL_pred_test <- predict(std_lasso_logit, x_test, type="class", s=best_lambda)
probability = predict(std_lasso_logit,x_test,type='response',s=best_lambda)

prediction=ifelse(SRL_pred_test=='Reduce','Reduce National Debt and Deficit','Spend to Improve Economy')

file4=cbind(file3,probability,prediction)
#file4[,-1]

colnames(file4)[151]="Prediction"
colnames(file4)[150]="Probability"
head(file4)
summary(file4$Prediction)
write.csv(file4,"file4.csv")







