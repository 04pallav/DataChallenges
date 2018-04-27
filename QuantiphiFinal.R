library(caret)
library(gbm)
library(xgboost)

############### reading files
training=read.csv('/Users/04pallav/Downloads/Training\ Sheet1.csv',header=TRUE)
dim(training)
summary(training)
colnames(training)
head(training)

scoring=read.csv('/Users/04pallav/Downloads/Scoring\ Sheet1.csv',header=TRUE)
dim(scoring)
summary(scoring)
colnames(scoring)
head(scoring)
###########################

training1=training
training1$id=NULL
#training1$total=NULL
training1$movie_sequel=as.factor(training1$movie_sequel)
training1$language1=as.factor(ifelse(training1$language=="English","English","NotEnglish"))

mydf=training1
mydf$Category<-as.factor(mydf$Category)
#mydf$Category <- factor(mydf$Category,levels=1:9,ordered = TRUE)
summary(mydf$Category)

library(plyr)
mydf$Category=revalue(mydf$Category, c("1"="Cat1","2"="Cat2","3"="Cat3","4"="Cat4","5"="Cat5","6"="Cat6","7"="Cat7","8"="Cat8","9"="Cat9"))
summary(mydf$Category)


trainIndex <- createDataPartition(mydf$Category, p = .8,list = FALSE,times = 1)
train <- mydf[ trainIndex,]
test  <- mydf[-trainIndex,]

attach(mydf)


################ Missing Values
mean(is.na(training))*100   # No missing values

################ Exploration
qplot(Category, data =mydf,geom = "bar",fill='red',main='Distribution of Categories')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mydf, aes(x = production_year))+geom_bar(fill='blue')+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Distribution of Production Year")

ggplot(mydf, aes(x = movie_sequel))+geom_bar(fill='blue')+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Distribution of Movie_Sequel")


qplot(creative_type, data = mydf, geom = "bar")
qplot(source, data = mydf, geom = "bar")
qplot(production_method, data = mydf, geom = "bar")
qplot(genre, data = mydf, geom = "bar")
qplot(language, data = mydf, geom = "bar")
qplot(movie_board_rating_display_name, data = mydf, geom = "bar")
qplot(movie_release_pattern_display_name, data = mydf, geom = "bar")


qplot(total, data =training, facets = production_year~ .,
      geom = "histogram")

qplot(Category, data =training, facets = production_year~ .,
      geom = "bar")
qplot(Category, data =training, facets = movie_sequel~ .,
      geom = "bar")
qplot(Category, data =training1, facets = language1~ .,
      geom = "bar")

ggplot(mydf, aes(x =movie_release_pattern_display_name,fill=Category))+geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Distribution of Categories in different Movie Release Patterns")

ggplot(mydf, aes(x =movie_sequel,fill=Category))+geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Distribution of Categories in different Sequel Types")

ggplot(mydf, aes(x =production_method,fill=Category))+geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Distribution of Categories in different Production_method Types")


ggplot(mydf, aes(x =creative_type,fill=Category))+geom_bar()+
  theme(plot.title = element_text(hjust = 0.5))+ggtitle("Distribution of Categories in different Creative_type Types")



################ Missing Values
mean(is.na(training))*100   # No missing values



################################################# SVM Classification

set.seed(1)
ctrl <- trainControl(method = "repeatedcv",number=5,repeats =1,savePredictions = TRUE,classProbs = TRUE)
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))
mod_fitSVMRadial <- train(Category~production_year+movie_sequel+creative_type+source+production_method+genre+
                      language+movie_board_rating_display_name+movie_release_pattern_display_name, 
                    data=train,method="svmRadial",trControl = ctrl,tuneGrid=grid)

mod_fitSVMLinear <- train(Category~production_year+movie_sequel+creative_type+source+production_method+genre+
                            language+movie_board_rating_display_name+movie_release_pattern_display_name, 
                          data=train,method="svmLinear",trControl = ctrl)#,tuneGrid=grid)

#mod_fitSVMRadial2 <- train(Category~movie_sequel+creative_type+production_method+movie_release_pattern_display_name, 
 #                         data=train,method="svmRadial",trControl = ctrl,tuneGrid=grid)

mod_fitSVMRadial

pred = predict(mod_fitSVMRadial, newdata=test,type='raw')
confusionMatrix(pred, test$Category)$overall[1]
confusionMatrix(pred, test$Category)
cm=table(pred,test$Category)
d <- row(cm) - col(cm)
away1error=sum(split(cm, d)$'0',split(cm, d)$'1',split(cm, d)$'-1')/sum(cm)
away1error

################################################ Random Forests
set.seed(99)
ctrl <- trainControl(method = "repeatedcv",number=5,repeats = 3,savePredictions = TRUE,classProbs = TRUE)
mod_fitRF <- train(Category~production_year+movie_sequel+creative_type+source+production_method+genre+
                     language1+movie_board_rating_display_name+movie_release_pattern_display_name, 
                   data=train,method="rf",trControl = ctrl)
mod_fitRF

varImpObj=varImp(mod_fitRF)
plot(varImpObj, main = "Variable Importance of Top 20", top = 20)
summary(mod_fitRF)


pred = predict(mod_fitRF, newdata=test,type='raw')
head(pred)
cm=table(pred,test$Category)
d <- row(cm) - col(cm)
confusionMatrix(pred, test$Category)$overall[1]
away1error=sum(split(cm, d)$'0',split(cm, d)$'1',split(cm, d)$'-1')/sum(cm)
away1error

###############################################   Recursive Feature Elimination with RF
subsets <- c(1:20)
ctrl <- rfeControl(functions = rfFuncs,method = "repeatedcv",repeats =5,verbose = TRUE)
x=train[,c("production_year","movie_sequel","creative_type","source","production_method","genre",
           "language","movie_board_rating_display_name","movie_release_pattern_display_name")]
y=train[,"Category"]

rfProfile <- rfe(x, y,sizes = subsets, rfeControl = ctrl)

rfProfile

plot(rfProfile, type=c("g", "o"))

pred = predict(rfProfile$fit, newdata=test,type='response')
head(pred)
cm=table(pred,test$Category)
d <- row(cm) - col(cm)
away1error=sum(split(cm, d)$'0',split(cm, d)$'1',split(cm, d)$'-1')/sum(cm)
confusionMatrix(pred, test$Category)$overall[1]
away1error


ctrl <- trainControl(method = "repeatedcv",number=5,repeats = 1,savePredictions = TRUE,classProbs = TRUE)
mod_fitRF <- train(Category~movie_sequel+creative_type+production_method+movie_release_pattern_display_name, 
                   data=train,method="rf",trControl = ctrl)
pred = predict(mod_fitRF, newdata=test,type='raw')
head(pred)
cm=table(pred,test$Category)
d <- row(cm) - col(cm)
away1error=sum(split(cm, d)$'0',split(cm, d)$'1',split(cm, d)$'-1')/sum(cm)
confusionMatrix(pred, test$Category)$overall[1]
away1error




############################################### Gradient Boosting Machines 
ctrl <- trainControl(method = "repeatedcv",number=5,repeats = 3,savePredictions = TRUE,classProbs = TRUE,verboseIter=TRUE)
mod_fitGBM <- train(Category~production_year+movie_sequel+creative_type+source+production_method+genre+language+
                      movie_board_rating_display_name+movie_release_pattern_display_name, 
                    data=train,method="gbm",trControl = ctrl,verbose=TRUE)

mod_fitGBM
varImpObj=varImp(mod_fitGBM)
plot(varImpObj, main = "Variable Importance of Top 20", top = 20)
summary(mod_fitGBM)


pred = predict(mod_fitGBM, newdata=test,type='raw')
head(pred)
cm=table(pred,test$Category)
d <- row(cm) - col(cm)
away1error=sum(split(cm, d)$'0',split(cm, d)$'1',split(cm, d)$'-1')/sum(cm)
confusionMatrix(pred, test$Category)$overall[1]
away1error


################################################ Neural Nets 


ctrl <- trainControl(method = "repeatedcv",number=5,repeats = 3,savePredictions = TRUE,classProbs = TRUE,verboseIter=TRUE)
mod_fitNN <- train(Category~production_year+movie_sequel+creative_type+source+production_method+genre+language+
                      movie_board_rating_display_name+movie_release_pattern_display_name, 
                    data=train,method="nnet",trControl = ctrl,verbose=TRUE)

mod_fitNN

summary(mod_fitNN)

predtrain = predict(mod_fitNN, newdata=train,type='raw')
head(predtrain)
confusionMatrix(predtrain, train$Category)


pred = predict(mod_fitNN, newdata=test,type='raw')
head(pred)
confusionMatrix(pred, test$Category)

cm=table(pred,test$Category)
d <- row(cm) - col(cm)
away1error=sum(split(cm, d)$'0',split(cm, d)$'1',split(cm, d)$'-1')/sum(cm)
confusionMatrix(pred, test$Category)$overall[1]
away1error


####################################### Scoring the Data

scoring$movie_sequel=as.factor(scoring$movie_sequel)
ctrl <- trainControl(method = "repeatedcv",number=5,repeats = 3,savePredictions = TRUE,classProbs = TRUE,verboseIter=TRUE)
mod_fitGBM2 <- train(Category~production_year+movie_sequel+creative_type+source+production_method+genre+
                      movie_board_rating_display_name+movie_release_pattern_display_name, 
                    data=mydf,method="gbm",trControl = ctrl,verbose=TRUE)

mod_fitGBM2
summary(mod_fitGBM2)


pred = predict(mod_fitGBM2, newdata=scoring,type='raw')
head(pred)
scoringfinal=cbind(scoring,pred)
head(scoringfinal)
scoringfinal$Category=revalue(scoringfinal$pred, c("Cat1"="1","Cat2"="2","Cat3"="3","Cat4"="4","Cat5"="5","Cat6"="6","Cat7"="7","Cat8"="8","Cat9"="9"))
scoringfinal$pred=NULL
head(scoringfinal)
scoringfinal$Category=as.numeric(scoringfinal$Category)
write.csv(scoringfinal,"scoringfinal.csv")




