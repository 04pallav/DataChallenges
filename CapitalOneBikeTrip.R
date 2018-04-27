#Question 1
#Programmatically download and load into your favorite analytical tool the trip data for September 2015.
data=read.csv('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')
dim(data)
summary(data)
ncol(data)
nrow(data)
colnames(data)

#Question 2
#Plot a histogram of the number of the trip distance ("Trip Distance")
library(ggplot2)
ggplot(data=data,aes(x=Trip_distance))+geom_histogram(bins=30,fill="red")+ggtitle("Histogram of Trip Distance")+xlim(0,25)
ggplot(data=data,aes(x=Trip_distance))+geom_histogram(bins=30,fill="red")+ggtitle("Histogram of Trip
Distance")

#Question 3
#Report mean and median trip distance grouped by hour of day.
library(dplyr)
library(lubridate)
data %>% mutate(hour_of_day = hour(as.POSIXct(lpep_pickup_datetime,"%Y-%m-%d %h:%m:%s"))) %>% 
  group_by(hour_of_day) %>% summarise(meanValue = mean(Trip_distance))
data %>% mutate(hour_of_day = hour(as.POSIXct(lpep_pickup_datetime,"%Y-%m-%d %h:%m:%s"))) %>% 
  group_by(hour_of_day) %>% summarise(medianValue = median(Trip_distance))

#NYC AREA AIRPORT ANALYSIS
JFKpick=data[data$Pickup_latitude > 40.6 & data$Pickup_latitude < 40.7 & 
               data$Pickup_longitude < -73.7 & data$Pickup_longitude > -73.8,]
JFKdrop=data[data$Dropoff_latitude > 40.6 & data$Dropoff_latitude < 40.7 & 
               data$Dropoff_longitude < -73.7 & data$Dropoff_longitude > -73.8,]
dim(JFKpick)
dim(JFKdrop)
JFKall=rbind(JFKpick,JFKdrop)
dim(JFKall)
mean(JFKall$Total_amount)

#Question 4 · Build a derived variable for tip as a percentage of the total fare.
data$tipPercent=data$Tip_amount/data$Fare_amount*100

#Triptime variable
data$Triptime=as.POSIXct(data$Lpep_dropoff_datetime,"%Y-%m-%d %h:%m:%s")-
  as.POSIXct(data$lpep_pickup_datetime,"%Y-%m-%d %h:%m:%s")
data$Triptime=as.numeric(data$Triptime,units="mins")
#Speed Variable
data$speed=data$Trip_distance/data$Triptime # speed in miles per minute

#Cleaning
datacleaned=data[data$Trip_distance>0 & data$Fare_amount>0 & data$Total_amount>0 & data$Triptime>0 & 
                   data$Tip_amount>=0,]


#Transformation
datacleaned$speedlog=log(datacleaned$speed)
datacleaned$Total_amountlog=log(datacleaned$Total_amount)
datacleaned$Triptimelog=log(datacleaned$Triptime)
datacleaned$Fare_amountlog=log(datacleaned$Fare_amount)
datacleaned$Tip_amountlog=log(datacleaned$Tip_amount)

#Partion
index=sample(1:nrow(datacleaned), size=0.8*nrow(datacleaned))
traindata=datacleaned[index,]
testdata=datacleaned[-index,]

#LinearRegression
lm.fit=lm(Tip_amount~Payment_type+Fare_amount+Total_amount+Trip_distance,data=traindata)
summary(lm.fit)
pred_lr=predict(lm.fit,testdata)
SSE =sum((pred_lr-testdata$Tip_amount)^2)
SSE
SST =sum((pred_lr-mean(pred_lr))^2)
SST
1-(SSE/SST)

#Lasso Regression
library(glmnet)
xmatrix <- data.matrix(traindata[, c(11, 12, 19, 20)])
y<- as.vector(traindata$Tip_amount)
fit_lasso = cv.glmnet(xmatrix,y, alpha = 1)
best_lambda = min(fit_lasso$lambda)
glm(formula = traindata$Tip_amount~traindata$Payment_type+traindata$Total_amount
    +traindata$Trip_distance+traindata$Fare_amount)
pred_lasso=predict(fit_lasso, s = best_lambda, newx = as.matrix(testdata[, c(11, 12, 19, 20)]))
SSE =sum((pred_lasso-testdata$Tip_amount)^2)
SSE
SST =sum((pred_lasso-mean(pred_lasso))^2)
SST
1-(SSE/SST)

#Linear Regression for Tip_percent
linearfit3=lm(tipPercent~Fare_amount+Trip_distance+Total_amount+Payment_type+Trip_type+speed+Tip_amount,data=datacleaned)
summary(linearfit3)

#Tree Model
library(tree)
treemodel2=tree(tipPercent~Fare_amountlog+Total_amountlog+Payment_type+Triptimelog+
                  speedlog+Tip_amount,data=traindata)
plot(treemodel2)
text(treemodel2,pretty=0)
tree.pred2=predict(treemodel2,newdata=testdata)
SSE =sum((tree.pred2-testdata$tipPercent)^2)
SST =sum((tree.pred2-mean(tree.pred2))^2)
1-(SSE/SST)

#Q5. A)Can you perform a test to determine if the average trip speeds are materially the same in all weeks
#of September? If you decide they are not the same, can you form a hypothesis regarding why they
#differ?

#Week of Year
dataclean2=datacleaned %>% group_by(week_of_year = week(lpep_pickup_datetime))%>%
  mutate(avgspeed= mean(speed))
kruskal.test(dataclean2$speed,dataclean2$week_of_year)

#Hour of day
dataclean4=datacleaned %>% group_by(hour_of_day= hour(lpep_pickup_datetime))%>% mutate(avgspeed= mean(speed))
kruskal.test(dataclean4$speed,dataclean4$hour_of_day)
ggplot(dataclean4, aes(x=hour_of_day, y=avgspeed*60)) + geom_line()+ggtitle("Speed Vs hour of day")
