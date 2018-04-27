
#1a)
LoanPath='/Users/04pallav/Dropbox/Cap\ One\ 14th/data-challenge-data-master/2012_to_2014_loans_data.csv'
InstitutionsPath='/Users/04pallav/Dropbox/Cap\ One\ 14th/data-challenge-data-master/2012_to_2014_institutions_data.csv'

loans=read.csv(LoanPath)
institutions=read.csv(InstitutionsPath)

dim(loans)
dim(institutions)
colnames(loans)
colnames(institutions)
summary(loans)
summary(institutions)


mergedData <- merge(loans, institutions, by=c("Agency_Code","Respondent_ID", "As_of_Year"), all.x = TRUE)

mergedData$Loan_Amount_000Cat<-cut(mergedData$Loan_Amount_000, c(0,10,100,500,1000,5000))



#1b)
hmda_init <- function() {
  loans=read.csv(LoanPath)
  institutions=read.csv(InstitutionsPath)
  mergedData <- merge(loans, institutions, by=c("Agency_Code","Respondent_ID", "As_of_Year"), all.x = TRUE)
  mergedData$Loan_Amount_000Cat<-cut(mergedData$Loan_Amount_000, c(0,10,100,500,1000,5000))
  return(mergedData)
} 

mergedData=hmda_init()


library(jsonlite)
library(dplyr)

hmda_to_json= function(data, state, conventional_conforming){ 
  if(missing(state) & missing(conventional_conforming)){data_filter=data }
  if (missing(state) & !missing(conventional_conforming)) { data_filter=data %>% filter(Conventional_Conforming_Flag==conventional_conforming)}
  if (!missing(state) & missing(conventional_conforming)) { data_filter=data %>% filter(State==state)}
  if (!missing(state) & !missing(conventional_conforming)){ data_filter=data %>% filter(State==state & Conventional_Conforming_Flag==conventional_conforming)}
  write(toJSON(data_filter), "data_filter.json")
  
  
  return(data_filter)
  }

##Checking the function in various testcases
data_filter=hmda_to_json(mergedData)
dim(hmda_to_json(mergedData))
dim(hmda_to_json(mergedData,"DC"))
dim(hmda_to_json(mergedData,"DC","Y"))
dim(hmda_to_json(mergedData,,"Y"))

#2) Quality Checks

mergedData$Loan_Amount_000=ifelse(mergedData$Loan_Amount_000>999,mergedData$Loan_Amount_000/1000,mergedData$Loan_Amount_000)

mergedData=unique(mergedData)
print(paste("The number of duplicate rows removed is",nrow(mergedData)-nrow(unique(mergedData))))

print(paste("The number of Null Values in Respondent_Name_TS is",
            sum(is.na(mergedData$Respondent_Name_TS ))))

a=mergedData %>% filter(Conventional_Status=='Conventional' & Conforming_Status=='Conforming')
if(all(a$Conventional_Conforming_Flag == 'Y')) {print("All Conventional_Conforming_Flags are correct ")} else { print("Resetting the Conventional Conforming Flags") 
  mergedData[mergedData$Conventional_Status=='Conventional' & mergedData$Conforming_Status=='Conforming',"Conventional_Conforming_Flag"]}

dataQualityCheck=function(testData) {
  
  testData$Loan_Amount_000=ifelse(testData$Loan_Amount_000>999,testData$Loan_Amount_000/1000,testData$Loan_Amount_000)
  
  
  print(paste("The number of duplicate rows removed is",nrow(testData)-nrow(unique(testData))))
  testData=unique(testData)
  
  print(paste("The number of Null Values in Respondent_Name_TS is",
              sum(is.na(testData$Respondent_Name_TS ))))
  
  a=testData %>% filter(Conventional_Status=='Conventional' & Conforming_Status=='Conforming')
  if(all(a$Conventional_Conforming_Flag == 'Y')) {print("All Conventional_Conforming_Flags are correct ")} 
  else { print("Resetting the Conventional Conforming Flags") 
    testData[testData$Conventional_Status=='Conventional' & testData$Conforming_Status=='Conforming',"Conventional_Conforming_Flag"]}
 summary(testData) 
}

dataQualityCheck(a)

#3)
#########PIE PLOT FOR MARKET SHARE
a=mergedData %>% group_by(State) %>% summarize(LoanVolume=n())%>%
  mutate(prcent = round(100*LoanVolume/sum(LoanVolume), 2))
pie(a$prcent,paste(a$State,' ',a$prcent,'%'),
    col=c("purple", "violetred1", "green3","cornsilk", "cyan","white"))

#########LOAN_VOLUME BY DIFFERENT STATES ACCORDING TO TIME
library(ggplot2)
a=mergedData %>% group_by(State,As_of_Year) %>% summarize(LoanVolume=n())
options(scipen=10000)
positions <- c("VA","MD","WV","DE","DC")
ggplot(a, aes(x = As_of_Year, y =LoanVolume,fill=factor(State,levels=rev(positions))))+
geom_area(stat ="identity",position ='stack')+
ggtitle("Loan Volume by different States according to time")+scale_x_continuous(breaks = c(2012:2014)) 

################LOAN_VOLUME BY DIFFERENT COUNTIES ACCORDING TO TIME
a=mergedData %>% group_by(County_Name) %>% summarize(LoanVolume=n())
a=a[order(-a$LoanVolume),][1:10,]
positions=a$County_Name

a=mergedData %>% filter(County_Name %in% positions) %>% group_by(County_Name,As_of_Year) %>% summarize(LoanVolume=n())
ggplot(a, aes(x = As_of_Year, y =LoanVolume,fill=factor(County_Name,levels=rev(positions))))+
geom_area(stat ="identity",position ='stack')+ggtitle("Loan Volume by top 10 Counties according to time")+
scale_x_continuous(breaks = c(2012:2014))

##########LOAN_VOLUME ACCORDING TO LOAN_PURPOSE
a=mergedData %>% group_by(State,Loan_Purpose_Description) %>% summarize(LoanVolume=n())
positions <- c("VA","MD","WV","DE","DC")
ggplot(a, aes(x = State, y =LoanVolume,fill=Loan_Purpose_Description))+geom_bar(stat ="identity",position = "dodge")+
  scale_x_discrete(limits = positions)+ggtitle("Loan Volume in states broken down according to Loan Purpose")

##########STRATEGY FOR TOP BANKS
a=mergedData %>% group_by(Respondent_Name_TS) %>% summarize(LoanVolume=n())
a=a[order(-a$LoanVolume),][1:10,]
positions=a$Respondent_Name_TS
a=mergedData %>% group_by(Respondent_Name_TS,Loan_Purpose_Description) %>% summarize(LoanVolume=n())
ggplot(a, aes(x =Respondent_Name_TS,y =LoanVolume,fill=Loan_Purpose_Description))+
geom_bar(stat ="identity",position ='dodge')+scale_x_discrete(limits = positions)+
ggtitle("Loan Volume by Top 10 banks according to Loan_Purpose")+theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1))

###############LOAN VOLUME ACCORDING TO TIME BY LOAN TYPE
a=mergedData %>% group_by(As_of_Year,Loan_Type_Description) %>% summarize(LoanVolume=n())

ggplot(a, aes(x = As_of_Year, y =LoanVolume,fill=Loan_Type_Description))+geom_bar(stat ="identity",position = "dodge")+ggtitle("Loan Volume in states broken down according to Loan_Type_Description")
