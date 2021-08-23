#Why are our best and most experienced employees leaving prematurely?

rm(list=ls())
#setwd("~/...")

EmployeeData <- read.csv("1 Employee Attrition.csv", header = TRUE, sep = ",") 


#clean data: variable names, missing value, recode values
#--------------------------------------------------------

#Explore variables
str(EmployeeData)
summary(EmployeeData)
head(EmployeeData)

#Rename variable "sales" to "department" 
library(reshape) 
EmployeeData <- rename(EmployeeData, c(sales="department")) 

#Missing value check
sum(is.na(EmployeeData)) 

#Recoding to factors
EmployeeData$Work_accident<-as.factor(EmployeeData$Work_accident)
EmployeeData$left<-as.factor(EmployeeData$left)
EmployeeData$promotion_last_5years<-as.factor(EmployeeData$promotion_last_5years)

#Order Salary levels
EmployeeData$salary<-ordered(EmployeeData$salary,levels=c("low","medium","high"))


#Explore Data 
#------------

g5 <- ggplot(EmployeeData, aes(x=left, y=satisfaction_level, group = left, fill = left)) + geom_boxplot() + ggtitle("Satisfaction Level")
g6 <- ggplot(EmployeeData, aes(x=left, y=last_evaluation, group = left, fill = left)) + geom_boxplot() + ggtitle("Last Evaluation")
g7 <- ggplot(EmployeeData, aes(x=left, y=number_project, group = left, fill = left)) + geom_boxplot() + ggtitle("Number of Projects")
g8 <- ggplot(EmployeeData, aes(x=left, y=average_montly_hours, group = left, fill = left)) + geom_boxplot() + ggtitle("Average Montly Hours")
g9 <- ggplot(EmployeeData, aes(x=left, y=time_spend_company, group = left, fill = left)) + geom_boxplot() + ggtitle("Time Spend in Company")

grid.arrange(g5, g6, ncol=2)
grid.arrange(g7, g8, g9, ncol=3)

c1 <- ggplot(EmployeeData, aes(left, fill = Work_accident)) + geom_bar(position="fill") + ggtitle("Work Accident") + labs(y="proportion")
c2 <- ggplot(EmployeeData, aes(left, fill = promotion_last_5years)) + geom_bar(position="fill") + ggtitle("Promotion Last 5 Years") + labs(y="proportion")
c3 <- ggplot(EmployeeData, aes(left, fill = salary)) + geom_bar(position="fill") + ggtitle("Salary Level") + labs(y="proportion")
c4 <- ggplot(EmployeeData, aes(left, fill = department)) + geom_bar(position="fill") + ggtitle("Department") + labs(y="proportion")

grid.arrange(c1, c2, ncol=2)
grid.arrange(c3, c4, ncol=2)


#Use logistic regression
#Improved model No4: only with good employees: evaluation > 0.75 ; time > 3 ; hours > 220 without department
#-----------------------------------------------------------------------------------------------------------

#Create good employees subset
GreatEmployeeData <- EmployeeData[EmployeeData$last_evaluation > 0.75 & EmployeeData$average_montly_hours > 220 & EmployeeData$time_spend_company > 3,]
dim (GreatEmployeeData)

#Subsetting
library(caret)
set.seed(1971)
sub_GreatEmployee <- createDataPartition(y=GreatEmployeeData$left, p=0.7, list=FALSE)
sub_training5 <- GreatEmployeeData[sub_GreatEmployee, ] 
sub_testing5 <- GreatEmployeeData[-sub_GreatEmployee, ]
dim(sub_training5) ; dim (sub_testing5)

#Training
model5 <- glm(left ~ . - department,family=binomial(link='logit'),data=sub_training5)
summary(model5)

#Testing
test_model5 <- predict(model5,newdata=sub_testing5, type="response")
summary(test_model5)

#Evaluating
test_model5 <- ifelse(test_model5 > 0.5,1,0)
confusionMatrix(test_model5, sub_testing5$left)
library(pROC)
auc5 <- roc(as.numeric(sub_testing5$left), as.numeric(test_model5))
auc5
