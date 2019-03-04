install.packages("xlsx")
install.packages("sqldf")
install.packages("reshape2")
install.packages("knitr")
install.packages("rpart")
install.packages("Hmisc")
install.packages("caret")
installed.packages("rpart.plot")
install.packages("mlr")
install.packages("plyr")
library(sqldf) #For SQL Functions
library(reshape2) #For plots
library(knitr) # For tables
library(Hmisc)# Description of the data
library(rpart)
library(caret)
library(rpart.plot)
library(mlr)
library(plyr)
salary_train <- read.csv(choose.files())
salary_test <- read.csv(choose.files())
install.packages("mlbench")
library(mlbench)
View(salary_train)
View(salary_test)
install.packages("e1071")
library(e1071)
str(salary_train)
table(salary_train$Salary)

View(salary_train)

model_salary <- naiveBayes(salary_train$Salary ~. ,data = salary_train[,-14])

pred_salary<-predict(model_salary,salary_test[,-14])
acc <- confusionMatrix(pred_salary,salary_test[,14])
acc
accuracy <- (10550+1789)/15060
accuracy ###81.93

model_salary1 <- naiveBayes(salary_train$Salary ~. ,data = salary_train[,-14],laplace = 1)

pred_salary1<-predict(model_salary1,salary_test[,-14])
acc1 <- confusionMatrix(pred_salary1,salary_test[,14])
acc1
accuracy1 <- (10547+1784)/15060
accuracy1### 81.87

model_salary2 <- naiveBayes(salary_train$Salary ~. ,data = salary_train[,-14],laplace = 2)

pred_salary2<-predict(model_salary2,salary_test[,-14])
acc2 <- confusionMatrix(pred_salary2,salary_test[,14])
acc2
accuracy2 <- (10546+1784)/15060
accuracy2 ##81.87
