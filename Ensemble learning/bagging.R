library(adabag)
library(data.table)
library(adabag)
library(rpart)
#http://archive.ics.uci.edu/ml/datasets/bank+marketing#
bank_data <- read.csv("C://000/bank.csv", sep = ";")
#group
sub <- sample(nrow(bank_data),round(nrow(data)/4))
bank_train <- bank_data[-sub,]
bank_test <- bank_data[sub,]
#mfinal : count of decide tree
bag_1 <- bagging(y~., bank_train, mfinal = 5)
names(bag_1)
#vote,prob,class
sort(bag_1$importance)
bag_2 <- bagging(y~., bank_train, mfinal = 5, control = rpart.control(maxdepth = 3))
#predict
bag_2_pre <- predict(bag_1, bank_test)
names(bag_2_pre)
bag_2_pre$class[11:20]
bag_2_pre$confusion
bag_2_pre$error

