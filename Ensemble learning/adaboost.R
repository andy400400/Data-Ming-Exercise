library(adabag)
library(data.table)
library(rpart)
#http://archive.ics.uci.edu/ml/datasets/bank+marketing#
bank_data <- read.csv("C://000/bank.csv", sep = ";")
#group
sub <- sample(nrow(bank_data),round(nrow(bank_data)/4))
bank_train <- bank_data[-sub,]
bank_test <- bank_data[sub,]
#mfinal : count of decide tree
boo_1 <- boosting(y~., bank_train, mfinal = 5)
names(boo_1)
#predict
boo_1_pre <- predict(boo_1, bank_test)
names(boo_1_pre)
boo_1_pre$class[11:20]
boo_1_pre$confusion
boo_1_pre$error
#vote,prob,class
sort(bag_1$importance)
boo_2 <- bagging(y~., bank_train, mfinal = 5, control = rpart.control(maxdepth = 3))

