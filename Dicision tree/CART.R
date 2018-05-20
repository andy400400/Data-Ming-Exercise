library(MASS)
library(rpart)
data("Pima.tr")
summary(Pima.tr)
set.seed(1111)
#na.action:na.rpart:刪除缺少y的資料 or 缺少所有x的資料
#rpart.control
  #cp:complexity parameter
  #minsplit:min sample of each node
  #minbucket:min sample of each leaf
  #maxdepth:control tree high
cart = rpart(type~.,Pima.tr,control = rpart.control(cp = 0))
summary(cart)
#plot
#避免圖片超出
par(xpd = TRUE);plot(cart);text(cart)
#pruning
cart_prune = prune(cart, cp=0.03)
par(xpd = TRUE);plot(cart_prune);text(cart_prune)
#predict
pre = predict(cart,Pima.te,type = "class")
confusion_matrix = table(Type = Pima.te$type,Predict = pre)
confusion_matrix
accuraty = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuraty
#---------------------------------------------------------------------------------------------
data("car.test.frame")
car_data <- car.test.frame
head(car_data)
car_data$Mileage <- 100*4.546/(1.6*car_data$Mileage)
names(car_data) <- c("Price","Country","Reliability","Fuel_consumption","Type","Weight",
                           "Engine_power","horsepower")
#group
group_Fuel_consumption <- {}
group_Fuel_consumption[which(car_data$Fuel_consumption>=11.6)] <- "A"
group_Fuel_consumption[which(car_data$Fuel_consumption<11.6)] <- "B"
group_Fuel_consumption[which(car_data$Fuel_consumption<=9)] <- "C"
car_data <- cbind(car_data,group_Fuel_consumption)
eachGroupCount <- as.numeric(table(car_data$group_Fuel_consumption)%/%4)
library(sampling)
car_sub <- strata(car_data, stratanames = "group_Fuel_consumption", size = eachGroupCount, method = "srswor")
car_test <- car_data[car_sub$ID_unit,]
car_train <- car_data[-car_sub$ID_unit,]
#cart
car_rpart <- rpart(Fuel_consumption~.,car_train,method = "anova")
car_rpart
#CP
printcp(car_rpart)
summary(car_rpart)
par(xpd = TRUE);plot(car_rpart);text(car_rpart)
#other plot
library(rpart.plot)
rpart.plot(car_rpart,type = 4, branch = 1)
rpart.plot(car_rpart,type = 4, fallen.leaves = TRUE)
draw.tree(car_rpart, col = rep(1,7), nodeinfo = TRUE)
#prune
car_rpart_prune <- prune.rpart(car_rpart, cp = 0.1)
printcp(car_rpart_prune)
#plot
par(xpd = TRUE);plot(car_rpart_prune);text(car_rpart_prune)
rpart.plot(car_rpart_prune,type = 4, branch = 1)

