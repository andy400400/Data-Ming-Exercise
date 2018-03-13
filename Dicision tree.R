library(MASS)
library(rpart)
data("Pima.tr")
summary(Pima.tr)
set.seed(1111)
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
library(C50)
library(MASS)
data("Pima.tr")
#noGlobalPruning = F 會進行pruning
C50_tree = C5.0(type~.,Pima.tr,control = C5.0Control(noGlobalPruning = F))
summary(C50_tree)
par(xpd = TRUE);plot(C50_tree);text(C50_tree)
#predict
pre = predict(C50_tree,Pima.te,type = "class")
confusion_matrix = table(Type = Pima.te$type,Predict = pre)
confusion_matrix
accuraty = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuraty
#---------------------------------------------------------------------------------------------
library(CHAID)
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
data("Pima.tr")
data("Pima.te")
Pima = rbind(Pima.tr,Pima.te)
level_name = {}
for(i in 1:7){
  #Convert Numeric to Factor
  Pima[,i] = cut(Pima[,i],breaks = 3,ordered_result = T,include.lowest = T)
  level_name <- rbind(level_name,levels(Pima[,i]))
}
level_name = data.frame(level_name)
row.names(level_name) = colnames(Pima)[1:7]
colnames(level_name) = paste("L",1:3,sep = "")
level_name
Pima.tr = Pima[1:200,]
Pima.te = Pima[201:nrow(Pima),]
set.seed(1111)
CHAID_tree = chaid(type~.,Pima.tr)
CHAID_tree
plot(CHAID_tree)
#predict
pre = predict(CHAID_tree,Pima.te)
confusion_matrix = table(Type = Pima.te$type,Predict = pre)
confusion_matrix
accuraty = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuraty
