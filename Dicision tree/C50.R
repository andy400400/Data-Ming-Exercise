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