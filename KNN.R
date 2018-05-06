library(kknn)
data(miete)
#Stratified Random sampling
library(sampling)
#2/3 data to training ,5 group
n <- round(2/3*nrow(miete)/5)
sub_train <- strata(miete, stratanames = "nmkat", size = rep(n,5), method = "srswor")
#edit getdata function
data_train <- qwe(miete[,c(-1,-3,-12)], sub_train$ID_unit)
data_test <- miete[ -sub_train$ID_unit,c(-1,-3,-12)]
dim(data_train)
dim(data_test)
#knn
library(class)
accuraty_vector <- rep(0,20)
k <- 1:20
accuraty_table <- data.frame(k,accuraty_vector)
for (x in 1:20) {
  pre_knn <- knn(data_train[,-12], data_test[,-12],cl = data_train[,12],k = x)
  pre_knn
  confusion_matrix <- table(data_test$nmkat,pre_knn)
  accuraty = sum(diag(confusion_matrix))/sum(confusion_matrix)
  accuraty_table[x,2] <- accuraty
}
plot(accuraty_table[,2],type = "l", xlab = "k", ylab = "accuraty",main = "accuraty plot")

