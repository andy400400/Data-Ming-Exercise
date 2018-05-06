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
#kknn
pre_kknn <- kknn(nmkat~., train = data_train, test = data_test[,-12], k = 5)
summary(pre_kknn)
#
pre_kknn$fitted.values
fit <- fitted(pre_kknn)
confusion_matrix <- table(data_test$nmkat,fit)
accuraty = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuraty
