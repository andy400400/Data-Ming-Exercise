#simple random sampling
data("UScrime")
#replace:抽取是否放回，預設不放回
group_1 <- sample(nrow(UScrime),100,replace =TRUE)
UScrime[group_1,]
#group train test dataset
random_sub <- sample(nrow(UScrime),nrow(UScrime)%/%3,replace =FALSE)
random_test <- UScrime[random_sub,]
random_train <- UScrime[-random_sub,]
#---------------------------------------------------------------------------------------------
#stratified sampling
library("sampling")
eachGroupCount <- as.numeric(table(UScrime$So)%/%4)
stratified_sub <- strata(UScrime, stratanames = "So", size = eachGroupCount, method = "srswor")
stratified_test<- UScrime[stratified_sub$ID_unit,]
stratified_train<- UScrime[-stratified_sub$ID_unit,]
#---------------------------------------------------------------------------------------------
