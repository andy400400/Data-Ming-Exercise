library(MASS)
library(Rcpp)
library(RSNNS)
data("Pima.tr")
data("Pima.te")
set.seed(1111)
Pima = rbind(Pima.tr,Pima.te)
level_name = {}
for(i in 1:7){
  #Convert Numeric to Factor
  Pima[,i] = cut(Pima[,i],breaks = 2,ordered_result = T,include.lowest = T)
  level_name <- rbind(level_name,levels(Pima[,i]))
}
#transform to data.frame
level_name = data.frame(level_name)
row.names(level_name) = colnames(Pima)[1:7]
colnames(level_name)= paste("L",1:2,sep = "")
#離散化屬性水準
level_name
#set training data and testing data
Pima.tr = Pima[1:200,]
Pima.te = Pima[200:nrow(Pima),]
#---------------------------------------------------------------------------------------------
library(bnlearn)
bn = naive.bayes(Pima.tr,"type")
plot(bn)
fitted = bn.fit(bn,Pima.te)
pred = predict(fitted,Pima.te)
train <- Pima.te[,"type"]
tab = table(pred,train)
#Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
acc = sum(diag(tab)) / sum(tab)
#---------------------------------------------------------------------------------------------
#construct Bayesian network
tan = tree.bayes(Pima.tr, "type")
plot(tan)
fitted = bn.fit(tan,Pima.te,method = "bayes")
pred = predict(fitted,Pima.te)
tab = table(pred,train)
acc = sum(diag(tab)) / sum(tab)
