library(MASS)
library(RSNNS)
data("Pima.tr")
set.seed(1111)
Pima.tr <- Pima.tr[sample(1:nrow(Pima.tr),length(1:nrow(Pima.tr))),]
Pima.Value <- Pima.tr[,1:7]
#define outcome
Pima.Targets <- decodeClassLabels(Pima.tr[,8])
#split training and test
Pima.tr <- splitForTrainingAndTest(Pima.Value, Pima.Targets, ratio = 0.1)
#normalization
Pima.tr <- normTrainingAndTestSet(Pima.tr)
#size = hidden layer
#maxit = maximun iterations
model <- mlp(Pima.tr$inputsTrain, Pima.tr$targetsTrain, size = 14, learnFuncParams = 0.01
             , maxit = 100, inputsTest = Pima.tr$inputsTest, targetsTest = Pima.tr$targetsTest)
plotIterativeError(model)
weightMatrix(model)
p_table <- expand.grid(size = c(12,13,14,15,16), learning.rate = c(0.001,0.01,0.1))
for (i in 1:nrow(p_table)) {
  model <- mlp(Pima.tr$inputsTrain, Pima.tr$targetsTrain, size = p_table[i,1], learnFuncParams = p_table[i,2]
               , maxit = 100, inputsTest = Pima.tr$inputsTest, targetsTest = Pima.tr$targetsTest)
  p_table$TestError[i] <- model$IterativeTestError[100]
}
Pima.te[,1:7] <- normalizeData(Pima.te[,1:7])
predictions <-predict(model, Pima.te[,1:7])
table <- confusionMatrix(Pima.te[,8], predictions)
accuracy <- sum(diag(table))/sum(table)
