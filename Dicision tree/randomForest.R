library(randomForest)
data(mtcars)
rf_1 <- randomForest(mpg~., data = mtcars, ntree = 1000, importance = TRUE)
#重要程度
#IncMSE: computed from permuting OOB data
#IncNodePurity:measure is the total decrease in node impurities from splitting on the variable
importance(rf)

data(iris)
rf_2 <- randomForest(Species~., data = iris, proximity = TRUE)
#pch:plotting symbols to use
#palette:colors to use to distinguish the classes.
MDSplot(rf_2, iris$Species, palette = 1:3, pch = as.numeric(iris$Species))
