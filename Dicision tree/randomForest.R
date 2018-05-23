library(randomForest)
data(mtcars)
#內插法，補遺漏值
iris_outlier = iris
iris_outlier[80,1]=NA;iris_outlier[100,2]=NA
iris_1 <- rfImpute(Species~., data = iris_outlier)
list("real" = iris[c(80,100),1:4], "have-NA" = iris_outlier[c(80,100),1:4],"disposed" = round(iris_1[c(80,100),2:5],1))
rf_1 <- randomForest(mpg~., data = mtcars, ntree = 1000, importance = TRUE)
#重要程度
#IncMSE: computed from permuting OOB data
#IncNodePurity:measure is the total decrease in node impurities from splitting on the variable
importance(rf)
data(iris)
rf_2 <- randomForest(Species~., data = iris, proximity = TRUE)
#視覺化
#pch:plotting symbols to use
#palette:colors to use to distinguish the classes.
MDSplot(rf_2, iris$Species, palette = 1:3, pch = as.numeric(iris$Species))
#每個樹的節點數
hist(treesize(rf_2))
#決定最佳決策樹數量
data(airquality)
air_rf <- randomForest(Ozone~., data = airquality, mtry = 3, importance = TRUE, na.action = na.omit)
plot(air_rf)
#---------------------------------------------------------------------------------------------


