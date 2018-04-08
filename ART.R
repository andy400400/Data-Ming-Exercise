library(RSNNS)
data("snnsData")
#26張圖，7*5二維圖形
patterns <- snnsData$art1_letters.pat
#1d vector as 2d map, row=72的陣列
inputMaps <- matrixToActMapList(patterns,nrow = 7)
#圖案排列
par(mfrow = c(3,3))
#display 1:9, red = 0, beige = 1
for (i in 1:9) plotActMap(inputMaps[[i]])
#vigilance = 0.5, iteration = 100
#learn rate = learnFuncParams
art_model <- art1(patterns, dimX = 7, dimY = 5, learnFuncParams = c(0.5,0,0), maxit = 100)
#列表
table_summary <- table(encodeClassLabels(art_model$fitted.values))
#---------------------------------------------------------------------------------------------
#extra add
label_vector <- encodeClassLabels(art_model$fitted.values)
#want to see label
label <- 1
#plot or list (raw < 9)
if(table_summary[[1]] < 9){
  a <- NULL
  for (x in 1:length(label_vector)) {
    if(label_vector[x] == label){
      a <- c(a,x)
    }
  }
}
length(a)
#決定圖案排列
par(mfrow = c(2,2))
for (i in 1:length(a)) plotActMap(inputMaps[[i]])

