library(MASS)
data("Pima.tr")
Pima_class <- rbind(Pima.tr,Pima.te)[,8]
#z-score
Pima <- scale(rbind(Pima.tr,Pima.te)[,-8])
library(kohonen)
set.seed(1111)
#grid設定輸出成大小,hexagonal = 六角形網路拓樸結構,rectangular = 方形網路拓樸結構
#rlen最大迭代次數,alpha 學習率0.05 -> 0.01
Pima_som <- som(Pima, grid = somgrid(4,4,"hexagonal"), rlen = 1000, alpha = c(0.05, 0.01))
plot(Pima_som, type = "changes")
#整體的網路拓樸結構(U-matrix)
plot(Pima_som, type = "dist.neighbours")
#各輸出神經元與輸入屬性間的權重比例
plot(Pima_som, type = "codes")
#各輸出神經元所包含之樣本數
plot(Pima_som, type = "counts")
