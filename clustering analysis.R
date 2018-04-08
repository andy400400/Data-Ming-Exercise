data("USArrests")
#euclidean, manhattan, minkowski
distance <- dist(USArrests, method = "euclidean")
#singel, complete, average, centroid, ward
hc <- hclust(distance, method = "complete")
plot(hc, hang = -1)
#plot group
rect.hclust(hc,3)
#group of each factor
cutree(hc, 3)
#---------------------------------------------------------------------------------------------
cluster_method <- c("single", "complete", "average", "centroid", "ward.D")
#col mean
overall_mean <- apply(USArrests, 2, mean)
total_ss <- 0
for (i in 1:nrow(USArrests)) {
  total_ss <- total_ss + sum((USArrests[i,] - overall_mean)^2)
}
#prepare 10X3 table
g_plot <- data.frame(group = 1:10, withinss = rep(0), betweenss = rep(0,10))
for (m in cluster_method) {
  hc <- hclust(dist(USArrests, method = "euclidean"), method = m)
  for (g in 2:nrow(g_plot)) {
    withinss <- 0
    #cutree : specifying the desired number of groups
    #calculate mean of each group
    group_mean <- sapply(USArrests, tapply, cutree(hc, k=g), mean)
    #calculate ss of each group
    for (i in 1:g) {
      group <- USArrests[cutree(hc, k=g) == i, ]
      for (j in 1:nrow(group)) {
        withinss <- withinss + sum((group[j,] - group_mean[i,])^2)
      }
    }
    #set value
    g_plot$withinss[g] <- withinss
    g_plot$betweenss[g] <- total_ss - withinss
  }
  #rounds the values in its first argument to the specified number of decimal places
  per <- round(g_plot$betweenss / total_ss, 3)
  #graphics device is opened
  windows()
  plot(g_plot[2:10, "withinss"], type = "n", ylab = "Sum of squares", xlab = "Number of cluster",
       xlim = c(2,10), ylim = c(2000, total_ss), main = m)
  points(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
  lines(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
  
  points(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2)
  lines(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2) 
  #position : g_plot[2:10, "betweenss"] - 100000
  text(g_plot$group[2:10], g_plot[2:10, "betweenss"] - 10000, per[2:10], col = 4)
  legend("topleft", legend = c("within SS", "between SS"), col = 1:2, lty = 1:2, pch = 1:2)
}
#to know sapply-------------------------------------------------------------------------------------------
caa <- NULL
for (a in 1:length(tree_1)) {
  if(tree_1[a] == 6){
    caa <- c(caa,names(tree_1[a]))
  }     
}
length(caa)
asd <- NULL
for (q in 1:nrow(USArrests)) {
  for (w in 1:length(caa)) {
    if(caa[w] == rownames(USArrests[q,])){
      asd <- rbind(asd,USArrests[q,])
    }
  }
}
apply(asd, 2, mean)
