data("USArrests")
set.seed(1111)
total_ss <- 0
for (i in 1:nrow(USArrests)) {
  total_ss <- total_ss + sum((USArrests[i,] - overall_mean)^2)
}
g_plot <- data.frame(group = 1:10, withinss = rep(0,10), betweenss = rep(0,10))
for (g in 2:nrow(g_plot)) {
  km <- kmeans(USArrests,centers = g)
  #set value
  g_plot$withinss[g] <- km$tot.withinss
  g_plot$betweenss[g] <- km$betweenss
}
#rounds the values in its first argument to the specified number of decimal places
per <- round(g_plot$betweenss / km$totss, 3)
windows()

plot(g_plot[2:10, "withinss"], type = "n", ylab = "Sum of squares", xlab = "Number of cluster",
     xlim = c(2,10), ylim = c(2000, total_ss), main = m)
points(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)
lines(g_plot$group[2:10], g_plot[2:10, "withinss"], col = 1, lty = 1, pch = 1)

points(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2)
lines(g_plot$group[2:10], g_plot[2:10, "betweenss"], col = 2, lty = 2, pch = 2) 

text(g_plot$group[2:10], g_plot[2:10, "betweenss"] - 10000, per[2:10], col = 4)
legend("topleft", legend = c("within SS", "between SS"), col = 1:2, lty = 1:2, pch = 1:2)
#---------------------------------------------------------------------------------------------
kmeans_3 <- kmeans(USArrests, centers = 3)
g_list <- list(cluster_1 = NULL, cluster_2 = NULL, cluster_3 = NULL)
#which factors are in which groups
for (a in 1:length(kmeans_3[[1]])) {
  
  for (b in 1:3) {
    if(kmeans_3[[1]][[a]] == b){
      g_list[[b]] <- append(g_list[[b]],names(kmeans_3[[1]][a]))
    }
  }
}
g_list

