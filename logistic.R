library(MASS)
data("Pima.tr")
logistic_model <- glm(type~., data = Pima.tr, family = binomial)
pre_logistic <- predict(logistic_model, Pima.tr, type = "response")
accuracy <- NULL
threshold <- seq(0.1, 0.9, 0.1)
for (a in threshold) {
  pre <- ifelse(pre_logistic >= a, 1, 0)
  tab <- table(Pima.tr$type, pre)
  accuracy <- c(accuracy, sum(diag(tab))/nrow(Pima.tr))
}
accuracy_list<- data.frame(threshold, accuracy)
accuracy_list
#optimal model
accuracy_list[which.max(accuracy_list$accuracy),]
