library(kknn)
data(miete)
#分層抽樣
library(sampling)
n <- round(2/3*nrow(miete)/5)
sub_train <- strata(miete, stratanames = "nmkat", size = rep(n,5), method = "srswor")
#修改function
data_train <- qwe(miete[,c(-1,-3,-12)], sub_train$ID_unit)
data_test <- miete[ -sub_train$ID_unit,c(-1,-3,-12)]
dim(data_train)
dim(data_test)
library(MASS)
fit_lda_1 <- lda(nmkat~.,data_train)
names(fit_lda_1)
fit_lda_1$means
plot(fit_lda_1)
#
pre_lad_1 <- predict(fit_lda_1,data_test)
pre_lad_1$class
pre_lad_1$posterior
confusion_matrix <- table(data_test$nmkat,pre_lad_1$class)
accuraty = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuraty



#edit getdata function
qwe<-function (data, m) 
{
  if (!is.data.frame(data)) 
    data = as.data.frame(data)
  if (is.null(names(data))) 
    stop("the column names are missing")
  if (is.vector(m) & !is.list(m)) {
    res = NULL
    if (is.null(names(m))) 
      if (all(m %in% c(0, 1))) {
        res = NULL
        if (!("ID_unit" %in% names(data))) {
          res = cbind.data.frame((1:length(m))[m == 1], 
                                 data[m == 1, ])
          names(res) = c("ID_unit", names(data))
        }
        else res = data[m == 1, ]
      }
    else res = data[m[m > 0], ]
  }
  res
}