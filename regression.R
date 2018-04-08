library(car)
library(MASS)
library(lmtest)
data("UScrime")
UScrime$So <- factor(UScrime$So)
summary(UScrime)
scatterplotMatrix(UScrime)
reg <- lm(y~., data = UScrime)
summary(reg)
residualPlots(reg)
#Linear
raintest(y~., data = UScrime)
qqnorm(residualPlots(reg))
qqline(residualPlots(reg))
#Performs the Shapiro-Wilk test of normality
shapiro.test(residualPlots(reg))
#Computes residual independence,each residual will not be affected by other residuals
#residual usually happen dependent in time series. When the DW value is close to 2 often independent
durbinWatsonTest(reg)
#Residual homogeneity
bptest(reg)
outlierTest(reg)
#find highly collinearity, VIF > 10
vif(reg)
for (a in 1:length(vif(reg))) {
  if(vif(reg)[[a]] > 10){
    print(names(vif(reg)[a]))
  }
}
