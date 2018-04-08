library(arules)
library(arulesViz)
data("IncomeESL")
#remove missing data
IncomeESL <- IncomeESL[complete.cases(IncomeESL),]
#view information
dim(IncomeESL);colnames(IncomeESL)
#all column to factor
#IncomeESL[] <- lapply(IncomeESL, factor)
#轉換為可以進行關聯分析的transactions物件
Income <- as(IncomeESL,"transactions")
summary(Income)
#出現頻率
sort(itemFrequency(Income),decreasing = T)
itemFrequencyPlot(Income,support = 0.2 ,cex.names = 0.8)
#設定門檻
rules <- apriori(Income, parameter =  list(support = 0.1 , confidence = 0.6))
summary(rules)
#scatter plot
plot(rules, measure = c("confidence","lift"), shading = "support")
plot(rules, method = "grouped")
#outcome為householder status=own
rulesOwn <- subset(rules, subset = rhs %in% "householder status=own" & lift > 1)
inspect(head(sort(rulesOwn,by = "support") , n = 5))
#找出增益小於1
rulesOwn <- subset(rules, lift > 1)