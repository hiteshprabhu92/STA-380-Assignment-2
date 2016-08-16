library(arules)
library(arulesViz)

groceries = read.transactions(file = "files/groceries.txt", rm.duplicates = TRUE, format = "basket", sep = ',')

itemFrequencyPlot(groceries,topN=20,type = "absolute", col = 'blue', xlab = 'Item', main = 'Frequency of Item Purchases')

rules = apriori(groceries, parameter = list(support = .01, confidence = .33, target = 'rules'))

plot(rules)

inspect(subset(rules, subset=lift > 2.3))


