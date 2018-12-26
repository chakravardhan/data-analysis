data <- read.csv("survey.csv")
data <- data[which(data$Timestamp == "2016"),]
data <- data[,!names(data) %in% c("Age","Timestamp","X","mental_illness")]
library(arules)
library(arulesViz)
data <- data[,5:25]
itemsets<-apriori(data,parameter = list(target="frequent",minlen=2))
itemsets.sorted <- sort(itemsets)
inspect(tail(itemsets.sorted,10))
rules <- apriori(data, parameter = list(conf = 0.9, supp = 0.1))
rules.sorted <- sort(rules, by = "lift")
inspect(head(rules.sorted,100))
plot(rules)
