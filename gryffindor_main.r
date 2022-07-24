## Case Study 2 - Association Analysis
## Prepared by: Team Gryffindor
##              Arvy Llave      BSCS_3A
##              Alliana Ermino
##              Ella Mae Poche  
## Ref: https://github.com/millendu/Association-Rule-Mining---Weka
#################################################

#Loading Required Libraries
library(arules)
library(arulesViz)
library(ggplot2)
library(tidyverse)

#inporting the dataset
mydata <- read.csv("C:/Users/LG/Desktop/Case Study 2/bank-data.csv", header = T, colClasses = "factor")

#Create Transactions
trans <- transactions(mydata)
trans

#show the structures of trans
summary(trans)

#Shows Transactions 1-10
inspect(trans[1:10])

#Check the Most frequent Items
itemFrequencyPlot(trans,topN = 20)
#plotting
ggplot(
  tibble(
    Support = sort(itemFrequency(trans, type = "absolute"), decreasing = TRUE),
    Item = seq_len(ncol(trans))
  ), aes(x = Item, y = Support)) + geom_line()

############## Model Creation ###################

#Finding frequent itemsets (target="frequent") with the default settings.
its <- apriori(trans, parameter=list(target = "frequent"))
its

#Sorting by Support
its <- sort(its, by = "support")
#inspecting
inspect(head(its, n = 10))
#plotting
ggplot(tibble(`Itemset Size` = factor(size(its))), aes(`Itemset Size`)) + geom_bar()

#Model Creation using APRIORI algorithm
rules <- apriori(trans, parameter = list(support = 0.05, confidence = 0.9))
length(rules)

#Show rules
inspect(rules[1:10])

#Look at rules with highest lift
rules <- sort(rules, by = "lift")
inspect(rules)
inspect(head(rules, n = 10))

#Visualization
plot(rules)
plot(rules, method = "grouped")
plot(rules, method = "graph")
