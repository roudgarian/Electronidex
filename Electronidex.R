#install.packages("arules")
#install.packages("arulesViz")
library(lattice)
library(ggplot2)
library(stringi)
library(caret)
library(corrplot)
library(corrgram)
library(Hmisc)
library(doMC)
library(arules)
library(arulesViz)
registerDoMC(cores=4)
setwd("C:/R/Cours02/Task04")
tdata <- read.transactions("ElectronidexTransactions2017.txt",
                           format<-c("basket"),rm.duplicates=TRUE)

summary(tdata)
itemFrequencyPlot(tdata, topN=10, type="absolute", main="Item Frequency")
rules <- apriori(tdata, parameter = list(supp=0.002, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

#topRules <- rules[1:10]
plot(rules )
plot(rules, method="graph")
plot(rules, method="paracoord", control=list(reorder=TRUE)[1:10])
plot(sort(rules, by='confidence', decreasing = TRUE)[1:10], method="grouped")

frequentItems<-eclat (tdata, parameter=list(supp=0.1))
inspect(head(frequentItems))

#The rules with confidence of 0.95 (see rules_conf above) imply that, whenever the LHS item was 
#purchased, the RHS item was also purchased 95% of the time.
rules_lift<-sort (rules, by="lift", decreasing=TRUE) Hello
inspect(head(rules_lift))
inspect(rules_lift)
#A rule with a lift of 6 (see rules_lift below) imply that, the items in LHS and RHS are 
#6 times more likely to be purchased together compared to the purchases when they are assumed 
#to be unrelated.

# subsetRules<-which(colSums(is.subset(rules, rules)) > 1)
# length(subsetRules)
# rules<-rules[-subsetRules]

rules<-apriori (data<-tdata, parameter=list (supp<-0.002,conf=0.8),
                  appearance=list (default="lhs",rhs="HP_Laptop"), control=list (verbose=F))
rules_subset <- subset(rules, (rhs %in% paste0("Product=")))
inspect(rules_subset)

rules_conf<- sort (rules, by="confidence", decreasing=TRUE)
rules_lift<- sort (rules, by="lift", decreasing=TRUE) 
inspect(head(rules_lift))
inspect(rules_conf)
summary(rules_conf)
#To see a certain item's rules
ItemRules <- subset(rules_conf, items %in% "HP_Laptop")
inspect(ItemRules)
plot(rules_lift[1:6], method<-"graph", control=list(type="HP_Laptop"))
plot(rules_lift[1:6], method<-"graph", control=list(type="iMac"))
XC=subset(rules, rhs %in% c('iMac', 'HP_Laptop'))
