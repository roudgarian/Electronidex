#install.packages("arules")
#install.packages("arulesViz")
library("RColorBrewer")
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
tdata<-tdata[which(size(tdata)!= 0)]
summary(tdata)

summary(size(tdata) >3)
data[(size(tdata) >3)]
inspect(head(data[size(tdata) >3]))
summary(size(tdata))

itemFrequencyPlot(tdata, topN=17, type="absolute", main="Item Frequency")
rules <- apriori(tdata, parameter = list(supp=0.002, conf=0.7))
rules <- sort(rules, by='confidence', decreasing = TRUE)

plot(rules,control=list(col=brewer.pal(6,"Spectral")),main="")

summary(rules)
inspect(head(rules))

frequentItems<-eclat (tdata, parameter=list(supp=0.1))
inspect(head(frequentItems))

plot(rules[1:6], method="graph", engine = 'htmlwidget')
plot(rules[1:15], method="paracoord", control=list(reorder=TRUE))
plot(sort(rules[1:10], by='confidence', decreasing = TRUE), method="grouped")


rules<-apriori (data<-tdata, parameter=list (supp=0.002,conf=0.8),
                appearance=list (default="lhs",rhs="iMac"), control=list (verbose=FALSE))
rules_subset <- subset(rules, (rhs %in% paste0("iMac")))
inspect(rules_subset)
data[(size(rules_subset))]
summary(size(rules_subset))

Company<- c("Blackwell","Blackwell","Electronidex","Electronidex")
Volume<-c(1578,4249,5704,5252)
products<-c("Desktop","Laptop","Desktop","Laptop")
Mat<-data.frame(Company,Desktops,Laptops)
str(Mat)

ggplot (Mat, aes(x=products, y=Volume, fill=Company,)) + 
  geom_bar (stat="identity", position = position_dodge(width = 0.5),
            color="black")+scale_fill_brewer(palette="Paired")+
  theme_minimal()

