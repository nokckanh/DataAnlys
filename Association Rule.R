rm(list=ls())

library(arules)
library(arulesViz)

mydata = read.csv("D:/Documents/DataAnalysis/dataset/OnlineRetail.csv")
head(mydata, n=10)
str(mydata)

dt<-split(mydata$StockCode, mydata$InvoiceNo)
data <- as(dt, "transactions") 
inspect(data)
summary(data)


itemFrequency(data, type = "relative")
itemFrequencyPlot(data,topN = 5)



######
rules <- apriori(data, parameter=list(support=0.005, confidence=0.8))
inspect(rules)

options(digits=2)
inspect(rules[1000:1030])
summary(rules)

rules<-sort(rules, by="lift", decreasing=TRUE)


######
inspect( subset( rules, subset = rhs %pin% "85099B"))

######
frequentItems <- eclat (data, parameter = list(supp = 0.07, maxlen = 15))
inspect(frequentItems)
itemFrequencyPlot(data, topN=10, type="absolute", main="Item Frequency")



######
#Khach hang co kha nang mua gi truoc khi mua san pham 22921
rules2<-apriori(data=data, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs=c("22921")),
               control = list(verbose=F))
rules2<-sort(rules2, decreasing=TRUE,by="confidence")

#lhs = 22666
rules2_subset<- subset( rules2, subset = lhs %pin% "85099B" )
inspect(rules2_subset)

plot(rules2_subset, method = "graph", measure = "lift", shading = "confidence", engine = "htmlwidget")


###########
#Khach hang co kha nang mua gi neu mua san pham 22921
rules3<-apriori(data=data, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="rhs",lhs="22921"))

rules3<-sort(rules3, decreasing=TRUE,by="confidence")
inspect(head((rules3),n=10))
plot(rules3,method = "graph",
     engine = "interactive")



