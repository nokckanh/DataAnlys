# doc du lieu
data = read.csv("D:\\Documents\\DataAnalysis\\dataset\\OnlineRetail.csv")

library("splitstackshape")
library("arules")

# lay 200 quan sat dau tien
data = data[1:200,]
# lay 10 quan sat
head(data, n=10)

# chia quan sat
dt <- split(data$Description, data$InvoiceNo)


# chuyen quan sat thanh nhieu cap do 
dt2 = as(dt,"transactions")
summary(dt2)
inspect(dt2)

# ve ra cac item xuat hien nhieu nhat
itemFrequency(dt2, type = "relative")
itemFrequencyPlot(dt2,topN = 5)

# su dung thuat toan apriori de tim cac quy tac
rules = apriori(dt2, parameter=list(support=0.005, confidence=0.6))


#Luu rules
rules3 = as(rules, "data.frame")
write(rules, "D:\\Documents\\DataAnalysis\\dataset\\rules.csv", sep=",")

# hien thi rule cua tat ca cac san pham 
inspect(rules)

# hien thi cac rule cua EDWARDIAN PARASOL RED
inspect( subset( rules, subset = rhs %pin% "EDWARDIAN PARASOL RED" ))

# hien thi 10 rule nhiu nhat
options(digits=2)
inspect(rules[1:20])

summary(rules)

# sap xep lift theo thu tu tang dan
rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:20])

#loai bo cac quy tac ko can thiet
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T ) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
rules<-rules.pruned

# Clean rule
rules3$rules=gsub("\\{", "", rules3$rules)
rules3$rules=gsub("\\}", "", rules3$rules)
rules3$rules=gsub("\"", "", rules3$rules)

#Split the rule

Rules4=cSplit(rules3, "rules","=>")
names(Rules4)[names(Rules4) == 'rules_1'] <- 'LHS'
Rules5=cSplit(Rules4, "LHS",",")
Rules6=subset(Rules5, select= -c(rules_2))
names(Rules6)[names(Rules6) == 'rules_3'] <- 'RHS'

# Khach hang se mua nhung gi truoc khi mua EDWARDIAN PARASOL RED 

ruleLHS<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="EDWARDIAN PARASOL RED"),
               control = list(verbose=F))
ruleLHS<-sort(rules, decreasing=TRUE,by="confidence")
# lay 5 rule dau tien 
inspect(rules[1:5])

library(arulesViz)
plot(ruleLHS, method = "graph", measure = "lift", shading = "confidence", engine = "htmlwidget")

# Khach hang se mua nhung gi tiep theo sau khi da mua EDWARDIAN PARASOL RED
rulesRHS<-apriori(data=dt, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="rhs",lhs="EDWARDIAN PARASOL RED"),
               control = list(verbose=F))
ruleRHS<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])







