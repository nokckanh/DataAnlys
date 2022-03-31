cancer <- c(1,1,0,0)
smoking <- c(1,0,1,0)
ntotal <- c(647,2,622,27)

# logit(P) = -2.6027 + 2.4211 *Smoking 
res <- glm(cancer ~ smoking, family = binomial, weight =ntotal)
summary(res)
d <- data.frame(cancer,smoking,ntotal)

conc <- c(1.35, 1.60, 1.75, 1.85, 1.95, 2.05, 2.15, 2.25, 2.35)
like <- c(13,19,67,45,71,50,35,7,1)
dislike <- c(0,0,2,5,8,20,31,49,12)
total <- like + dislike

data <-data.frame(conc,like,dislike,total)

prob <- like/total
prob
plot(prob ~ conc,pch=16,xlab = "Bieudo")
modellog <- glm(prob ~  conc,family = "binomial", weight = total)

summary(modellog)

lines(conc,modellog$fitted,type = 'l',col="red")

#------------------------- EXCECIL

library(MASS)
install.packages('ISLR')

dataIS <- ISLR::Default
dim(dataIS)

set.seed(100)
library(caTools)

sample <- sample(c(TRUE, FALSE), nrow(dataIS), replace=TRUE, prob=c(0.7,0.3))
train <- dataIS[sample,]
test <- dataIS[sample,]

model1 <- glm(default~student+balance+income,family="binomial",data = train)
summary(model1)

data = read.csv("D:/Documents/DataAnalysis/dataset/adult.csv")


