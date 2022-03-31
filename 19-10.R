library(ggplot2)
library(car)
library(caret)
library(corrplot)

#Loading data
data(mtcars) 

View(mtcars)
# Looking at variables
str(mtcars)
dim(mtcars)
head(mtcars)
summary(mtcars)
names(mtcars)

#------------------------
# Data Preparation
# converting variables to factors.
mtcars$am   = as.factor(mtcars$am)
mtcars$cyl  = as.factor(mtcars$cyl)
mtcars$vs   = as.factor(mtcars$vs)
mtcars$gear = as.factor(mtcars$gear)

#Dropping dependent variable for calculating Multicollinearity

mtcars_a = subset(mtcars, select = -c(mpg))


#Identifying numeric variables
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)
descrCor

# Visualize Correlation Matrix
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# Checking Variables that are highly correlated 
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)
highlyCorrelated


#Identifying Variable Names of Highly Correlated Variables
highlyCorCol = colnames(numericData)[highlyCorrelated]

#Print highly correlated attributes/variables
highlyCorCol

#Remove highly correlated variables and create a new dataset
dat3 = mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)

#Build Linear Regression Model

fit = lm(mpg ~ ., data=dat3)

#Check Model Performance
summary(fit)

anova(fit)



#--------------------------26.10-------------------
data = read.csv("D:/Documents/DataAnalysis/dataset/data3.csv")
names(data)
dim(data)
str(data)

model <- lm(y~ x1+x2+x3+x4+x5+x6+x7,data = data)
summary(model)

modelx6 <- lm(y~x6,data = data)
summary(modelx6)
pairs(data)

modelAll <- lm(y~.,data = data)
summary(modelAll)

#----> Ham step

step(modelAll)

#----------- Lay he so trong model
coefficients(modelAll)
# xem phan du
residuals(modelAll)

anova(modelAll)

plot(modelAll)

modelfinal <- lm(y~ x6+x7,data = data)
summary(modelfinal)
