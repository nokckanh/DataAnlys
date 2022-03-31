# read data / dataset in package

data
iris
CO2
# AirPassengers

# dim(AirPassengers)

# str(AirPassengers)

Orange

#---------- comand bacsic ------------

#----- delete data
#----- set dataset name
d <- Orange

dim(d)
str(d)
names(d)

View(d)

rm(list = ls())

# ------------- read data on Pc
# ------------- create folder dataset on D

df1 <- read.csv("D:/Documents/DataAnalysis/dataset/OnlineRetail.csv")
dim(df1)

df2 <- read.table("D:/Documents/DataAnalysis/dataset/adult.txt")
dim(df2)

df3 <- read.table("D:/Documents/DataAnalysis/dataset/cancer.txt")
dim(df3)

df4 <- read.table("D:/Documents/DataAnalysis/dataset/cancer.txt", sep = ",", header = T)

df5 <- read.xlsx("D:/Documents/DataAnalysis/Dataset-master/scimagojr-3.xlsx", sheetIndex = 1)

df6 <- read.csv("D:/Documents/DataAnalysis/Dataset-master/gdp.csv")

# ------------------- Geting data from online sourcce

df7 <- fread("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
head(mydat)
dim(mydat)

install.packages('curl')

df8 <- fread("https://raw.githubusercontent.com/scheckley/online-retail/master/data.csv")
head(df8)
dim(df8)

# -------- Save data
write.csv(df8, file = "D:/Documents/DataAnalysis/dataset/savefile.csv")

names(df8)


sl1 <- select(df8 ,Quality,UnitPrice)

sl1

sl2 <- select(df8, -Decription , -InvoiceNo)

View

write.cvs(sl2, UnitPrice >= 4.25)

filter(df, condition)


#--------------------- 14/9 ----------------------------
library(curl)
install.packages("data.table")
library("data.table")
Path1 <- fread("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv")
head(Path1)
dim(Path1)

Path2 <- fread("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/train.csv")
head(Path2)
dim(Path2)

names(Path1)
names(Path2)

install.packages("dplyr")
library(dplyr)

#-------- loai bo cot
Path3 <- dplyr::select(Path2, -Survived)
dim(Path3)
names(Path3)

#------ noi 2 bang
Path13 <- rbind(Path1, Path3)
dim(Path13)

AgeTB <- mean(Path3$Age, na.rm = TRUE)
AgeTB

AgeTB2 <-sum(Path3$Age, na.rm = TRUE)/891
AgeTB2

#-------------
sum(is.na(Path3))

#------------- 
colnames(Path3)[apply(df_notNA, 2, anyNA)]

#---------------delete all NA
df_notNA  <- na.omit(Path3)

mean(df_notNA$Age, )

dim(df_notNA)

colnames(df_notNA)[apply(df_notNA, 2, anyNA)]


s <- Path3[complete.cases(Path3)]

is.na(Path3)


#Sample random row in dataframe

dim(Path13)
t1<- Path13[sample(nrow(Path3), 15),]

dim(t1)

nrow(Path3)
ncol(Path3)

#rbind two data frames in R / join 2 dataset

kq <-rbind(Path13 , t1)
dim(kq)

# get 3 colums(1:3) of dataframe Pathh2

t2 <- Path13[1:3]
t3 <- select(Path13 , "Name" , "Sex")
dim(t3)
names(Path13)
dim(t2)
names(t2)

# rename all varib/ comlums
colnames(t2) <- c("name1" , "name2" , "name3")

dim(t2)
names(t2)
dim(Path3)

# nối theo ngang

kq2 <- cbind(Path13 , t2)

dim(kq2)
names(kq2)

edit(kq2)

# Remove duplicate rowss of the dataframe
q1 <- distinct(Path13)

dim(q1)

Path3 %>% distinct() %>% summarise()

str(ori)

#remove duplicate row of thhe data frame using Name Var
distinct(Path3,PassengerID, .keep_all = TRUE)

kq %?% distinct(PassengerId,.keep_all = TRUE)

#---- Apply unique function for data Framre in R
unique(Path3)

## uniqque value of thhe colums in R data Frame
 
r <- Path3[!duplicated(Path3)]
dim(r)

dim(t1)

#return thhe columes contain misssing
list_na <- colnames(df)[apply(df, 2, anyNA)]

list_na

sum(is.na(df$Age))
sum(is.na(df$Fare))
dim(df)

# Excluede the misssing Obs
df_drop <- Path3 %>%

na.omit()

dim(df_drop)

df

# row have missing
sum(complete.cases(Path3))

dim(Path3)

?str

?dim

#row no have missing value
sum(!complete.cases(Path3))

which(is.na(Path13))


#------------ 
# replace missing val with zero

t <- df

edit(Path13)

tam <- Path3

mean(tam$Age, na.rm =TRUE)
# update cac gia tri trung binhh vao NA 
tam$Age[is.na(tam$Age)] <- mean(tam$Age, na.rm =TRUE)
edit(tam)

# replace missing val of column withh mean

tbAge <- mean(Path3$Age, na.rm = TRUE)
toAge <- sum(Path3$Age,na.rm = TRUE)

toAge
tbAge

test <- Path3
edit(test)

test$Age[is.na(test$Age)] <- mean(tam$Age, na.rm =TRUE)

edit(Path3)

#-----replace missingg val of col with median
test$Age[is.na(test$Age)] <- median(tam$Age, na.rm =TRUE)

edit(Path3)


#------------------
str(Path3)
range(Path3$Pclass)

#---------- convert one val to factor
Path13$Pclass <- as.factor(Path13$Pclass)
str(Path13)

# convert many val to factor

cols <- c("Pclass","Sex","Embarked")

for (i in cols) {
  Path3[,i] <- as.factor(Path3[,i])
  
}

str(Path3)

dim(Path13)

names(Path13)

#------BT 14/9

data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv")

#----------gsub 
data$NameVariable <- gsub(".*, | .{1}.*", "", data$Name)

#ABC -> A1C
#-----------------------------
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv")

library(stringr)
data$NameVariable <- str_trim(str_sub(str_extract(data$Name, ",[^\\.]*"), 2))

str_extract(data$Name, ",[^\\.]*")
str_sub(str_extract(data$Name, ",[^\\.]*"))

#------------------21/9--------------------

data$new22 <- (data$Age*2)



#-----------gggsub
data$Ngoi <- gsub(".*, | .{1}.*", "", data$Name)

library("dplyr")
data %>%
  mutate(new3 = Age*2/4) %>%
  head(6)




str(data)

# kiem tra pham vi do tuoi
range(data$Age , na.rm = TRUE)

min(data$Age , na.rm = TRUE)

max(data$Age , na.rm = TRUE)
  

c <- cut(data$Age,4)
c

cc <- cut(data$Age, c(-Inf,20,40,60,Inf))
cc

s <- data %>%
  mutate(newAge = cut(data$Age,4)) %>%
  head(6)


#------------------
rm(list = ls())

View(mtcars)

plot(mtcars$wt, mtcars$drat)

str(mtcars)

library(ggplot2)

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()


ggplot(data = NULL, aes(x = mtcars$wt, y = mtcars$mpg)) + geom_point()


View(pressure)

plot(pressure$temperature, pressure$pressure, type = "l")       

#--------------c1 ve voi plot
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")


#-------c2 sao ggplot

ggplot(pressure, aes(x = temperature, y = pressure)) +  geom_line()

ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line() +  geom_point() # With points added

# ------------- BT
geom_line(pressure$temperature, pressure$pressure/2, col = "red")

geom_point(pressure$temperature, pressure$pressure/2, col = "blue")

#----------------

BOD1 <- BOD  # Make a copy of the data
str(BOD1)
ggplot(BOD1, aes(x = Time, y = demand, group = 1)) +geom_line()

BOD1$Time <- (BOD1$Time)

str(BOD1)

ggplot(BOD1, aes(x = Time, y = demand, group = 1)) +geom_line()


# Map supp to colour
library(tidyverse)

data(iris)
tg <- ToothGrowth
names(tg)
ggplot(tg, aes(x = dose, y = len, colour = supp)) +  geom_line()
# Map supp to linetype 
ggplot(tg, aes(x = dose, y = len, linetype = supp)) +  geom_line()
# Map supp to linetype and colour
ggplot(tg, aes(x = dose, y = len, linetype = supp, colour = supp)) +
  geom_line()

#----------------
barplot(BOD$demand, names.arg = BOD$Time)


View(mtcars)
str(mtcars)

range(mtcars$cyl)
dim(mtcars)

# table dung de dem 
table(mtcars$cyl)
barplot(table(mtcars$cyl))

# ---------------
data$rfactor <-factor(data$Pclass)
str(data)
table(data$Pclass)
barplot(table(data$rfactor))

#----------
# Load gcookbook for the uspopchange data set library(dplyr) upc <- uspopchange %>% arrange(desc(Change)) %>% slice(1:10) upc

library(gcookbook) 
ggplot(upc, aes(x = Abb, y = Change, fill = Region)) + geom_col()
ggplot(upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("#669933", "#FFCC66")) +
  xlab("State")



