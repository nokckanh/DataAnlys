df <- data.frame(hours=c(1, 2, 4, 5, 5, 6, 6, 7, 8, 10, 11, 11, 12, 12, 14), score=c(64, 66, 76, 73, 74, 81, 83, 82, 80, 88, 84, 82, 91, 93, 89)) 

plot(df$hours, df$score, type = "p" , main = "Biểu đồ phân tán" , xlab  ="Giờ" , ylab= "Điểm" , col = "black", pch = 21)
scatter.smooth(df$hours, df$score,main = "đường")

boxplot(df$hours,df$score)
#fit simple linear regression model 
model <- lm(df$score~df$hours)

print(model)
#view model summary 
summary(model)

# �

data <- data.frame(Weight=c(140, 155, 159, 179, 192, 200, 212), Height=c(60, 62, 67, 70, 71, 72, 75)) 
plot(data$Weight~data$Height, type = "p" , main = "Biểu đồ phân tán" , xlab  ="Cân Nặng" , ylab= "Chiều cao" , col = "black", pch = 21)
modeltt <- lm(data$Weight ~ data$Height)
summary(modeltt)

abline(modeltt,col = 'red')

partice = read.csv("D:/Documents/DataAnalysis/dataset/dataset1.csv")   
plot(partice$income~partice$happiness, type = "p" , main = "Biểu đồ phân tán" , xlab  ="a" , ylab= "Hạnh phúc" , col = "black", pch = 21)
modeltt <- lm(partice$income~partice$happiness)
summary(modeltt)
modeltt
abline(modeltt,col = 'red')
