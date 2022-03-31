#
newdata = read.csv("D:/Documents/DataAnalysis/dataset/winequality-red.csv")

orig_red = data
data$X <- NULL 
str(data)
factor(data$quality)
data$quality = factor(data$quality, levels= c(3,4,5,6,7,8), ordered = T)


library(ggplot2)
library(reshape2)

ggplot(data = melt(newdata), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) +
  geom_density() + 
  facet_wrap(~variable, scales = 'free_x')

ggplot(aes(x=quality), data = data) +
  geom_bar(fill = "deeppink4", color = "grey3")

ggplot(aes(x=pH), data = data) +
  geom_histogram() + 
  scale_x_continuous(breaks = seq(2.7, 4.1, .1))
summary(data$pH)

ggplot(aes(x=residual.sugar), data = data) +
  geom_histogram() +
  scale_x_log10() +
  xlab("Log10 Residual Sugar")
summary(data$residual.sugar)

ggplot(aes(x=chlorides), data = data) +
  geom_histogram() +
  scale_x_log10() +
  xlab("Log10 Chlorides")
summary(data$chlorides)

install.packages("gridExtra")
library(gridExtra)
v <- ggplot(aes(x=volatile.acidity), data = data) +
  geom_histogram() +
  scale_x_continuous()
f <- ggplot(aes(x=fixed.acidity), data = data) +
  geom_histogram() +
  scale_x_continuous()
c <- ggplot(aes(x=citric.acid), data = data) +
  geom_histogram() +
  scale_x_continuous()
p <- ggplot(aes(x=pH), data = data) +
  geom_histogram() +
  scale_x_continuous()
grid.arrange(v,f,c,p, ncol=2)

#

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode <- getmode(data$alcohol)
ggplot(aes(x=alcohol), data = data) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(8, 15, .5))
summary(data$alcohol)
paste("Mode:", mode)


#-----------------------

library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)

drop_col <- c("fixed.acidity", "X","pH")
red_pair <- data[,!names(orig_red) %in% drop_col]
ggpairs(red_pair)

drop_col <- c("sulphates", "total.sulfur.dioxide","X")
red_pair2 <- orig_red[,!names(orig_red) %in% drop_col]
ggpairs(red_pair2)

# don co tang thi chat luong co xu huong tang
ggplot(aes(x=quality, y=alcohol, group = quality), data = data) +
  geom_boxplot()

# Có vẻ như axit citric cao hơn có thể dẫn đến rượu vang ngon hơn mặc dù vẫn có một
# sự khác biệt lớn trong hầu hết các cấp chất lượng.

ggplot(aes(x=quality, y=citric.acid), data = data) +
  geom_boxplot()

by(data$volatile.acidity, data$quality, var)


ggplot(aes(x=quality, y=volatile.acidity, group = quality), data = data) +
  geom_boxplot()


ggplot(aes(x=quality, y=sulphates, group = quality), data = data) +
  geom_boxplot()


library(gridExtra)
a <- ggplot(aes(x=pH, y=volatile.acidity), data = data) +
  geom_point(alpha=1/5, color="springgreen4")
b <- ggplot(aes(x=pH, y=fixed.acidity), data = data) +
  geom_point(alpha=1/5, color="springgreen4")
c <- ggplot(aes(x=pH, y=citric.acid), data = data) +
  geom_point(alpha=1/5, color="springgreen4")
d <- ggplot(aes(x=citric.acid, y=volatile.acidity), data = data) +
  geom_point(alpha=1/5, color="orange2")
e <- ggplot(aes(x=citric.acid, y=fixed.acidity), data = data) +
  geom_point(alpha=1/5, color="orange2")
f <- ggplot(aes(x=fixed.acidity, y=volatile.acidity), data = data) +
  geom_point(alpha=1/5)
grid.arrange(a,e,c,d,b,f)

#-- Danh gia chat luong neu nhu luong Citruc acid  = 0
library(dplyr)
c1 <- ggplot(aes(x=quality, y=citric.acid), data = data) + geom_boxplot()
data$citric_zero <- ifelse(data$citric.acid == 0, TRUE, FALSE)

citric_ratio <- group_by(data, quality) %>% summarise(count_0= sum(citric_zero == TRUE),n = n())

citric_ratio$ratio <- (citric_ratio$count_0 / citric_ratio$n) * 100

c2 <- ggplot(aes(x=quality, y=ratio), data = citric_ratio) +
  geom_bar(stat = "identity", fill = "orange2") +
  ylab("Percent") + 
  ggtitle("Percent of Wines with No Citric Acid Present by Quality Level")
grid.arrange(c1, c2)


#-- nhin chung do sunfat ruou tot cao hon ruou dom? 
cs1 <- ggplot(aes(x=chlorides, y=sulphates, color=quality), data = data) +
  geom_point() +
  scale_color_brewer(palette= "Greens") +
  scale_x_log10() +
  xlab("Log10 Chlorides") + 
  theme_dark()

cs2 <- ggplot(aes(x=chlorides, y=sulphates), data = data) +
  geom_point(alpha=1/3) +
  facet_grid(~quality) +
  scale_x_log10() +
  xlab("Log10 Chlorides")
grid.arrange(cs1, cs2)


# --- 
new_red <- data
new_red <- new_red[new_red$quality != 5 & new_red$quality != 6, ]
new_red$quality.level <- ifelse(new_red$quality == 7 | (new_red$quality == 8), "High", "Low")

a <- ggplot(aes(x=pH, y=citric.acid, color=quality.level), data = new_red) +
  geom_point()

b <- ggplot(aes(x=pH, y=citric.acid, color=quality), data = data) +
  geom_point() + 
  scale_color_brewer(palette= "Greens") +
  theme_dark()

c <- ggplot(aes(x=citric.acid, y=fixed.acidity, color=quality.level), data = new_red) +
  geom_point()

d <- ggplot(aes(x=citric.acid, y=fixed.acidity, color=quality), data = data) +
  geom_point() + 
  scale_color_brewer(palette= "Greens") + 
  theme_dark()


grid.arrange(a,c,b,d, ncol =2)

#------------
ggplot(aes(x=fixed.acidity, y=chlorides, color=quality), data = data) +
  geom_point() +
  scale_color_brewer(palette = "Greens") +
  scale_y_log10() +
  ylab("Log10 Chlorides") +
  theme_dark()


#------------
ggplot(aes(x=citric.acid, y=residual.sugar, color=quality.level), data = new_red) +
  geom_point() +
  scale_y_log10() +
  ylab("Log10 Residual Sugar")

#---
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)

m1 <- lm(I(quality) ~ I(alcohol), data = orig_red)
m2 <- update(m1, ~ . + volatile.acidity)
m3 <- update(m2, ~ . + citric.acid)
m4 <- update(m3, ~ . + sulphates)
m5 <- update(m4, ~ . + pH)
m6 <- update(m5, ~ . + I(log10(chlorides)))
m7 <- update(m6, ~ . + I(log10(residual.sugar)))
m8 <- update(m7, ~ . + fixed.acidity)
m9 <- update(m8, ~ . + total.sulfur.dioxide)
mtable(m1, m2, m3, m4, m5, m6, m7, m8, m9)

ggplot(aes(x=quality), data = data ) +
  geom_bar(fill = "deeppink4", color = "grey3") +
  ggtitle("Wine Quality Distribution") +
  xlab("Wine Quality")


ggplot(aes(x=quality, y=alcohol, group = quality), data = data) +
  geom_boxplot() +
  xlab("Wine Quality") +
  ylab("Alcohol Percentage") +
  ggtitle("Alcohol Content by Wine Quality")

ggplot(aes(x=pH, y=citric.acid, color=quality.level), data = new_red) +
  geom_point() + 
  xlab("pH Level") +
  ylab("Citric Acid (g/dm^3)" ) + 
  ggtitle("Citric Acid vs pH Levels for High and Low Quality Red Wines ") +
  scale_color_manual("Quality Level", values=c('#F8766D',"#00BFC4"))


model <- lm(quality ~ alcohol ,data = newdata)
summary(model)
step(model)

ggplot(aes(x = quality ,y=alcohol , group = quality) , data = newdata)+
  geom_line(model)


q1 <- ggplot(aes(x=sulphates, y=quality), data=newdata) +
  geom_jitter(alpha=2/3) +
  geom_smooth() +
  ggtitle("Sulphates vs Quality")

q2 <- ggplot(aes(x=sulphates, y=quality), data=subset(newdata, newdata$sulphates < 1)) +
  geom_jitter(alpha=2/3) +
  geom_smooth() +
  ggtitle("Sulphates vs Quality without outliers")


grid.arrange(q1,q2, ncol=1)