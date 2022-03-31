#--------
install.packages("MASS")
library("MASS")

?Boston
str(Boston)

# Pairs vẽ biểu đồ phân tán giữa các biến



DataBT = Boston
pairs(DataBT)
pairs(~medv+lstat+age+rm,data = DataBT)


#t�t



# Ma trận tương quan giữa các biến medv , lstat, age ,rm
cor(DataBT[c("medv","lstat","age","rm")])

# Thực hiện chia bộ dữ liêu làm 2 phần: 75% qsat để xây dựng mô hình
# 25% qsat để kiếm định (catTools)
install.packages("caTools")
library("caTools")
DataBT["ID"] = c(1:506)
set.seed(123)
chiadulieu = sample.split(DataBT$ID, SplitRatio = 2/3)
chiadulieu
# Ktra
table(chiadulieu)

mauxaydung = subset(DataBT,chiadulieu == TRUE)
maukiemdinh = subset(DataBT,chiadulieu == FALSE)

#
dubao = glm(chas ~ crim + indus + nox + rad + tax + medv, family = binomial, 
            data = mauxaydung)
dubao = predict(model ,type="response" , newdata = mauxaydung)
summary(dubao)
table(mauxaydung$chas , dubao > 0.4)

step(model)

# Xây dựng mô hình 1 : Sử dụng biến lstat(% dân cư có địa vị thấp trong khu vực đó),
# age(tuổi nhà TB), rm (số phòng TB của ) đẻ dự báo về giá trị nhà
#---------------

mohinh1 = lm(medv~ lstat+age+rm ,data = mauxaydung)
mohinh1
summary(mohinh1)

# Dự báo
dubao_xaydung = predict(mohinh1, mauxaydung)
dubao_xaydung


