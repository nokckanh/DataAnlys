# Đọc dữ liêu

data = read.csv("D:/Documents/DataAnalysis/dataset/framingham.csv")

# Xem cấu trúc của data
str(data)
names(data)
# Bộ dữ liệu nghiên cứu các nhân tố mà ảnh hưởng đến khả năng mắc bệnh tim mạch do mạch vành (Biến TenYearCHD)
# từ đó đưa ra các chuẩn đoán và điêu trị
# Các yêu tố nghiên cứu : Thông tin cá nhân của người bệnh (tuổi, giới tính , học vấn,có hút thuốc không.....)
# Lịch sử khám bệnh: Có tiền sử bệnh tim hay không, có béo phì hay không, huyết áp đo được tại lần khám trước
# Các thông số khám bệnh :Lượng cholesteron , huyết áp, chỉ số BMI, chỉ số glucose , nhịp tim ..

# Nghiên cứu 1 về liên hệ độ tuổi với lượng cholesterol 
library(dplyr)
data1 <- select(data ,age,totChol)
data1["ID"] = c(1:4240)
data1

reg <- lm(data1$totChol ~ data1$age , )
reg

plot(data = mauNam ,totChol  ~ age, pch = 16)
abline(reg)

# => Kết quả thấy được lượng cholesterol có xu hương tăng khi tuổi tăng

# Nghiên cứu 2 về nhóm độ tuổi có ảnh hướng việc mắc bệnh tim 
age <- data$age
tench <- data$TenYearCHD
range(age)
chiatuoi <- cut(data$age, c(-Inf,32,40,60,Inf),na.rm = TRUE)
chiatuoi
table(chiatuoi)

age.ten <- table(data$TenYearCHD,chiatuoi)
age.ten

barplot(age.ten, beside = TRUE, main="Biểu đồ nhóm tuổi")

pie(table(age.ten))

441 / (2472+ 441)
38 /(709 + 38)
165 / (414 + 165)

# => từ kết quả có thể suy ra nhóm có khả năng cao mắc bệnh tim là trên 60
# => 28% khả năng mắc bệnh ở nhóm tuổi này cao nhất trong 4 nhóm 

# Nghiên cứu 3 về độ tuổi phân bố trong mẫu và độ tuổi TB mắc bệnh 
# và tỉ lệ mắc bệnh trung bình theo độ tuổi

plot(density(age),add=TRUE,
     type = "l",
     xlab= "Tuổi",
     ylab = "f(tuổi)",
     main="Biểu đồ độ tuổi ")

table(data$TenYearCHD, data$age)
tb.age.macbenh <- tapply(data$age, list(data$TenYearCHD), mean) 
tb.age.macbenh
barplot(tb.age.macbenh, beside = TRUE,
        xlab= "Mắc bệnh",
        ylab = "Tuổi",
        main="Độ tuổi trung bình mắc bệnh ")

barplot(tapply(data$TenYearCHD, list(data$age), mean) , 
        beside = TRUE, main="Tỉ lệ mắc bệnh trung bình theo độ tuổi")

library(ggplot2)
ggplot(data, aes(x = age, y = TenYearCHD)) + geom_point() + geom_line()
str(data)

# Kết quả thì Nam có 48,8 % mắc bệnh còn Nữ là 54,2%
# Độ tuổi trung bình mắc bệnh là 54,3 

# Nghiên cứu 4 về tỷ lẹ mắc bệnh tim theo giới tính
by(data, data$male , summary)

TiLe_male <- tapply(data$TenYearCHD, list(data$male), mean) 
TiLe_male

barplot(TiLe_male, beside = TRUE,
        xlab= "Giới tính",
        ylab = "% Mắc bệnh",
        main="Tỷ lệ mắc bệnh theo giới tính")

# Kết quả thu được là Nam 12,4% , Nữ 18,8% => không khả qua vì tỷ lệ mẫu nam
# và nữ trong mẫu không đều 

# Nghiên cứu về huyết áp có liên quan đến bệnh tim

tapply(data$sysBP, list(data$TenYearCHD), mean)

# => Huyết áp trung bình của ng bt là 130,3 còn người mắc bệnh là 143,6 theo 4240 quan sát
by(data, data$TenYearCHD , summary)

freq <- table(data$TenYearCHD, data$sysBP)
freq

barplot(freq, horiz=T, las=1,
        col=rgb(0.8,0.1,0.1,0.6),
        xlab= "",
        ylab = "Huyết áp",
        main="Tỷ lệ mắc bệnh do huyết áp")



# Nghiên cứu 5 Áp dụng mô hình logtics để dự báo khả năng mắc bệnh của mẫu qsat
#--- Chia bộ mẫu quan sát thành 2 phần bộ mẫu xây dựng và bộ kiểm định

set.seed(100)
library(caTools)
chiadulieu = sample.split(data$TenYearCHD , SplitRatio = 0.65)
chiadulieu

#--- subset
mauXaydung = subset(data, chiadulieu == TRUE)
mauKiemdinh = subset(data, chiadulieu == FALSE)


# Áp dụng mô hình logitics để dự đoán sử dụng tất cả các biến % tỉ lệ bênh nhân mắc bệnh
# Từ đó dự đoán bệnh nhân có khả năng mắc bệnh hay ko
# Mô hình tất cả các biến để dự báo khả năng mắc bệnh 
mohinhhLog = glm(TenYearCHD ~ ., data = mauXaydung, family = binomial())
summary(mohinhhLog)

# Sau khi sumary thì ta tiền sử lý các biến ko cần thiết 
# thì ở đây e ko tiền xử lí để lại tất cả các biến


#Dự báo trên bộ mấy xây dựng #Respon thì dự báo ra xác suất còn để defautl thì có 0/1

dubaoXaydung = predict(mohinhhLog, type = "response" ,newdata = mauXaydung)
summary(dubaoXaydung)

# Dự báo kiểm định
dubaoKiemdinh = predict(mohinhhLog ,type = "response",newdata = mauKiemdinh)
summary(dubaoKiemdinh)

# Xây dựng ma trận nhầm lẫn confuse matrix với ngưỡng t = 0.5
table <-table(mauKiemdinh$TenYearCHD, dubaoKiemdinh > 0.5)
table

barplot(table(mauKiemdinh$TenYearCHD, dubaoKiemdinh > 0.5), beside = TRUE,
        xlab= " Mắc bệnh",
        ylab = "Số lượng",
        main="Bản dự đoán")
# => kết quả dự báo có 1068 có khả năng ko mắc bệnh và 14 chắc chăn ko mắc bệnh
# => Có 173 người có khả năng mắc bệnh và 18 chắc chắn mắc bênh ở mẫu kiểm định

# tính độ chính xác của mô hình
 (1068 + 18) / nrow(mauKiemdinh)

# => 73 % độ chính xác khi dựa vào mô hình logtic

# độ chính xác của mô hình cơ sở

(1068 + 14 ) / nrow(mauKiemdinh)
# => 72,9 % độ chính xác khi dựa vào mô hình cơ sở (ko có sự sai lệch ở 2 mô hình)

# -------- Dự đoán theo đường ROC
# install.packages("ROCR")
library(ROCR)


# Tạo phương tình dự báo = prection trong ROC
ptROC <- prediction(dubaoKiemdinh , mauKiemdinh$TenYearCHD)

# Hàm thực hiện PT ROCR (pt, trục y , 1 - y)
thROC <-  performance(ptROC , "tpr","fpr")

plot(thROC)
plot(thROC, colorize = TRUE)

# => sau khi có biểu đồ ta có thể đánh giá độ chính xác của cách này là 94% cao hơn nhiều
# so với mô hình logtics

#------------------------------------- END ------------------------------------
