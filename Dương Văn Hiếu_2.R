#-----------------if----------------

x <- 30L
if(is.integer(x)){
  print("X is an Interger")
  cat(x, "is an Interger")
}

# ---------------- $ in $ -------------

x <- c("What","is","Truth")
if("Truth" %in% x){
  print("Truth is found")
} else {
  print("Truth is not found")
}

#------------if - eleif - else ---------------

x <- c("What","is","truth")
if("Truth" %in% x){
  print("Truth is found the first time")
} else if("truth %in% x"){
  print("Truth is found the second time")
} else {
  print("Truth is not found")
}

#---------- Switch--------------

x <-switch (2,"first","second","third","fourth")
print(x)

#----------reeat------------

v <- c("Hello","loop")
d <- 3

repeat {
  print(v)
  d <- d+1
  if (d > 5) {
    break
  }
}

#----------While-----------

v <- c("Hello","While loop","addtion")
cnt <- 2

while(cnt < 7){
  print(v)
  cnt = cnt + 1
}

# --------------for ------------

v <- LETTERS[1:10]
for(i in v){
  print(i)
}

#---------- paste() ---------

a <- "Hello"
b <- "Data Analyis"
c <- "With R"
# tam <- paste(a,b,c)
print(paste(a,b,c))
# print(tam)
print(paste(a,b,c, sep = "-"))

print(paste(a,b,c, sep = "",collapse =  ""))

#-----------Format---------------

resutl <- format(23.123456789 , digits = 9)
print(resutl)

resutl <- format(c(6, 13.14521) ,  scientific = TRUE)
print(resutl)

resutl <- format(23.47 , nsmall = 5)
print(resutl)

resutl <- format(6)
print(resutl)

resutl <- format(13.7, width = 6)
print(resutl)
#le
resutl <- format("Hello", width = 8, justify = "l")
print(resutl)
#center
resutl <- format("Hello", width = 8, justify = "c")
print(resutl)
#right
resutl <- format("Hello", width = 8, justify = "r")
print(resutl)

#------------------toupper() , tolower () -----------

rst <- toupper("Chagging to Upper")
print(rst)

rst <- tolower("Chagging to Lower")
print(rst)

#-------- substring()-----------

rst <- substring("Extract string", 2 ,5)
print(rst)

#------------- vecter -------------

v <- 5:13
print(v)

v <- 6.6 : 12.6
print(v)

v <- 3.8 : 11.4
print(v)

#----- seq()------------

print(seq(5, 9, by =0.4))

# ------ c()-----

s <- c('apple', 'red', 5 , TRUE)
print(s)

#--------- Accept vector using  index -------------
t <- c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat")
u <- t[c(2,3,6)]
print(u)

v <- t[c(-2,-5)]
print(x)

v <- t[c(TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)]
print(v)

y <- t[c(0,0,0,0,0,0,0,1)]
print(y)

v1 <-c(3,8,4,5,0,11)
v2 <-c(4,11,0,8,1,2)
v3 <-c(4,11)

add.resutl <- v1+v2
print(add.resutl)

add.resutl1 <- v1+v3
print(add.resutl1)

#---------------Sort-----------------

v <-c(3,8,4,5,0,11,-8-301)
sort.rst <- sort(v)
print(sort.rst)

revsort.result <- sort(v,decreasing = TRUE)
print(revsort.result)

#---------------List-------------

list_data <-list("Red","Green",c(21,32,11), TRUE, 51.23,119.1)
print(list_data)

#----------Name List

list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),list("green",12.3))

names(list_data) <- c ("vector","A matrix" ,"List")

print(list_data)

print(list_data[1])

print(list_data[3])
print(list_data$`A matrix`)



