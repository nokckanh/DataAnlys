# comment
# create obj/val

x <- 2
y <- 7
z <- x +y

vect <- c(1:20)
a <- c(3,12,7,17,4,6,5,12,3,4,2)
vect
a

s <- sum(a)
l <- length(a)

tb <- sum(a)/length(a)
tb
tb1 <- s/l
tb1

#Test Fun

.libPaths( )

# Show lib
library()

# show loading packet
search()

#install 
install.packages("XML")

# load package
library("XML")

# assigmetnt
var <- 34
var

b = 4.5
b

12 -> a
a

String <- "Hello world !"
String

# -------
#Logical Data type  
variable_logical<- TRUE  
cat(variable_logical,"\n")  
cat("The data type of variable_logical is ",class(variable_logical),"\n\n")  

#Numeric Data type  
variable_numeric<- 3532  
cat(variable_numeric,"\n")     
cat("The data type of variable_numeric is ",class(variable_numeric),"\n\n")  

#Integer Data type  
variable_integer<- 133L  
cat(variable_integer,"\n")   
cat("The data type of variable_integer is ",class(variable_integer),"\n\n")  

#Complex Data type  
variable_complex<- 3+2i  
cat(variable_complex,"\n")  
cat("The data type of variable_complex is ",class(variable_complex),"\n\n")  

#Character Data type  
variable_char<- "Learning r programming"  
cat(variable_char,"\n")  
cat("The data type of variable_char is ",class(variable_char),"\n\n")  

#Raw Data type  
variable_raw<- charToRaw("Learning r programming")  
cat(variable_raw,"\n")  
cat("The data type of variable_char is ",class(variable_raw),"\n\n")

#-----------
# Assignment using equal operator.  
variable.1 = 124             

# Assignment using leftward operator.  
variable.2 <- "Learn R Programming"     

# Assignment using rightward operator.     
133L -> variable.3             

print(variable.1)  
cat ("variable.1 is ", variable.1 ,"\n")  
cat ("variable.2 is ", variable.2 ,"\n")  
cat ("variable.3 is ", variable.3 ,"\n")  

#Data Type
variable_y<- 124  
cat("The data type of variable_y is ",class(variable_y),"\n")  

variable_y<- "Learn R Programming"     
cat("  Now the data type of variable_y is ",class(variable_y),"\n")  

variable_y<- 133L   
cat("   Next the data type of variable_y becomes ",class(variable_y),"\n")

# IF
a<-11  
if(a<15)  
  print("I am lesser than 15")  

#ELSE
a<-22  
if(a<20){  
  cat("I am lesser than 20")  
}else{   
  cat("I am larger than 20")  
}  

# Repeat
x <- 1  
repeat {  
  cat(x)  
  x = x+1  
  if (x == 6){  
    break  
  }  
}  

# While
a <- 20  
while(a!=0){  
  cat(a)  
  a = a-2  
}     

# For
v <- LETTERS[1:4]  
for ( i in v) {  
  print(i)  
}  


# Function
new.function<- function(n) {  
  for(i in 1:n) {  
    a <- i^2  
    print(a)  
  }  
}  
new.function(6)  

# Next
v <- LETTERS[1:6]  
for ( i in v) {  
  if (i == "D") {  
    next  
  }  
  print(i)  
}  

#Break
n<-1  
while(n<10){  
  if(n==3)  
    break  
  n=n+1  
  cat(n,"\n")  
}  
cat("End of the program")  

#True/False

#Null
as.null(list(a = 1, b = "c")) 

# Inf and NaN
is.finite(x)  
is.infinite(x)  
is.nan(x)  

# NA

is.na(x)  
anyNA(x, recursive = FALSE)  

## S3 method for class 'data.frame'  
is.na(x)  

is.na(x) <- value  

