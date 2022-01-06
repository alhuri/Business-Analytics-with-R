
# Lab: Introduction to R


## Basic Commands
###
x <- c(1, 3, 2, 5)
x
?c

###
x = c(1, 6, 2)
x
y = c(1, 4, 3)

###
length(x)
length(y)
x + y

### ls to list vars while rm removes vars
ls()
rm(x, y)
ls()
###remove all vars in ls ..its a shortcut
rm(list = ls())

###
?matrix
### fill matrix col by col
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
###
x <- matrix(c(1, 2, 3, 4), 2, 2)

### fill row by row
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
###
sqrt(x)
x^2

###matrix with normal distribution 
##with a mean of 0 and a standard deviation of  1
x <- rnorm(50)
mean(x)
sd(x)
var(x)
sqrt(var(x))
median(x)
max(x)

##the index of the max value in the verctor
which.max(x)
min(x)
which.min(x)
quantile(x, probs=0.75)
summary(x)
 
###
y <- x + rnorm(50, mean = 50, sd = .1)
var(x, y)
##the operation below result in the correlation
var(x, y) / sqrt(var(x)) / sqrt(var(y))
##this is correlation
cor(x, y)

###so i can get the same values everytime i run the code
set.seed(1303)
rnorm(50)

###
set.seed(3)
y <- rnorm(100)

mean(y)
var(y)
sqrt(var(y))
sd(y)


## Graphics
###
x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
##clo for color, pch for the shape of points
plot(x, y, col=2, pch=1, xlab = "this is the x-axis",
    ylab = "this is the y-axis",
    main = "Plot of X vs Y")

### save fig into a pdf
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

getwd()

###
x <- seq(1, 10)
x
x <- 1:10
x
x <- seq(-pi, pi, length = 50)
###
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))

contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)

fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

###
image(x, y, fa)

persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)


## Indexing Data

###
A <- matrix(1:16, 4, 4)
A
###
A[2, 3]
###return values at(1,2)(1,2)(3,2)(3,4)
A[c(1, 3), c(2, 4)]
### from row 1 to 3 and from col 2 to 4
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
###
A[1, ]
### keep all rows or columns 
###except those indicated in the index
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
###
dim(A)


## Loading Data
getwd()
setwd('C:/Users/15316/OneDrive/Curriculum/2021Autumn Business Analytics/R Lab/Data_file')

###
Auto <- read.table("Auto.csv", header = TRUE, sep = ",")
head(Auto)
Auto[1:4, ]

###omit missing data
Auto <- na.omit(Auto)
dim(Auto)

###to check the variable names.
names(Auto)

## Additional Graphical and Numerical Summaries
###this wont work kis this vars are in the auto file
plot(cylinders, mpg)

###this is the right way
plot(Auto$cylinders, Auto$mpg)

###this is also right 
attach(Auto)
plot(cylinders, mpg)

###box plot needs a factor
cylinders <- as.factor(cylinders)
###
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
## varwidth = T makes the width similar to the number of values
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T,
    horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T,
    xlab = "cylinders", ylab = "MPG")

###
hist(mpg)
hist(mpg, col = 2)
## breaks id for the number of bars
hist(mpg, col = 2, breaks = 15)

### many scatter plots with different pairs
pairs(Auto)
##select the vars of the pairs
pairs(
    ~ mpg + displacement + horsepower + weight + acceleration,
    data = Auto
  )
###

plot(horsepower, mpg)

###
summary(Auto)
###
summary(mpg)
###
