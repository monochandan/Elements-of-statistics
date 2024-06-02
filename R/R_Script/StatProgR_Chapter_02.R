getwd()

setwd("../New_wd") # relative to current working directory
getwd()
setwd("../Chapter_02") # back to the original working directory

sink("Test.txt")
5 + 5
10 - 9
sink()

c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
seq(from = 1, to = 10, by = 1)
1:10

?seq

??logistic

apropos("sink")

seq(1, 10, 1)

seq(to = 20, by = -5, length.out = 5)

seq(to = 5, by = 1)

rep(29, times = 4)
rep(c(2, 9), times = 4)
rep(c(2, 9), each = 4)
rep(c(2, 9), times = 3, each = 3)

0:10 + 10:0
sqrt(c(1, 16, 36, 144))

1:10 + 10
1:10 + 1:5
1:4 + 1:3

x <- c(20, 19, 25, 1)
x
y <- c(9, 10, 4, 28)
(z <- x + y)

ls()

x
save(x, file = "x.RData")
save(x, y, z, file = "Vectors.RData")

rm(x)
ls()
load("x.RData")
ls()

some_name <- rep(1, 4)
save(some_name, file = "Some_other_name.RData")
new_name <- get(load("Some_other_name.RData"))
new_name
ls()

# Do not run: rm(list = ls())

sort(x)
sort(x, decreasing = TRUE)

some_vector <- c(seq(2, 20, 2), 1:10, seq(3, 30, 3))
some_vector

order(x)
order(x, decreasing = TRUE)

some_vector[] # complete vector
some_vector[1]
some_vector[1:10]
some_vector[-(1:20)]
some_vector[c(5, 10, 15)]

some_vector[c(rep(5, 5), rep(1, 5))]
some_vector[c(1, 5, 10)][3] # third element of new vector

a <- c(10, 2, 20)
length(a)
sum(a)
cumsum(a)
prod(a)
cumprod(a)
mean(a)

min(a)
cummin(a)
max(a)
cummax(a)
range(a)

b <- c(pi, 2.71828, 6.52525)
round(b)
round(b, 2)
round(b, -1)
floor(b)
ceiling(b)

signif(1251.48, 5)
signif(1251.48, 4)
signif(1251.48, 3)
trunc(c(-2020.8, 2020.8))

l_part <- c(TRUE, FALSE)
mode(l_part)
r_part <- 1.25
mode(r_part)
(c1 <- c(l_part, r_part)) # TRUE: 1, FALSE: 0
mode(c1)

(c_part <- complex(real = -1, imaginary = 3))
mode(c_part)
(c2 <- c(c1, c_part))
mode(c2)
ch_part <- "Abc"
(c3 <- c(c2, ch_part)) # Note the double quotes
mode(c3)

c3[1] + c3[3]

two_numbers <- numeric(2)
two_numbers[] <- c(0, 5) # overwriting previous elements
as.logical(two_numbers) # 5 is not 0 and must then be 1
as.numeric(complex(real = 2, imaginary = 1))

b
b == pi
b != pi # ! always used for negation
b < 3
b[1] >= 3

b[1] > 3 & b[3] > 10 # TRUE if both are TRUE
b[1] < 3 | b[3] < 10 # TRUE if at least one is TRUE
xor(b[1] > 3, b[3] < 10) # TRUE if only one is TRUE

all(b > 1)
any(b > 20)

sum(1:1000 %% 3 == 0)

(check <- b != pi)
b[check] # only elements that are TRUE extracted
(1:length(b))[check]
which(b != pi)
which.min(b)
which.max(b)

names(b) <- c("Pi", "e", "Six_and_change")
b[c("Pi", "Six_and_change")]

TRUE & TRUE
T & T
T <- 0
T & T

number1 <- -1
number2 <- -1^3
number1 == number2
identical(number1, number2)
is.numeric(c(number1, number2))
1 == as.integer(1)
identical(1, as.integer(1))

(m1 <- matrix(data = 1:16, nrow = 4))

m2 <- matrix(16:1, ncol = 4)
m1 + m2

(m3 <- matrix(1:16, nrow = 4, byrow = TRUE))
t(m1)

m3[9] # default column-wise orientation
m3[1, 3]
m3[2, -1] # from matrix to vector
m3[2, -1, drop = FALSE]

m3[, 1:2]
m3[3:4, ]

to_manipulate <- seq(4, by = 4, length.out = 4)
m3[to_manipulate] <- 100 # recycling
m3

(m4 <- matrix(1:4, ncol = 2))
rbind(m4, c(3, 5))
cbind(m4, c(5, 6))

(m5 <- matrix(1:6, ncol = 3))
(m6 <- matrix(10:5, nrow = 3))
m5 %*% m6

(m7 <- matrix(c(1, 2, 2, 3), ncol = 2))
solve(m7)
m7 %*% solve(m7)

eigen(m7)

dim(m5)
nrow(m5)
ncol(m5)
length(m5) # matrix as vector
rowSums(m5)
colSums(m5)

diag(m3)
diag(m3) <- 3
m3
diag(2)

colnames(m5) <- c("Variable1", "Variable2", "Variable3")
ID <- c("I01", "I02")
(m5_new <- cbind(m5, ID))
mean(m5_new[, "Variable1"])

HairEyeColor

dim(HairEyeColor)
HairEyeColor["Brown", "Hazel", "Female"]
HairEyeColor["Blond", , "Female"]

HairEyeColor["Red", , ]
mode(HairEyeColor)
HairEyeColor[1, 1, 1] <- "50"
mode(HairEyeColor)

an_array <- array(1:20, dim = c(2, 5, 2))
data_vector <- 1:20
dim(data_vector) <- c(2, 5, 2)
all(an_array == data_vector)

an_array
