# 01 Aritmetic----

### How many multiples of 6 does the solution to the expression 415+(213−72)⋅7 contain (hint: integer division)?----

expression <- 463 + (317 - 26) * 12
expression

expression %/% 8 # division

array <- (1:expression)
array
Multipliers <- array[array %% 8 == 0]
Multipliers
length(Multiplies)

### What is the remainder of the operation in a)?----

left <- expression %% 6
left

### What is the sum of the square roots of the following numbers: 196, 784, 961?----

variables <- c(196,784,961)
sum(sqrt(variables))


### What is the natural logarithm of 39?----

log(39)


# 02 - Vectors - Sequences I----


### What is the 222nd element of a sequence from 22 through 1138 with an increment of 4?----

(sequence1 <- seq(22,1138,4))
(asd <- seq(from = 22, to= 1138, by=5))
  sequence1[222]

### How many elements does a sequence contain that has the following characteristics: each of the natural numbers from 47 through 83 is repeated 11 times----

sequance2 <- rep(seq(47,83), times= 11)
sequance2
length(sequance2)

#-sequence1[sequence1 %in% sequance2]
#-length(sequence1[sequence1 %in% sequance2])



# 03 - Vectors - Sequences II----
### Example of sequences----
seq(length.out = 7, by = -1, to = 1 )
rep(1:3, each = 2, times = 3)
rep(1:3, 4)




# 04 - Vectors - Data types I----
### Please order the different data types in ascending order of their information content.----

# Data type Example Output of mode
# Empty set (;) NULL "NULL"
# Logical values TRUE "logical"
# Integers and real numbers 1.25 "numeric"
# Complex numbers 2+1i "complex"
# (Strings of) characters "Hello world!" "character"
# The information content of the shown data types increases with every line



# 05 - Vectors - Data types II----
### Please match the data type of the resulting vectors (on the right) to the function calls (on the left) below. (5 points per correct match)----

a <- c(7, 3, complex(real = -1, imaginary = 3), 5)
b <- c(pi, 10, exp(1), 9)
d <- c("1", "2", "3", "4") 
e <- c(FALSE, FALSE, TRUE, FALSE)



# 06 - Vectors - Logical operators----
### How many times does the number 1454 occur in the provided vector?----
a <- (a_vector %in% 1454)
a_vector[a]
length(a_vector[a_vector %in% 1454])

### What is the share of elements that are less than or equal to 7350 in the provided vector? Please state your result in percent and rounded to two decimal places.----
a <- (a_vector <= 7350)
a_vector[a]
length(a_vector[a])
percentage <- length(a_vector[a])/length(a_vector)
round(percentage*100, digits= 2)

### Is the statement The vector contains an element that is 123 TRUE or FALSE? Please type the correct logical value (all caps) into the gap.----
b <- 123
b %in% a_vector
a_vector[(b == a_vector)]

### How many elements of the provided vector are greater than 1000 and less than 4000?----
d <- seq(1001,3999)
e <- (a_vector %in% d)
length(a_vector[e])


### Which is the smallest element of the provided vector? Please do not provide a value but an index.----
d <- seq(1001,3999)
f <- which(a_vector %in% d)
length(f)


which.min(a_vector)

### What is the value of this smallest element of the provided vector?----
#-g <- a_vector[(a_vector %in% d)]
#-min(g)
min(a_vector)
a_vector

# 07 - Matrices----
### What is the sum of the elements of the result of the matrix product of the matrices A and B above?----

k <- seq(2,107,3)
m1 <- matrix(k, nrow= 6, ncol= 6)
m1

l <- seq(2,37,7)
l
m2 <- matrix(l, ncol = 1)
m2

(pre_result <-  m1 %*% m2)
result <- sum(pre_result)
result

### What is the sum of the main diagonal elements of matrix A above?----

n <- diag(m1)
sum(n)



# 08 - Arrays----
### How many dimensions does the array representation of the dataset HairEyeColor (part of R's base installation) have?----

HairEyeColor
dim(HairEyeColor)
length(dim(HairEyeColor))

### How many male students with red hair and blue eyes does the dataset contain?----
str(HairEyeColor)
HairEyeColor["Red","Blue","Male"]

### How many students with black hair and green eyes does the dataset contain?----

sum(HairEyeColor["Black","Green",])

### How many female students does the dataset contain?----

sum(HairEyeColor[,,"Female"])


1:50 %in% c(3, 10, 7)



# 09 - Multiple Choices----
#Please only check the correct statements below. (2 points per correct choice)----

# FALSE #   You cannot source a complete script in RStudio.
# TRUE # 	%/% is the operator for integer division in R.
# FALSE # 	In R, function arguments are separated by semicolons.
# TRUE #  is the comment symbol in R.
# FALSE # 	Code between two calls of the function sink will be printed to the console.
# FALSE # 	You have to use argument names when you use the default argument order in function calls.
# FALSE # 	R will not use recycling if the shorter vector's length is not a multiple of the longer vector's length.
# TRUE # 	You cannot assign the name break to an object in R.
# TRUE # 	The function sort sorts a vector in ascending order by default.
# FALSE # 	Matrices are filled by row by default.