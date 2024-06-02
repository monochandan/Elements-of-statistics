#1 How many multiples of 8 does the solution to the expression 463+(317???26)???12 contain (hint: integer division)?
expression <- 463 + (317 - 26) * 12
expression

array <-(1:expression)

multiple_8 <- array[array %% 8 == 0]
multiple_8

length(multiple_8)


#2 What is the remainder of the operation in a)?

expression %% 8

#3 What is the product of the natural numbers from 1 through 10?
product_i <- 1
for(i in 2:10){
  product_i <- product_i * i
}
product_i

#4 What is e5???e3?
ans <- exp(5) * exp(3)
ans


# What is the 237th element of a sequence from 4 through 2228 with an increment of 8?
sequence <- seq(4, 2228, 8)
sequence[237]


# How many elements does a sequence contain that has the following characteristics: each of the natural numbers from 37 through 59 is repeated 14 times?
new_sequence <- rep(seq(37, 59), times=14)
new_sequence
length(new_sequence)

# How many times does the number 1454 occur in the provided vector?
a_vector
a <- (a_vector %in% 1454)
a
a_vector[a]
length(a_vector[a_vector %in% 1454])
length(a_vector[a])

# What is the share of elements that are less than or equal to 7350 in the provided vector? Please state your result in percent and rounded to two decimal places.

a <- (a_vector <= 7350)
a
length(a_vector[a])

percentage <- length(a_vector[a]) / length(a_vector)
percentage
round(percentage * 100, digits = 2)

# Is the statement The vector contains an element that is 123 TRUE or FALSE? Please type the correct logical value (all caps) into the gap.

value <- 123
value %in% a_vector
is_contain
a_vector[(5140  == a_vector)]

# How many elements of the provided vector are greater than 1000 and less than 4000?

values <- seq(1001, 3999)
values
is_contain <- (values %in% a_vector)
is_contain
length(a_vector[is_contain])


# Which is the smallest element of the provided vector? Please do not provide a value but an index.

which.min(a_vector)


match()
a_vector[2222]
match(which.min(a_vector), a_vector)

# What is the value of this smallest element of the provided vector?
min(a_vector)



# sum of the matrix product

(m7 <- matrix(c(2,20,38,56,74,92,5,23,41,59,77,95,8,26, 44, 62, 80, 98, 11,29,47,65,83,101,14,32,50,68,86,104,17,35,53,71,89,107), ncol = 6))
m1 <- t(m7)

m1

m2 <- matrix(c(2, 9, 16, 23, 30, 37), ncol = 1)
m2


m3 <- m1 * as.vector(m2)
m3
m4 <- m1 %*% m2
m4
result <- sum(m4)
result

# What is the sum of the main diagonal elements of matrix A above?


n <- diag(m1)
n
sum(n)


####




#How many individuals does the provided data frame cover?

nrow(a_df)


# How many variables does the provided data frame contain?
ncol(a_df)


# How many data types does the provided data frame contain?
str(a_df)



#  What is the income of the 54th individual with an average education in the data set (original order of the individuals)?
#subset(df1, Imputed == TRUE)'
subset_df <- subset(a_df, Education == "Average")
subset_df[54,2]


# Please first classify the individuals in the data set according to their income as follows:
# 0 < X <= 30,000
# 30,000 < X <= 50,000
# 50,000 < X <= 75,000
# 75,000 < X <= 100,000
# X > 100,000
max(a_df$Income)
a_df$groupe <- cut(a_df$Income,
                     breaks = c(0,30000,50000,75000,100000, 1554800),
                     labels = c("First", "Second", "Third", "Fourth","Fifth"),
                     right = TRUE # intervals closed on the right
                     )

a_df$groupe
subset_of_income <- subset(a_df, a_df$groupe =="Fourth")
subset_of_income
nrow(subset_of_income)


# What is the maximum of the number of individuals aged 35 within a group?

subset_of_age <- subset(a_df, a_df$Age == 35)
subset_of_age
nrow(subset_of_age)
table(subset_of_age$groupe)
max(table(subset_of_age$groupe))

# What is the mode (i.e. data type) of the new variable you created in subproblem e)? 
# Please state your answer without quotation marks.

str(subset_of_age$groupe)

a_df$groupe

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))] 
}

getmode(a_df$groupe)
str(a_df$groupe)




dim(some_list)
dim(some_list)
length(some_list)


diag(4)

a_vector[1:10]
a_vector[-1:10]

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11.5,12,13,14,'hello')

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2))
print(result)



list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2), list("green",12.3))
list_data
abc <- list(c(1,2,3,4,5))
abc
is.list(abc[1])
abc[[c(3,4)]]


a <- as.numeric(factor(c(3, 1, 4, 1, 5))
a <- factor(c(3, 1, 4, 1, 5)          
a


a <- 1:50 %in% c(3, 10, 7) 
a
