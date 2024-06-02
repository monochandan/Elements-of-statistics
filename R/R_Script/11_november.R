print("hello world")

## inverse of exponent function

log(exp(15))

## absolute value of  number

abs(-15)

10 + 5 + 5

sum(10, 5, 5) ## to separate arguments in function, we use comma


### order of operations

10 * 9 + 1

1 + 9 * 10


10 * ( 9 + 1)

1 * 2  * 3 * 4* 5* 6 

factorial(6)
 

##### Section A
#####
10 * 5






####                                chapter 2  ###############

getwd()

## change working directory ---- path has to be relative to current working directory

setwd("C:/Users/looka/OneDrive/Documents/EOS/prep/R_Script")
getwd()

#### simple text file

sink("test.txt")
5 + 5
10 - 9
sink()

######## basic data structure
# (Atomic) vector ---  always column vector but in consoal it lookes like row vector

##option 1
c(1, 2,3, 4, 5, 6, 7, 8, 9)

##option 2
seq(from=1, to=10, by=1)

##option 3
1:10 


?seq
??Logistic

apropos("sink")

help.start()


seq(1,10,1) #by default

seq(10,1,1) # Error in seq.default(10, 1, 1) : wrong sign in 'by' argument

#option 1
seq(10,1,-1)

#option 2
seq(to=10, from=1, by=1)

seq(to=20, by=-5, length.out=5) ### desired length of the sequence should be non negative number


seq(from = 20, by = 5, length.out = 5)

seq(to = 5, by = 1) # default from value is 1

rep(29, times=4)
rep(c(2, 9), times = 4)
rep(c(2, 9), each = 4)
rep(c(2,9), times = 3, each = 3)

0:10 + 10:0 ## nth element of first vector + nth element of second vector added , computation does in elementwise
sqrt(c(1,16,36,144))

## recycle for size mismatch

1:10 + 10 ### 10 is repeated here

1:10 + 1:5 ## 1 2 3 4 5 6 7 8 9 10 + 1 2 3 4 5 -> 2 4 6 8 10 7 9 11 13 15

1:4 + 1:3 ##  1 2 3 4 + 1 2 3 = 2 4 6 5

x <- c(20,19,25,1)
x
y <- c(9, 10, 4, 28)
y
(z <- x + y)
ls() #### all stored in global environment ouput in vector

save(x, file="x.RData")
save(x,y,z, file="vector.Rdata")

## remove objects
rm(x)

load("x.RData")
x

some_name <- rep(1,4)
save(some_name, file="some_other_name.RData")
new_name <- get(load("some_other_name.RData"))
new_name

# rm(lst = ls()) whole desktop will be cleared

### indexing function

sort(x)
sort(x, decreasing = TRUE) # descending order

some_vector <- c(seq(2, 20, 2), 1:10, seq(3,30,3))
some_vector


x
order(x) # index values of the largest, smallest and other values
order(x, decreasing = TRUE)

### loke array

some_vector[1]
some_vector[1:10]
some_vector[-1:-2]
some_vector[20]
some_vector[-(1:20)] ## last index to 20th index value
some_vector[-1]
some_vector[c(5, 10, 15)] ## values of 5, 10 and 15 index

rm(some_vectir)

some_vector[c(rep(5, 4), rep(1, 5))] ## 5th element of  vector 4 times and 1st element of vector 5 times

### vector indices use recursively

some_vector[c(1, 5, 10)][3] # vector of 1st, 5th and 10th indexes elements and among them 3rd one

### usefull functions to analyse vectors

a <- c(10,2,20)
length(a)
sum(a) # 10 + 2 + 20 = 32 ----- vector of length 1

cumsum(a) # 10, 10+2 = 12, 12+20 = 32 ------ vector of length 3

prod(a)  ## length 1
cumprod(a) ### length 3

mean(a) ##  arithmetic mean

min(a)
cummin(a)

max(a)
cummax(a)

range(a)


b <- c(pi, 2.71828, 6.52525)
round(b)
round(b, digit = 2)
round(b, -1)
floor(b)
ceiling(b)

signif(1251.48, 5)

signif(1251.48, 3)

## round into the direction of 0
trunc(c(-2020.8, 2020.8))

























