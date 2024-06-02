print("hello world")
print(15 %% 7)
print(15%/% 7)
print(sqrt(15))
print(10 ^5)
print(exp(1))
print(log(exp(15)))
print(abs(-15))
print(sum(10,15,-2))
print(9 * 10 + 1) #order
print(10*(9+1))
print(factorial(6))

##### Section A
#####
10  + 5
10 - 5
###### Section B
#####



##### Set new directory
#####

getwd()
setwd("C:/Users/looka/OneDrive/Documents/EOS/Stat_with_R/R_Script/my_R")
getwd()

#####

##### ############ calculate data and show and store it to the txt file
#####
sink("Test.txt")
5 - 10
10 - 10
0 + -0
10 + 2 / 8
sink()
#####

##### Vector
######
c(1, 2, 3, 4, 5, 6, 7, 8)
seq(from= 1, to = 10, by = 1)
1:10
#####

###### to get help
#####

?seq
??Logistic
apropos("sink")

help.start()

apropos("seq")
#####

##### sequence
#####
seq(1, 10, 1)

seq(from= 10, to= 1, by = -1) #reverse sequence

seq(to = 20, by= -5, length.out = 5) ####by default from = 1
seq(to = 5, by = 1) #default argument for to and from is 1


##### repeat
######
##### each --> taking each seperate number from vector
##### times --> taking each vactor and printing n times

rep(29,5)
rep(c(2, 9),times= 5)
rep(c(2,9), each = 4)
rep(c(2,9), each = 4, times = 2)
rep(c(2,9), times = 2, each = 4)

###### vectorization
#####

0 : 10

10 : 0

0 : 10 + 10 : 0

sqrt(c(1, 6, 36, 16, 144)) ## sqrt is vectorize function

##### Recycling
#####

1 : 10 + 10
1 : 10 + 1 : 5

1 : 10 + 1 : 3 
#####

##### Assignment operator
#####
x <- c(20, 19, 25, 1)
print(x)
y <- c(9, 10, 4, 28)
(z <-(x + y))

#####

##!!!!!!!case sensitive!!!!!!!

###to see current contents
ls()### it is also a vector

x

###save as external data set in same folder
save(x, file="x.Rdata")
save(x,y,z, file = "Vector.Rdata")

### Remove the vector if it is not useful anymore 
rm(x)
ls()

##### again load the vector data
### !!!!!important the data (x.Rdata) should be in the same file of the R script
load("x.Rdata")
x
#### Load data from one file and save to another file
some_name <- rep(1,5)
some_name
save(some_name, file= "some_other_name.Rdata")

new_name <- get(load("some_other_name.Rdata"))
save(new_name, file="some_other_name_again.Rdata")

ls()

rm(some_other_name)

new_vec <- get(load("some_other_name.Rdata"))
new_vec

###### remove everything from desktop

######rm(list = ls())









