(m1 <- matrix(1:16, nrow = 4, ncol = 4))
(m2 <- matrix(16:1, ncol = 4))

##### (m2 <- matrix(16:1)) ---< it will take it as a column vector (by default), so at we need to define row and column

m1 + m2

(m3 <-matrix(1:16, nrow = 4, byrow = TRUE)) ##### first 4 number will be in first row ----> printing by row


#######or#######
t(m1) ###transpose of a matrix

m3[9] ###9th vector value column wise ----
##1st column: 4(position 1,2,3,4) + 2nd column(position 1,2,3,4): 4 + 3rd column(position 1): 1 ----< 9th pos value is 3

######or######
m3[1,3]

m3[2, -2] #### print second rows all value but not printing the (2nd row 2nd column)value, which is 6

m3[2, -1, drop = FALSE] #####data printed in matrix form, not in vector form (by default)

#### matrix_name[row  : column]

m3[, 1:2] #### printing  1st and 2nd column of all row
m3[3:4,] #### printing  3rd and 4th row

###### overwriting objects
to_manupulate <- seq(4, by= 4, length.out = 4)
m3[to_manupulate] <- 100
####### vector wise : to _manipulate has 4, 8, 12, 16
####### in m3 matrix we will replace the values by 100 and the location will be 4, 8, 12, 16
m3

(m4 <- matrix(1:4, ncol= 2))
##added extra row
rbind(m4, c(3,5))
###added extra column
cbind(m4, c(5,6))


### matrix multiplication

(m5 <- matrix(1:6, ncol= 3))    ### row x n  == n x column
(m6 <- matrix(10:5, nrow = 3))

m5 %*% m6



(m7 <- matrix(c(1,2,2,3), ncol = 2))
solve(m7) ##gives us inverse of m7
###if it gives us identity matrix than solve() gives us inversable matrix
m7 %*% solve(m7) ## gives us identity matrix if the m7 is inversable 


eigen(m7)  ###### gives eigan values and eigan vectors

dim(m7) ###dimension of matrix

m5
nrow(m5)
ncol(m5)
length(m5)
rowSums(m5) ####  sum of every row
colSums(m5) #### sum of every column

m3

diag(m3) #### diagonal values of matrix

diag(m3) <- 3
m3

####Recycling
diag(2,2)
diag(2) ### identity matrix


m5
colnames(m5)<- c("Veriable1","Veriable2","Veriable3")
m5

####Information preserving behavior ------< data type with largest information content will be preserved.. 
#for this time (ID) character is largest
ID <- c("ID1","ID1")
(m5_new <- cbind(m5, ID))

mode(m5_new)
mean(m5_new[,"Veriable1"]) ##### ERROR because information preservation behavior,,, everything converted to character



### we can not represent more than  2 dimension in Matrix
#### So we need Array
?HairEyeColor

HairEyeColor

dim(HairEyeColor) #### hair is 4 by 4, eye is 4 by 4 and sex is 2 by 1

HairEyeColor["Black", "Brown","Female"]


##### eye colors of females with blond hair

HairEyeColor["Blond", , "Female"]

##### it's look like matrix  Because, we are working for 2 dimensional
HairEyeColor["Red",,]
HairEyeColor[,"Brown",]

mode(HairEyeColor)
##### Same problem as matrix
HairEyeColor[1,1,1] <- "50" #### changing  the value of given location but inserting character
HairEyeColor
######Data type will be changed
mode(HairEyeColor)
