x <- c(20, 19, 25, 1)
print(x)
y <- c(9, 10, 4, 28)
(z <-(x + y))

sort(x)
sort(x, decreasing = TRUE)

some_vector <- c(seq(1, 20, 2), 1 : 10, seq(3, 30, 3))
some_vector

x

## giving the index number for the value in ascending order
order(x)
## giving the index number for the value in descending order
order(x, decreasing = TRUE)

#reverse
rev(order(x))

some_vector[1]
some_vector[1:10]
### it is removing (only for this command... just hiding) first 20 vector and showing the last remaining vectors
some_vector[-(5:20)] ### !!!!!parenthesis is important ()



##### pick vector values from given location
some_vector[c(1,3,6,9,11,13)]

### now repeat the vectors from given location
some_vector[c(rep(1,3), rep(3, 3), rep(6,3), rep(9,3), rep(11, 3), rep(13, 3))]


### choosing the vector values form given locations
##### 1,2,3,4,5,6.... we are taking 6th ( 13 ) index from the selected indexes 
##### and printing the stored value on that index
some_vector[c(1,3,6,9,11,13)][6] 


###### analyze vectors

a <- c(10, 2, 20)
sum(a)
length(a)

###cumulative sum
cumsum(a)

prod(a)
cumprod(a)
###after every element calculate the max value e.g - after 1th elem. 10 is big
##### after 2th element also 10 is big
##### after 3rd element 20 is big
cummax(a)
#### same as min but calculate the minimum value after every index
cummin(a)

cummax(1:10)

min(a)

max(a)

mean(a)

range(a)

b <- c(pi, 2.71828, 6.52525)
round(b)

round(b, digits = 2)# cut everything after 2 digits decimal point

round(b,  digits = -1)#
####lower integer
floor(b)
####next bigger integer
ceiling(b)

#### round to some significant digit

signif(1257.256, 5)
signif(1245.233, 4)
signif(1257.256, 3)
signif(1253.556, 3)

#### round to next integer towards 0
trunc(c(-2020.8, 2020.8))