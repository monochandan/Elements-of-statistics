x <- c(1, 2, 3, 4, 5, 6)
x
mode(x)

l_part <- c(TRUE, FALSE)
mode(l_part)

r_part <- 1.25
mode(r_part)

(c1<- c(l_part, r_part))

mode(c1)

(c_part <- complex(real = 1, imaginary = 3))
mode(c_part)

(c2 <- c(c1, c_part))
mode(c2)

ch_part <- "Abd"
(c3 <- c(c2, ch_part))
mode(c3)

c3[1] + c3[3]


two_character <- numeric(2) ##having a vector for 2 numeric values
two_character ###empty now

two_character[] <- c(0, 2)
as.logical(two_character) ###forcing and converting to another type and 
#######for that we are also loosing the real information

as.numeric(two_character)

as.numeric(complex(real= 2, imaginary = 1)) ###imaginary part discarded..... so information lost


b <- c(pi, 2.71828, 6.52525)
b == pi #vectorized function and comparing every element in b
###vectorize means ---> it will compare with every values in the vector

#### non vectorize means ----> it will compare 

b != pi
b < 3
b[1] >= 3
b[1]>3 & b[3]<10

b[1]>3 & b[3]>10 ###false because one of this 2 condition are false

b[1] < 3 | b[3] < 10 #### true because one of this conditions are true

xor(b[1] > 3, b[3] < 10) ####false because both of them are true

xor(b[1]>3, b[3] >10) ### TRUE one of them are false


####### "&" and "|" operators are vectorize 
####### the non vectorize version is "&&" and "||",,,,, should use when the vector have only one value


all(b>3) ### all values of b are not greater than 3, that's why FALSE
any(b > 3) #### some value of b are greater than 3 that's why TRUE

1:50 %% 3 == 0

sum(1:50 %% 3 == 0)

check <- (b != pi)

check
b[check] ###giving the values , which are not equal to pi  
##because check vector stores the values by given condition

###if we want to get the locations of the TRUE values checked by check vector
(1:length(b))[check]

which(b != pi) ### giving the location of values which are not equal to pi
which.min(b) ###only gives location not values
which.max(b)

##set names for every elements in vector b

names(b)<-c("pi","e","six_and_change")

b
###printing vector values by the given name
b[c("pi","six_and_change")]

TRUE & TRUE

T & T
T <- 0
T & T


#### that's why prof saide that use the fuckig TRUE Not fucking T Cunt !!!!!!!

number1 <- -1
number2 <- -1^3
number1 == number2
identical(number1, number2)
is.numeric(c(number1, number2))

####difference between == and  identical
1 == as.integer(1) ### TRUE
###values are identical but the types are different 1 is double and as.integer(1) is integer so FALSE
####usinf g integer is good because it safes memory
identical(1, as.integer(1)) ### FALSE

mode(1)
mode(as.integer(1))



