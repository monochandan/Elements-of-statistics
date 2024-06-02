



############################### 12 march ###################################

rm(list = ls())

##m 1.1
4 - (1/10)

## 1.2
8 + (1/10^2)

## 1.3
1/4

##2.1
(e_y1 <- 8*13 + 3*2 + 8*18 + 1*15)

## 2.2
k1 <- 7
k2 <- 5
(var_F <- ((2*(k1 + k2 - 2)) / (k1 *(k2 - 4))) * (k2 / (k2 - 2))^2)

## 2.3
k <- 5
e_x <- k
var_x <- 2*k
(qt(p = 0.99, df = 5))
(qnorm(p = 0.99, mean = k , sd = sqrt(var_x)))

## 2.4

(var_y1 <- (8^2 * 1) + (3^2 * 4) + (8^2 * 1) + (1^2 * 1))
(qnorm(p = 0.95, mean = e_y1, sd = sqrt(var_y1)))
##8^2
#############################################
##############################
##################
####
#1.1
((1/5) * 2) - 4

## 1.2
(1/25) + (1/25) + 16

## 1.3

1/9 + 1/9 + 1/9


## 2.1

(val <- 9*4 - 4*21 + 9*23 + 4) 

##2.2
k1 <- 5
k2 <- 10

(var <- ((2*(k1+k2-2)) / (k1 *(k2-4))) * (k2/(k2-2))^2)

## 2.4

(qt(p = 0.01, df = 2))

##
(var <- (9^2*1) +((-4)^2*49) + (9^2*49))
qnorm(p = 0.01, mean = val, sd = sqrt(var))

################################## 12 march #################################





###########################################
###########################
####################
########
###
##
#

## The expected value of the estimating function U is E(U)

.5 * (8 + 5+3-6)



### Please calculate the expected value of the random variable Y3

### F distribution

k1 <- 8
k2 <- 5

EX <- k2/(k2 - 2)
round(EX, 4)

## Please calculate the variance of the random variable Y1
### std. normal distribution

k <- 7
var_y1 <- k

## Please state the number of degrees of freedom of the random variable Y2.

## K squared distribution

k_3 <- 5
degree_y2 <- k_3


## Please state the 95%-quantile of the random variable Y4. 

## T distribution

k_t <- 5 ## degrees of fredom

round(qt(p = 0.05, df = k_t),4)

## Please state the 1%-quantile of the random variable Y2
## std. normal distribution
round(qnorm(p = 0.01, mean = 0, sd = sqrt(10)),4)




