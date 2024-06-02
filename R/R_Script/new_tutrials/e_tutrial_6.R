
########## 12 march ################################################

rm(list = ls())

N <- 1200
n <- 66

## 1.1
sum_x_i <- 19419
(pop_total_mean <- (N / n) * sum_x_i)

## 1.2

(sample_mean <- sum_x_i / n)
(var_x <- (1/(n-1)) * 1151235.59)
(var_sample_mean <- (1/n) * var_x * ((N-n)/(N-1)))

##1.3

alpha <- 1 - 0.95
sigma <- sqrt(var_sample_mean)
(ci_upper <- sample_mean + qnorm(p = 1 - (alpha/2)) * sigma)
(ci_lower <- sample_mean - qnorm(p = 1 - (alpha/2)) * sigma)

## 1.4
mean <- 289.72
var <- 266.41
sigma <- sqrt(var)
alpha <- 1 - 0.90
d <- 6

qntl <- qnorm(p = 1 - (alpha/2))
(n_min <- ceiling((2 * qntl * sigma/d)^2))




## 2.1
N <- 4600
n <- 200


sum_y_i <- 9994279 
(total_annual_gross_y <- (N/n) * sum_y_i)


#### 2.2
(var_y <- 1/(n-1) *  781691536)

(var_estimat_sample_mean <- 1/n * var_y )

(sample_mean_y <- 9994279 / n)

alpha <- 1 - 0.99

sigma <- sqrt(var_estimat_sample_mean)

(ci_lower <- sample_mean_y - qnorm(p = 1 - (alpha/2)) * sigma)



### 2.4

delta <- -9000

(sample_mean_y <- 9994279 / n)
(var_y <- 1/(n-1) *  781691536)
(var_estimat_sample_mean_y <- 1/n * var_y )

(sample_mean_x <- 7951906 / n)
(var_x <- 1/(n-1) *  901718794)
(var_estimat_sample_mean_x <- 1/n * var_x)

(z <- (sample_mean_x - sample_mean_y - delta) / sqrt(var_estimat_sample_mean_x + var_estimat_sample_mean_y))

## 3.1
(1/6)*4 + 9

## 3.2
4*(1/36) + 81

## 3.3
5 * (1/25)
#########################################
#########################
##############
######
###
##
#
###

N <- 1200
n <- 66
#sample_mean <- 19419
#sample_var <- 1151235.59

## Please use an unbiased estimator to reach an estimate for the population total
sample_mean <- 19419 / n
pop_tot_mean <- (N/n)*19419
round(pop_tot_mean,4)


## Please calculate a variance estimator for the sample mean

var_x <- (1/(n-1)) * 1151235.59
(var_sample_mean <- (1/n) * var_x * ((N-n)/(N-1)))
var_pop_total_mean <- (N^2 / n) * var_x * ((N-n) / (N-1))
round(var_pop_total_mean,4)


##  Please calculate the value of the upper bound of a two-sided 95 percent confidence interval for the sample mean

alpha<-0.05
(C.upper.sm<-sample_mean + qnorm(p=1-(alpha/2))*(sqrt(var_sample_mean)))
(C.lower.sm<-sample_mean - qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var_sample_mean))

C.upper.pop<-pop.tot.mean + qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.pop.tot.mean)
C.lower.pop<-pop.tot.mean - qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.pop.tot.mean)