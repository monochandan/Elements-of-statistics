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
var_sample_mean <- (1/n) * var_x * ((N-n)/(N-1))
var_pop_total_mean <- (N^2 / n) * var_x * ((N-n) / (N-1))
round(var_pop_total_mean,4)


##  Please calculate the value of the upper bound of a two-sided 95 percent confidence interval for the sample mean

alpha <- 0.05

(c_upper_smp <- sample_mean + qnorm(p = 1 - (alpha / 2), mean = 0, sd = 1) * sqrt(var_sample_mean))
(c_lower_smp <- sample_mean - qnorm(p = 1 - (alpha / 2), mean = 0, sd = 1) * sqrt(var_sample_mean))

(c_upper_pop <- pop_tot_mean + qnorm(p = 1 - (alpha / 2), mean = 0, sd = 1) * sqrt(var_pop_total_mean))
(c_lower_pop <- pop_tot_mean - qnorm(p = 1 - (alpha / 2), mean = 0, sd = 1) * sqrt(var_pop_total_mean))


## From another sample you know that x¯¯¯= 289.72 and s2= 266.41. Please determine 
## the minimal sample size needed to ensure a two-sided 90 percent confidence interval 
## which is not longer than 6. Please assume sampling with replacement here


mean = 289.72
var = 266.41

alpha <- 0.10
quantile <- qnorm( p = 1 - (alpha/2))
sigma <- sqrt(266.41)
d <- 6
n_min <- ceiling(( 2 * quantile * sigma/d)^2)

round(n_min,4)


### 

N <- 3900
n <- 160

### Please use an unbiased estimator to estimate the total of the annual gross salaries of the graduates of university Y

sample_mean_x <- 6396000  / n
sample_mean_y <- 8032448 / n

pop_tot_mean_x <- (N/n) * 6396000
(pop_tot_mean_y <- (N/n) * 8032448)


##  Please calculate the upper bound of a two-sided 95 percent confidence 
## interval for the average annual gross salary of the graduates of university X.

alpha <- 1 - 0.95


var_x <- (1/(n-1)) * 622097138
var_y <- (1/(n-1)) * 601129632

var_sample_mean_x <- (1/n) * var_x ### with replacement so no charaction factor
var_sample_mean_y <- (1/n) * var_y

var_pop_total_mean_x <- (N^2 / n) * var_x
var_pop_total_mean_y <- (N^2 / n) * var_y


(c_lower_x_sm <- sample_mean_x - qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_sample_mean_x))
(c_upper_x_sm <- sample_mean_x + qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_sample_mean_x))



c_lower_y_sm <- sample_mean_y - qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_sample_mean_y)
c_upper_y_sm <- sample_mean_y + qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_sample_mean_y)


c_lower_pop_tot_x <- pop_tot_mean_x - qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_pop_total_mean_x)
c_upper_pop_tot_x <- pop_tot_mean_x + qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_pop_total_mean_x)

c_lower_pop_tot_y <- pop_tot_mean_y - qnorm(p = 1 - (alpha/2), mean = 0, sd = 1) * sqrt(var_pop_total_mean_y)
c_lower_pop_tot_y <- pop_tot_mean_y - qnorm(p = 1 - (alpha / 2), mean = 0, sd = 1) * sqrt(var_pop_total_mean_y)


## You assume that the salaries differ significantly across the two universities. 
## Test the hypothesis that the difference between the average annual gross salary of university X 
## and that of university Y is significantly smaller than -11000. Please calculate the test statistic
## corresponding to your hypothesis. Please note: The variances of the two samples are unequal!

delta <- -11000

round(test_stat <- (sample_mean_x - sample_mean_y - delta) / sqrt(var_sample_mean_x + var_sample_mean_y), 4)
(c_stat <- qnorm(p = alpha))





### The expected value of the estimating function U is E(U)

(Ex_u <- (1/ 5) * (2+7+9-10))



# The variance of the estimating function U is Var(U)

(Vra_U <- (1/5)^2 * (4 + 49 + 81 + (-10)^2))

## The estimating function X¯=1n???i=1nXi is an unbiased estimator for the expected value of a
## population characteristic. Please calculate the variance of this estimator in the case of n=3.
## The variance of the estimator X¯ is Var(X¯)

(var_x_bar <- (1/n)^2 * n^2)

## The estimating function X¯=1n???i=1nXi is an unbiased estimator for the expected value of a population characteristic. Please calculate the variance of this estimator in the case of n=3. The variance of the estimator X¯ is Var(X¯)