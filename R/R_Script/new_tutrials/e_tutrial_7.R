rm(list = ls())

head(RData)





#################################### 12 marchh ############################

RData

##1.3

N_germany <- 340
N_usa <- 204

mean_germany <- mean(RData$turnover.Germany)
mean_usa <- mean(RData$turnover.USA)

n_germany <- length(RData$turnover.Germany)
n_usa <- length(RData$turnover.USA)


var_germany <- (1 /(n_germany - 1)) * sum((RData$turnover.Germany - mean_germany)^2)
var_usa <- (1/(n_usa - 1)) * sum((RData$turnover.USA - mean_usa)^2)

(var_mean_germany <- (1/(n_germany)) * var_germany )
(var_total_germany <- (N_germany^2*(1/n_germany)) * var_germany)
var_mean_usa <- (1/(n_usa)) * var_usa * ((N_usa - n_usa) / (N_usa - 1))

## 1.4
alpha <- 0.01

test <- t.test(RData$turnover.Germany, RData$turnover.USA, conf.level = 1 - alpha, mu = 1507.69, alternative = "two.sided" )
ans <- abs(test$statistic)
round(ans, 4)

test <- t.test(RData$turnover.USA, RData$turnover.Germany, conf.level = 1 - alpha, mu = -1507.69, alternative = "greater" )
ans <- abs(test$statistic)
round(ans, 4)



## 1.5
abs_test<-test$statistic
ans.e<-((mean_germany-mean_usa-1507.69)/abs_test)^2
ans.e

(test$stderr)^2




## 2.1


#################################
##################
#######
##
##
###


## you assume the average turnover in Germany to be exactly 2349.83. 
## You checked your assumption using a hypothesis test. Its results are depicted in the R output above.
## Please state the resulting test statistic.
t <- 2.7475


## To which of the following statements - regarding the test result shown in the R output
## - do you agree? In case the null hypothesis is rejected at several significance levels,
## always select the smallest one at which the null is still rejected.

p <- 0.008
p < 0.1
p < 0.001
p < 0.05


## The turnover figures of randomly drawn shops in Germany and in the USA are documented in the attached R file.
## Based on this information, please calculate an estimate for the variance of the estimated average turnover in Germany.
## Use sampling without replacement

N_germany <- 347
N_usa <- 219

mean_germany <- mean(RData$turnover.Germany)
mean_usa <- mean(RData$turnover.USA)

n_germany <- length(RData$turnover.Germany)
n_usa <- length(RData$turnover.USA)


var_germany <- (1 /(n_germany - 1)) * sum((RData$turnover.Germany - mean_germany)^2)
var_usa <- (1/(n_usa - 1)) * sum((RData$turnover.USA - mean_usa)^2)

(var_mean_germany <- (1/(n_germany)) * var_germany * ((N_germany - n_germany) / (N_germany - 1)))
var_mean_usa <- (1/(n_usa)) * var_usa * ((N_usa - n_usa) / (N_usa - 1))

#You assume that the average turnover in the USA is 1595.1 less than the average 
#turnover in Germany. Check your assumption using a hypothesis 
#test and a significance level of ??= 0.1. Please state the resulting absolute test value. 

test2<-t.test(turnover.USA,turnover.Germany,conf.level = 0.90,mu= -1595.1 , alternatives="less")
ans<-abs(test2$statistic)
ans


## You assume that the average turnover in Germany is 1578.51 more than the average turnover
## in the USA. Check your assumption using a hypothesis test and a significance level of ??= 0.01.
## Please state the resulting absolute test value.
alpha <- 0.01

test <- t.test(RData$turnover.Germany, RData$turnover.USA, conf.level = 1 - alpha, mu = 1578.51, alternative = "two.sided" )
ans <- abs(test$statistic)
round(ans, 4)

test2<-t.test(RData$turnover.USA, RData$turnover.Germany, conf.level = 0.90,mu= 1578.51 , alternatives="greater")
ans<-abs(test2$statistic)
ans
## Referring to your test result in exercise part d): please calculate the estimate 
## for the variance of the difference-in-means estimator.

abs_test<-test$statistic
ans.e<-((mean_germany-mean_usa-1578.51)/abs_test)^2
ans.e

(test$stderr)^2



###

rm(list = ls())
RData

n <- 69
N <- 1193

## Please calculate an estimate for the variance of the estimatior for
## the average weight of gummy bears in the sample. In order to do so, use sampling without replacement

(mean_weight <- mean(RData$weight))
(var_weight <- (1/(n-1)) * sum((RData$weight - mean_weight)^2))
(var_samp_weight <- (1/n) * var_weight * ((N-n) / (N-1)))
round(var_samp_weight, 4)

## If the variance of the weight of the gummy bears produced is not higher than 174.220135
## it follows that the plant produces with a sound level of accuracy. In order to test the precision of the machine,
## a hypothesis test is conducted (see R output). Please calculate the missing value in the R output

test_stat<-(n-1)/ 217.88758*var(RData$weight)
test_stat


## Please calculate the critical value at which the hypothesis stated in the R output is rejected.

alpha<-0.05
crit.value<-qchisq(p = alpha , df = (n - 1))
crit.value


## The plant has to produce equal shares of gummy bears of each colour. 
##Use the information given in the sample to test the hypothesis (??= 0.1) that
## the plant indeed produces the same amount of gummy bears in each colour. 
## Please state the resulting test statistic. 

(data<-table(RData$color))
(n<-length(RData$color))
theta<-rep(1/4,4)
theta

test_stat<-chisq.test(data,p=theta*n,rescale.p = TRUE,correct = FALSE)$statistic
test_stat
round(test_stat, 4)


## In case the null hypothesis is rejected at several significance levels,
## always select the smallest one at which the null is still rejected.

crit_reg <- qchisq(p = alpha, df = 3)
crit_reg

test_stat > crit_reg

0.1184 < 0.001

1-0.99 
0.008 < 0.01
0.008 < 0.5
