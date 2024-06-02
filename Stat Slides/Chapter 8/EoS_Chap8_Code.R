################################################################################
# Elements of Statistics                                                       #
# Chapter 8: Estimation                                                        #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 8.1 Fundamentals
####################\\\\\\\\\\//////////########################################

mu <- 5
sigma <- sqrt(4)
n <- 10

Mean_U1 <- 1/n * (n * mu)
Mean_U2 <- 1/(n + 1) * (2 * mu + 9 * mu)
Mean_U3 <- 1/(n + 6) * (2 * mu + 2 * mu + 8 * mu)
Mean_U4 <- 1/2 * (2 * mu)

Means<-cbind(Mean_U1,Mean_U2,Mean_U3,Mean_U4)

Means


Var_U1 <- (1/n)^2 * (n * sigma^2)
Var_U2 <- (2/(n + 1))^2 * sigma^2 + (1/(n + 1))^2 * 
  (9 * sigma^2)
Var_U3 <- (2/(n + 6))^2 * (2 * sigma^2) + 
  (1/(n + 6))^2 * (8 * sigma^2)
Var_U4 <- (1/2)^2 * (2 * sigma^2)
Variances <- cbind(Var_U1,Var_U2,Var_U3,Var_U4)
Variances


x8_1 <- seq(from = 0, to = 10, length.out = 1000)

f_x8_1_1 <- dnorm(x = x8_1, mean = Mean_U1,
                  sd = sqrt(Var_U1))
f_x8_1_2 <- dnorm(x = x8_1, mean = Mean_U2, 
                  sd = sqrt(Var_U2))
f_x8_1_3 <- dnorm(x = x8_1, mean = Mean_U3, 
                  sd = sqrt(Var_U3))
f_x8_1_4 <- dnorm(x = x8_1, mean = Mean_U4, 
                  sd = sqrt(Var_U4))

plot(x = x8_1, y = f_x8_1_1, type = "l", xlab = "x",
     ylab = "f(x)", ylim = c(0,1), lwd = 2, col = "blue")
lines(x=x8_1,y=f_x8_1_2,type="l",lwd=2,col="green")
lines(x=x8_1,y=f_x8_1_3,type="l",lwd=2,col="red")
lines(x=x8_1,y=f_x8_1_4,type="l",lwd=2,col="black")
abline(v = 5)


####################//////////\\\\\\\\\\########################################
# 8.2 Properties of estimating fuctions
####################\\\\\\\\\\//////////########################################

Mean_U3 <- (n + 2)/(n + 6) * mu
Mean_U3
Bias_U3 <- Mean_U3 - mu
Bias_U3
n_new <- 10000
Bias_U3_new <- (n_new + 2)/(n_new + 6) * mu - mu
round(Bias_U3_new, digits = 4)


####################//////////\\\\\\\\\\########################################
# 8.3 Methods to gain estimating functions
####################\\\\\\\\\\//////////########################################

x8_9 <- 4
n <- 10
theta <- seq(from = 0.34, to = 0.46, by = 0.02)
theta


f_x8_9 <- dbinom(x = x8_9, size = n, prob = theta)
round(f_x8_9, digits = 4)


theta_hat <- theta[which.max(f_x8_9)]
theta_hat


Log_Likelihood <- expression(n * p * log(Theta) +
                               n * (1 - p) * log(1 - Theta))
D_Log_Likelihood <- D(expr = Log_Likelihood, name = "Theta")
Log_Likelihood
D_Log_Likelihood


####################//////////\\\\\\\\\\########################################
# 8.4 Interval estimation
####################\\\\\\\\\\//////////########################################

alpha <- 0.05
sigma <- 30
n <- 36


SpMean <- 72
CI <- vector()
CI[1] <- SpMean - qnorm(p = 1 - (alpha/2))*(sigma/sqrt(n))
CI[2] <- SpMean + qnorm(p = 1 - (alpha/2))*(sigma/sqrt(n))
CI_lower_alternative <- SpMean + qnorm(p = alpha/2) * 
  (sigma/sqrt(n))
round(CI, digits = 1)                        


alpha <- 0.05
SpMean <- 72
SpVar <- 33^2
n <- 36
CI <- vector()
CI[1] <- SpMean - qt(p = 1 - (alpha/2), df = n - 1) * 
  sqrt(SpVar/n)
CI[2] <- SpMean + qt(p = 1 - (alpha/2), df = n - 1) * 
  sqrt(SpVar/n)
round(CI, digits = 1)                    


alpha <- 0.1
n <- 25
SpVar <- 7.244
CI <- vector()
CI[1] <- ((n - 1) * SpVar) / 
  qchisq(p = 1 - alpha/2, df = n-1)
CI[2] <- ((n - 1) * SpVar) /
  qchisq(p = alpha/2, df = n-1)
round(CI, digits = 3)         

n <- 36
alpha <- 0.05
VarX <- 30^2
CI_new <- vector()
CI_new[1] <- SpMean - qnorm(p=1-alpha/2) * sqrt(VarX/n)
CI_new[2] <- SpMean + qnorm(p=1-alpha/2) * sqrt(VarX/n)
round(CI_new, digits = 1)


alpha <- 0.01
n <- 100
p <- 15/100
n * p * (1 - p) > 9
0.1 <= p & p <= 0.9
CI <- vector()
CI[1] <- p - qnorm(p = 1 - alpha/2)*sqrt((p * (1 - p))/n)
CI[2] <- p + qnorm(p = 1 - alpha/2)*sqrt((p * (1 - p))/n)
round(CI, digits = 3)


alpha <- 0.05
Quantile <- qnorm(p = 1 - alpha/2)
sigma <- 30
d <- 5
n_min <- ceiling((2 * Quantile * sigma/d)^2) 
n_min
