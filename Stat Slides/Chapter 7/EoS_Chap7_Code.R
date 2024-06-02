################################################################################
# Elements of Statistics                                                       #
# Chapter 7: Selected distributions                                            #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 7.1 Selected discrete distributions
####################\\\\\\\\\\//////////########################################

x7_1 <- 0:3          
Theta7_1 <- 0.3 ; n <- 3
f_x7_1 <- dbinom(x7_1, size = n, prob = Theta7_1)
names(f_x7_1) <- x7_1
f_x7_1
Mean_X <- weighted.mean(x = x7_1, w = f_x7_1)
Mean_X
Mean_X <- n * Theta7_1


Var <- sum(f_x7_1 * (x7_1 - Mean_X)^2)
Var
Var <- n * Theta7_1 * (1 - Theta7_1)


x7_2 <- c(1, 0, 2); n <- 3                     
Theta7_2 <- c(0.3, 0.2, 0.5)          
f_1_0_2 <- dmultinom(x7_2, size = n, prob = Theta7_2)
f_1_0_2


x7_3 <- 0:3   
n <- 3            
N7_3 <- 100             
M7_3 <- 30              
Theta7_3 <- M7_3 / N7_3        

f_x7_3 <- dhyper(x7_3, m = M7_3, n = N7_3 - M7_3, k = n)
names(f_x7_3) <- x7_3
round(f_x7_3, digits = 3)


x7_4 <- 0:5
Lambda7_4 <- 0.1
f_x7_4 <- dpois(x = x7_4, lambda = Lambda7_4)
names(f_x7_4) <- x7_4       
F_x7_4 <- ppois(q = x7_4, lambda = Lambda7_4)
names(F_x7_4) <- x7_4
round(f_x7_4, digits = 6)
F_x7_4
Prob <- 1 - F_x7_4["2"]
Prob_old <- sum(f_x7_4[4:6])
round(Prob, digits=6) 
Prob_old


####################//////////\\\\\\\\\\########################################
# 7.2 Selected continuous distributions   
####################\\\\\\\\\\//////////########################################

x7_7 <- seq(from = -5, to = 10, by = 0.00001)
  
f_x7_7_1 <- dnorm(x = x7_7, mean = 0, sd = 1)
f_x7_7_2 <- dnorm(x = x7_7, mean = 5, sd = 1)
f_x7_7_3 <- dnorm(x = x7_7, mean = -3, sd = 0.35)
f_x7_7_4 <- dnorm(x = x7_7, mean = 5, sd = 2)
plot(x = x7_7, y = f_x7_7_1, type = "l", xlab = "x", 
     ylab = "f(x)", ylim = c(0, 1.2))
lines(x = x7_7, y = f_x7_7_2, type = "l", col = "blue")
lines(x = x7_7, y = f_x7_7_3, type = "l", col = "red")
lines(x = x7_7, y = f_x7_7_4, type = "l", col = "green")


F_x7_8 <- pnorm(q = c(1,1.96,3.29,-1,-1.96,-3.29), 
                mean = 0, sd = 1)
round(F_x7_8, digits = 4)


Prob7_8 <- pnorm(q=2,mean=0,sd=1) - pnorm(q=1,mean=0,sd=1)
round(Prob7_8, digits=4)


Phi7_9 <- pnorm(q = c(28,12,35.68,4.32,46.32,-6.32), 
                mean = 20, sd = sqrt(64))
round(Phi7_9, digits=4)
Phi7_9 <- pnorm(q = c(1,-1,1.96,-1.96,3.29,-3.29),
                mean = 0, sd = 1)
Prob7_9 <- pnorm(36,mean=20,sd=8) - pnorm(28,mean=20,sd=8)
round(Prob7_9, digits=4)


Quantile_z7_10 <- qnorm(p = c(0.8,0.9,0.95,0.15,0.01),
                        mean = 0, sd = 1)
Quantile_x7_10 <- qnorm(p = c(0.8,0.9,0.95,0.15,0.01),
                        mean = 6, sd= 2)
round(Quantile_z7_10, digits=2)   
round(Quantile_x7_10, digits=2)  
Quantile_x7_10_alternative <- 6 + 2 * Quantile_z7_10
round(Quantile_x7_10_alternative, digits=2) 


round(pchisq(q = 3.94, df = 10), digits = 2)
round(1 - pchisq(q = 15.987, df = 10), digits = 2)
round(pchisq(q = 20.48,df = 10)- pchisq(q = 4.87, df = 10), digits = 3)


Prob7_13 <- pchisq(q = 220, df = 200)
round(Prob7_13, digits = 4)


Prob7_13_1 <- pnorm(q = sqrt(2*220)-sqrt(2*200-1), 
                    mean = 0, sd = 1)
round(Prob7_13_1, digits = 4)
Prob7_13_2 <- pnorm((220-200)/sqrt(400), mean = 0, sd = 1)
round(Prob7_13_2, digits=4)


####################//////////\\\\\\\\\\########################################
# 7.3 Limit theorems and approximation conditions
####################\\\\\\\\\\//////////########################################

n <- 100
Theta7_14 <- 0.36

Mean7_14 <- n * Theta7_14 
Var <- n * Theta7_14 * (1 - Theta7_14)
Mean7_14 
Var


Prob7_14_1 <- pbinom(q = 40, size = n, prob = Theta7_14)
Prob7_14_2 <- pnorm(q = (40.5-Mean7_14)/sqrt(Var), 
                    mean = 0, sd = 1)
Prob7_14_3 <- pnorm(q = 40.5, mean = Mean7_14, sd = sqrt(Var))
round(Prob7_14_1, 4) 
round(Prob7_14_3, 4)


Prob7_14_4 <- 1 - pbinom(q=39, size=n, prob=Theta7_14)                          
Prob7_14_5 <- 1 - pnorm(q=(39.5-Mean7_14)/sqrt(Var),
                        mean=0,sd=1)                  
Prob7_14_6 <- 1 - pnorm(q=39.5,mean=Mean7_14,sd=sqrt(Var)) 
round(Prob7_14_4, 4) 
round(Prob7_14_6, 4)
  