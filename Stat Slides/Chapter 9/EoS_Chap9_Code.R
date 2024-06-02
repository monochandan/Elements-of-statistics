################################################################################
# Elements of Statistics                                                       #
# Chapter 9: Hypothesis testing                                                #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 9.1 Fundamentals
####################\\\\\\\\\\//////////########################################

sigma <- 30; mu0 <- 210; alpha <- 0.05; n <- 81


C <- vector()
C[1] <- mu0 - qnorm(1-alpha/2)*sigma/sqrt(n)
C[2] <- mu0 + qnorm(1-alpha/2)*sigma/sqrt(n)
C


Prob_a <- pnorm(q = C[1], mean = mu0, sd = sigma/sqrt(n)) + 
  1 - pnorm(q = C[2], mean = mu0, sd = sigma/sqrt(n))
Prob_a


Prob_b <- pnorm(q = C[2], mean = 216, sd = sigma/sqrt(n)) - 
  pnorm(q = C[1], mean = 216, sd = sigma/sqrt(n))  
round(Prob_b, digits=2)


SpMean <- 3; SpVar <- 25
mu0 <- 5    ; alpha <- 0.05; n <- 100


Teststat <- (SpMean - mu0)/sqrt(SpVar) * sqrt(n)  
c_stat <- qnorm(p = alpha)
Teststat
c_stat
Teststat < c_stat


p_value <- pnorm(q = Teststat)
p_value


####################//////////\\\\\\\\\\########################################
# 9.2 One-sample tests
####################\\\\\\\\\\//////////########################################

x9_10 <- c(4.30, 4.30, 3.88, 6.81, 5.17, -0.53, 4.05, 3.34,
           6.07, 6.58, 6.94, 0.96, 6.14, 4.20, 8.35, 7.01,
           3.39, 3.96, 3.19, 4.21)
sigma <- sqrt(4)
mu0 <- 3
alpha <- 0.05
n <- 20
SpMean <- mean(x9_10)


Teststat <- (SpMean - mu0)/sigma * sqrt(n)  
c_stat <- vector()
c_stat[1] <- qnorm(p = alpha/2)
c_stat[2] <- qnorm(p = 1 - alpha/2)
round(Teststat, digits = 3)
Teststat < c_stat[1] | Teststat > c_stat[2]


t.test(x9_10,alternative="two.sided",mu=3,conf.level=0.95)


t.test(x9_10,alternative="less",mu=3,conf.level=0.95)


t.test(x9_10,alternative="greater",mu=3,conf.level=0.95)


load("Example9-12.RData")


SpMean <- mean(x9_12)
SpVar <- var(x9_12)
mu0 <- 200
alpha <- 0.05
n <- length(x9_12)
t.test(x = x9_12, alternative = "less", mu = mu0, 
       conf.level = 1 - alpha)


sigmaq0 <- 30


c_stat <- qchisq(p = 1 - alpha, df = n - 1) 
Teststat <- (n-1) / sigmaq0 * SpVar    
c_stat
Teststat
Teststat > c_stat


theta0 <- 0.25; alpha <- 0.05; n <- 100


n * theta0 * (1 - theta0) > 9
0.1 <= theta0 & theta0 <= 0.9


p <- 0.3
c_stat <- vector()
c_stat[1] <- qnorm(p = alpha/2)   
c_stat[2] <- qnorm(p = 1 - alpha/2) 
Teststat <- vector()
Teststat[1] <- (n*p+0.5-n*theta0) / sqrt(n*theta0*(1-theta0))  
Teststat[2] <- (n*p-0.5-n*theta0) / sqrt(n*theta0*(1-theta0))     
c_stat
Teststat
Teststat[1] < c_stat[1] | Teststat[2] > c_stat[2]


####################//////////\\\\\\\\\\########################################
# 9.3 Two-sample tests
####################\\\\\\\\\\//////////########################################

SpMean_X1 <- 31.5; SpMean_X2 <- 28.5    
SpVar_X1 <- 27.3  ; SpVar_X2 <- 23.8  
Cov_X1_X2 <- 15.6 
delta0 <- 5; alpha <- 0.05; n <- 50
n > 30


S_d2 <- SpVar_X1 + SpVar_X2 - 2 * Cov_X1_X2
Teststat <- (SpMean_X1 - SpMean_X2 - delta0) / 
  sqrt(S_d2) * sqrt(n) 
c_stat <- qnorm(p = alpha)  
S_d2
c_stat
Teststat
Teststat < c_stat


x9_17 <- c(3,5,8,7,9,12,17,10,5,9,14,22)
y9_17 <- c(17,25,9,21,12,14,10,12,14,10,16,12,19,14,28)
t.test(x = x9_17, y = y9_17, alternative="two.sided", 
       var.equal=FALSE, paired=FALSE, mu=0, conf.level=0.99)


t.test(x=x9_17,y=y9_17,alternative="less",var.equal=FALSE,
       paired=FALSE, mu=0, conf.level=0.99)


n1 <- 240 
n2 <- 160
p_X1 <- 60 / n1
p_X2 <- 24 / n2   
delta0 <- 0.05
alpha <- 0.05


(n1 * p_X1 * (1 - p_X1)) > 9 & (n2 * p_X2 * (1 - p_X2)) > 9
0.1 <= p_X1 & p_X1 <= 0.9
0.1 <= p_X2 & p_X2 <= 0.9
c_stat <- vector()
c_stat[1] <- qnorm(p = alpha/2)       
c_stat[2] <- qnorm(p = 1 - alpha/2)  
Teststat <- (p_X1-p_X2-delta0) / (sqrt((p_X1 * (1-p_X1)) / n1 
                                       + (p_X2 * (1-p_X2)) / n2))   
c_stat
Teststat
Teststat < c_stat[1] | Teststat > c_stat[2]


p <- (n1 * p_X1 + n2 * p_X2) / (n1 + n2)
Teststat_new <- (p_X1-p_X2) / sqrt(p*(1-p)*(1/n1+1/n2))
p
Teststat_new
Teststat_new < c_stat[1] | Teststat_new > c_stat[2]


load("Example9-19.RData")
var.test(x = x9_19,y = y9_19,ratio = 1,alternative="less",
         conf.level = 0.95)


####################//////////\\\\\\\\\\########################################
# 9.4 Chi-squared tests
####################\\\\\\\\\\//////////########################################

Sp9_20 <- c(40,45,80,55,45,35)
n <- 300
m <- 6
theta0 <- rep(x = 1/6, times = m)
alpha <- 0.05
m > 2
n >= 30
all(n * theta0 >= 1)
sum(n * theta0 < 5) / m <= 0.2


c_stat <- qchisq(p = 1-alpha, df = m-1)
chisq.test(x = Sp9_20, p = theta0 * n, rescale.p = TRUE,
           correct = FALSE) # ATTENTION: argument rescale.p


load("Example9-21.RData")
y9_21_kl <- table(y9_21)
n <- 1000
m <- 4 
theta0 <- c(159, 341, 341, 159) / n
alpha <- 0.05
m > 2
n >= 30
all(n * theta0 >= 1)
sum(n * theta0 < 5) / m <= 0.2


chisq.test(x = y9_21_kl, p = theta0 * n, rescale.p = TRUE, 
           correct = FALSE)


load("Example9-22.RData")
alpha <- 0.05 
n <- sum(Sp9_22)
m <- dim(Sp9_22)[1]
r <- dim(Sp9_22)[2]
theta0 <- margin.table(x = Sp9_22, margin = 1) %*%
  t(margin.table(x = Sp9_22, margin = 2)) / 
  margin.table(x = Sp9_22)


n >= 30
sum(theta0 == 0) == 0
sum(theta0 < 5) / (m*r) <= 0.2


c_stat <- qchisq(p = 1 - alpha, df = (m-1) * (r-1))
c_stat
chisq.test(x = Sp9_22, correct = FALSE, rescale.p = FALSE) 


load("Example9-23.RData")
alpha <- 0.05  
n<-sum(Sp9_23);m<-dim(Sp9_23)[1];r<-dim(Sp9_23)[2]
theta0 <- margin.table(x = Sp9_23, margin = 1) %*%
  t(margin.table(x = Sp9_23, margin = 2)) / 
  sum(x = Sp9_23)


n >= 30
sum(theta0 == 0) == 0
sum(theta0 < 5) / (m*r) <= 0.2


chisq.test(x = Sp9_23, correct = FALSE, rescale.p = FALSE)
c_stat <- qchisq(p = 1 - alpha, df = (m-1) * (r-1)) 
theta0_alternative <- chisq.test(x = Sp9_23, correct = FALSE,
                                rescale.p = FALSE)$expected
c_stat
theta0_alternative
