################################################################################
# Elements of Statistics                                                       #
# Chapter 10: Regression analysis                                              #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 10.1 Preliminaries
####################\\\\\\\\\\//////////########################################

x10_1 <- c(1010, 1200, 1400, 980, 850, 1640,
           1320, 880, 1500, 900, 1980, 1820)
y10_1 <- c(550, 650, 700, 510, 320, 780,
           680, 390, 760, 420, 850, 810)
plot(x = x10_1, y = y10_1)


x10_2 <- c(20.3, 31.5, 40.5, 19.6, 25.0,
           10.5, 30.0, 35.1, 50.3, 45.0)
y10_2 <- c(45.0, 70.3, 78.3, 44.0, 55.0,
           27.0, 65.0, 71.0, 98.0, 95.0)
plot(x = x10_2, y = y10_2)


x10_3 <- c(0.815, -1.342, 1.38, -1.435, 0.936,
           0.45, -0.786, -0.09, 0.314, -0.93,
           -0.012, 1.145, 0.184, 0.732, -1.781,
           0.114, 0.966, -0.865, -0.564, 0.696)
y10_3 <- c(-1.425, -0.381, -1.493, -1.329, -0.393,
           0.893, 0.716, -0.273, 0.943, -0.031,
           -0.428, 1.199, -0.919, 0.1, 2.469,
           0.493, 0.387, -1.171, -0.853, 0.868)
plot(x = x10_3, y = y10_3)


####################//////////\\\\\\\\\\########################################
# 10.2 Simple linear regression model
####################\\\\\\\\\\//////////########################################

SpMean_x <- mean(x10_2); SpMean_y <- mean(y10_2)
SpMean_x
SpMean_y


reg_analysis <- lm(formula = y10_2 ~ x10_2)
summary(reg_analysis)


b <- summary(reg_analysis)$coeff[2,1]   
a <- summary(reg_analysis)$coeff[1,1] 
b
a
r <- sqrt(summary(reg_analysis)$r.squared)
r


x_0 <- 42.5
y_hat_0 <- predict(object = reg_analysis, 
                   newdata = data.frame(x10_2 = x_0))

y_hat_0
plot(x = x10_2, y = y10_2)
abline(reg = reg_analysis, col = "blue") # Point not shown

r_q <- summary(reg_analysis)$r.squared  
r_q


####################//////////\\\\\\\\\\########################################
# 10.3 Inferential statistical properties of OLS
####################\\\\\\\\\\//////////########################################

Std_a <- summary(reg_analysis)$coeff[1,2]
Std_b <- summary(reg_analysis)$coeff[2,2] 
Std_e <- summary(reg_analysis)$sigma
round(Std_a,4) 
round(Std_b,4)
round(Std_e,4)


alpha <- 0.05
CI_a_and_b <- confint(object = reg_analysis, 
                      level = 1 - alpha
)
CI_a_and_b


n <- length(x10_2)
df <- summary(reg_analysis)$df[2]

CI_sigma_epsilon <- vector()
CI_sigma_epsilon[1] <- ((n - 2) * Std_e^2) / 
  qchisq(p = 1-(alpha/2), df = df)
CI_sigma_epsilon[2] <- ((n - 2) * Std_e^2) / 
  qchisq(p = (alpha/2), df = df)
CI_sigma_epsilon


x_0 <- c(35, 50)
CI_Mean_Obs <- predict(reg_analysis, 
                       newdata = data.frame(x10_2 = x_0), 
                       interval = "confidence")

CI_Obs <- predict(reg_analysis, 
                  newdata = data.frame(x10_2 = x_0), 
                  interval = "prediction")
CI_Mean_Obs
CI_Obs


summary(reg_analysis)


alpha <- 0.05      
Teststat_a <- summary(reg_analysis)$coeff[1,3]  
p_value_a <- summary(reg_analysis)$coeff[1,4]
c_stat_a <- vector()
c_stat_a[1] <- qt(p = alpha/2, df = df)    
c_stat_a[2] <- qt(p = 1-alpha/2, df = df)
Teststat_a  
c_stat_a[1]
c_stat_a[2]
Teststat_a < c_stat_a[1] | Teststat_a > c_stat_a[2]
p_value_a < alpha  
0 < CI_a_and_b[1,1] | 0 > CI_a_and_b[1,2]


Teststat_b <- summary(reg_analysis)$coeff[2,3]  
p_value_b <- summary(reg_analysis)$coeff[2,4]
c_stat_b <- vector()
c_stat_b[1] <- qt(p = alpha/2, df = df)    
c_stat_b[2] <- qt(p = 1-alpha/2, df = df)
Teststat_b 
c_stat_b[1]
c_stat_b[2]
Teststat_b < c_stat_b[1] | Teststat_b > c_stat_b[2]
p_value_b < alpha  
0 < CI_a_and_b[2,1] | 0 > CI_a_and_b[2,2]


b0 <- 1 
Teststat_b0 <- (b - b0) / Std_e * 
  sqrt(sum((x10_2 - SpMean_x)^2))      
c_stat_b0 <- qt(p = 1-alpha, df = df)
Teststat_b0 
c_stat_b0
Teststat_b0 > c_stat_b0


####################//////////\\\\\\\\\\########################################
# 10.4 The multiple linear regression model
####################\\\\\\\\\\//////////########################################

g10_8 <- factor(x = c("w", "w", "m", "w", "m", "w", "w", 
                      "m", "m", "w"), levels = c("m", "w"))
reg_analysis <- lm(formula = y10_2 ~ x10_2 + g10_8)
summary(reg_analysis)
