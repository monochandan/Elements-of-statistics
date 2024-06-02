################################################################################
# Elements of Statistics                                                       #
# Chapter 4: Methods of Data Collection and Visualisation                      #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 4.1 Preliminaries
####################\\\\\\\\\\//////////########################################

load("Example2-8.RData")
round(FQtable["Female", ] / FQtable["Female", "Sum"], 4)


load("Example4-2.RData")


F_j_k <- t(apply(apply(p_j_k, 2, cumsum), 1, cumsum))
F_j_k


####################//////////\\\\\\\\\\########################################
# 4.2 Measures of contingency and correlation
####################\\\\\\\\\\//////////########################################

load("Example4-4.RData")
addmargins(n_j_k)
n_j_k_star <- margin.table(n_j_k, 1) %*% t(margin.table(n_j_k, 2)) / margin.table(n_j_k)
n_j_k_star


chisq <- summary(n_j_k)$statistic
chisq


KK <- sqrt(chisq / (sum(n_j_k) + chisq))
M <- min(dim(n_j_k))
K_max <- sqrt((M - 1) / M)
K_star <- KK / K_max
KK
M
K_max
K_star


load("Example4-5.RData")
cor_SP <- cor(Rg_x5_5, Rg_y5_5, method = "spearman")
cor_SP


load("Example4-6.RData")
Rg_x5_6_mean <- c(3.0, 13.0, 35.5)
Rg_y5_6_mean <- c(2.5, 13.5, 36.5)


n <- sum(n_j_k)
Rx_Ry_sum <- sum(n_j_k[1, ] * Rg_x5_6_mean[1] * Rg_y5_6_mean) +
             sum(n_j_k[2, ] * Rg_x5_6_mean[2] * Rg_y5_6_mean) +
             sum(n_j_k[3, ] * Rg_x5_6_mean[3] * Rg_y5_6_mean)
Rx_sum <- sum(n_j_k * Rg_x5_6_mean)
Ry_sum <- sum(t(n_j_k) * Rg_y5_6_mean)
Rx_2 <- sum(Rg_x5_6_mean^2 * margin.table(n_j_k, 1))
Ry_2 <- sum(Rg_y5_6_mean^2 * margin.table(n_j_k, 2))
cor_SP <- (Rx_Ry_sum - 1/n * Rx_sum * Ry_sum) /
          (
          sqrt(Rx_2 - 1/n * Rx_sum^2) *
          sqrt(Ry_2 - 1/n * Ry_sum^2)  
          )
round(cor_SP, 4)


nc <- sum(
 n_j_k[1, 1] * n_j_k[2:3, 2:3],
 n_j_k[1, 2] * n_j_k[2:3, 3],
 n_j_k[2, 1] * n_j_k[3, 2:3],
 n_j_k[2, 2] * n_j_k[3, 3]
 )
nd <- sum(
 n_j_k[3, 1] * n_j_k[1:2, 2:3],
 n_j_k[2, 1] * n_j_k[1, 2:3], 
 n_j_k[3, 2] * n_j_k[1:2, 3],
 n_j_k[3, 1] * n_j_k[2,2]   
 )
nc
nd


Tx <- sum(n_j_k[ , 1] * n_j_k[, 2:3], n_j_k[, 2] *n_j_k[, 3])
Ty <- sum(t(n_j_k)[, 1] * t(n_j_k)[, 2:3], t(n_j_k)[, 2] * t(n_j_k)[, 3])
Tx
Ty


n <- sum(n_j_k)
M <- min(dim(n_j_k))

tau_a <- (nc - nd) / (1/2 * n * (n - 1))
tau_b <- (nc - nd) / sqrt((nc + nd + Tx) * (nc + nd + Ty)) 
tau_c <- 2 * M * (nc - nd) / (n^2 * (M - 1))
gamma <- (nc - nd) / (nc + nd)
round(tau_a, 4)
round(tau_b, 4)
round(tau_c, 4)
round(gamma, 4)


x4_8 <- c(1, 1.5, 2.5, 3, 4, 4, 5, 5.5, 6.5, 7)
y4_8 <- c(1, 3.5, 3,2, 2, 4, 4.5, 5, 3.5, 4.5)


n <- length(x4_8)
x_y_cov <- (n-1) / n * cov(x4_8, y4_8)
cor_BP <- cor(x4_8, y4_8, method = "pearson")
x_y_cov
round(cor_BP, 4)

plot(x4_8,y4_8,xlim=c(0,10),ylim=c(0,6),xlab="x",ylab="y",
     type="p",pch=16)
abline(v = mean(x4_8), col = "red")
abline(h = mean(y4_8), col = "red")
points(x = mean(x4_8), y = mean(y4_8), col = "red", pch=19)

####################//////////\\\\\\\\\\########################################
# 4.3 Linear regression
####################\\\\\\\\\\//////////########################################

reg_mod <- lm(y4_8 ~ x4_8)
a <- summary(reg_mod)$coeff[1, 1]  
b <- summary(reg_mod)$coeff[2, 1]  
r_2 <- summary(reg_mod)$r.squared
y_hat_8 <- a + b * 8
y_hat_9 <- a + b * 9
round(a, 4)
round(b, 4)
round(r_2, 4)
y_hat_8
y_hat_9

plot(x4_8, y4_8, xlim=c(0,10), ylim=c(0,6), type="p",pch=16)
abline(v = mean(x4_8), col = "red")
abline(h = mean(y4_8), col = "red")
points(x = mean(x4_8), y = mean(y4_8), col = "red", pch=19)
abline(reg_mod, col="blue")

