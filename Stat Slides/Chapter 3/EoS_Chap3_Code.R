################################################################################
# Elements of Statistics                                                       #
# Chapter 3: Methods of Data Collection and Visualisation                      #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 3.1 Measures of central tendency and measures of variation
####################\\\\\\\\\\//////////########################################

x3_1 <- c(5.7, 15.6, 12.6, 8.7, 11.9, 15.9, 1.6, 4.9, 19.8, 14.3)


plot(ecdf(sort(x3_1)), xlab = "x3_1", ylab = "F(x)",
     col = "blue",
     main = "Empirical distribution function"
     )
points(sort(x3_1), seq(from = 0, to = 0.9, by = 0.1),
       col = "blue"
       )
lines(x = c(-1, 4.9), y = c(0.2, 0.2), col = "red")
lines(x = c(4.9, 4.9), y = c(0.2, -1), col = "red")


# New plot like on the last slide
lines(x = c(-1, 5.7), y = c(0.25, 0.25), col = "red")
lines(x = c(5.7, 5.7), y = c(0.3, -1), col = "red")


sort(x3_1)
quantile(x = x3_1, probs = c(0.2, 0.25), type = 1)


x3_2 <- c(13.1, 12.5, 8.3, 6.4, 9.1, 10.5, 10.8, 17.9, 22.3)
sort(x3_2)
median(x3_2)


x3_3 <- c(6, 17, 22, 22, 23, 31, 34, 80, 90, 200)
sort(x3_3)
median(x3_3)


#setwd("path") # your path
load("Example3-4.RData")
F_j <- cumsum(p_j)
plot(x = c(0, 0), type = "n",
             main = "Empirical distribution function",
     ylab = "F(x)", xlab = "x",
     xlim = c(0, 50),
     ylim = c(0, 1)
     )
lines(x = c(0, x_o, 50), y = c(0, 0, F_j, 1), col = "blue")
points(x = x_o, y = c(0, F_j), col = "blue")


lines(x = c(-5, 15.8421), y = c(0.5, 0.5), col = "red")


lines(x = c(15.8421, 15.8421), y = c(0.5, -5), col = "red")


x_o
length(x_o)
F_j
length(F_j)
j <- 5
x_median <- x_o[j] + (x_o[j + 1] - x_o[j]) * (0.5 - F_j[j - 1]) / (F_j[j] - F_j[j - 1])
round(x_median, 4)


mean(x3_2)


x2_5 <- c(3500, 3200, 2100, 500, 1800, 2100, 5600, 4500, 1400, 1200,
          1500, 2200, 3100, 1500, 2800, 1100, 5200, 4500, 5400, 800
          )
mean(x2_5)


x_o <- c(0, 1500, 3000, 4500, 6000)
x2_5_kl <- cut(x2_5, x_o, right = FALSE)
n_j <- table(x2_5_kl)
x_mean_j <- tapply(X = x2_5, INDEX = x2_5_kl, FUN = mean)
x_mean_kl <- sum(n_j / sum(n_j) * x_mean_j)
x_mean_kl


x_mean_approx_j <- x_o[-5] + (x_o[-1] - x_o[-5]) / 2
x_mean_approx_kl <- sum(n_j / sum(n_j) * x_mean_approx_j)
x_mean_approx_kl


x3_7 <- c(1.8, 2.8, 3.0, 3.0, 3.9, 2.1, 
          1.3, 1.4, 2.5, 2.1, 3.3, 3.2, 
          0.3, -4.3, 2.1, 1.5
          )
wa <- x3_7 / 100 + 1
x_geom <- prod(wa)^(1 / length(wa))
x_geom


x3_8 <- c(81538.6, 81817.5, 82012.2, 82057.4, 82037, 82163.5, 
          82259.5, 82440.3, 82536.7, 82531.7, 82500.8
          )


wa <- x3_8[-1] / x3_8[-length(x3_8)]
x_geom <- prod(wa)^(1 / length(wa))
x_geom


x3_9 <- c(8, 12)
n <- length(x3_9)
x_harm <- n / sum(1 / x3_9)
x_harm


y <- seq(from = -5, to = 10, by = 0.01)
plot(y, dnorm(y, mean = 0, sd = 1),
     type = "l", xlab = "", ylab = "",
     main = "Comparison of distributions",
     col = 4)
lines(y, dnorm(y, mean = 5, sd = 1), col = 2)
lines(y, dnorm(y, mean = 5, sd = 2), col = 3)
text(x = 1.6, y = 0.35, label = "N(0;1)", col = 4)
text(x = 6.6, 0.35, "N(5;1)", col = 2)
text(x = 8.6, 0.1, "N(5;4)", col = 3)


x_1 <- c(10.4, 9.9, 10.3, 9.8, 9.9, 
         10.3, 10.4, 10.1, 10.2, 9.7
         )
x_2 <- c(81, 80.3, 80, 79.9, 79.8, 
         80.6, 80.3, 80.2, 80.3, 80.6
         )
mean(x_1)
mean(x_2)


x_1_var <- (length(x_1) - 1) / length(x_1) * var(x_1)
x_2_var <- (length(x_2) - 1) / length(x_2) * var(x_2)
x_1_var
x_2_var


sqrt(x_1_var)
sqrt(x_2_var)


x_1_var_koeff <- sqrt(x_1_var) / mean(x_1) * 100
x_2_var_koeff <- sqrt(x_2_var) / mean(x_2) * 100
x_1_var_koeff
x_2_var_koeff


x3_11 <- c(3500, 3200, 2100, 500, 1800, 2100, 5600, 8500, 1400, 1200)
x_o <- c(0, 2500, 5000, 7500, 10000)
x3_11_kl <- cut(x3_11, x_o, right = FALSE)


p_j <- prop.table(table(x3_11_kl))
n_j <- p_j * length(x3_11)
x_mean_j <- tapply(X = x3_11, INDEX = x3_11_kl, FUN = mean)
x_var_j <- (n_j - 1) / n_j * tapply(X = x3_11, INDEX = x3_11_kl, FUN = var)
x_var_j


x_var_j[is.na(x_var_j)] <- 0
x_var_between <- sum(p_j * (x_mean_j - mean(x3_11))^2)
x_var_within <- sum(p_j * x_var_j)
x_var_kl <- x_var_between + x_var_within


x_mean_approx_j <- x_o[-5] + (x_o[-1] - x_o[-5]) / 2
x_mean_approx_kl <- sum(p_j * x_mean_approx_j)
x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)
x_var_approx_kl


sort(x2_5)
stem(x = x2_5, scale = 2)


