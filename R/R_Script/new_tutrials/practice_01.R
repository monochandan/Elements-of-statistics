getwd()

x3_1 <- c(5.7 , 15.6 , 12.6 , 8.7 , 11.9 , 15.9 , 1.6 ,
          4.9 , 19.8 , 14.3)
plot(ecdf(sort(x3_1)),
     xlab = "x3_1",
     ylab = "F(x)",
     col = "blue",
     main = "Emperical distribution function")

points(sort(x3_1), seq(from = 0, to = 0.9, by = 0.1), col = "blue")
lines(x = c(-1, 4.9), y = c(0.2, 0.2), col = "red")
lines(x = c(4.9, 4.9), y = c(0.2, -1), col = "red")


ant <- (1 : length(x3_1)) / length(x3_1)
ant

sort(x3_1)

lines(x = c(-1, 5.7), y = c(0.3,0.3), col = "green")
lines(x = c(5.7, 5.7), y = c(0.3, -1), col = "green")


quantile(x = x3_1, probs = c(0.2, 0.25, 0.5), type = 1)
length(x3_1)

load("Example3-4.RData")



#rm(list = ls())


x_o ### groups
n_j ##  absolute frequency
p_j ### relative frequency

F_j <- cumsum(x = p_j)
F_j

x_o
ant <- 1 : length(x_o) / length(x_o)
ant
x_o[5]
summary(x_o)
median(x_o)
j <- 5
x_median <- x_o[j] + (x_o[j+1] - x_o[j]) * 
          (0.5 - F_j[j-1]) / (F_j[j] - F_j[j-1])
x_median

x3_2 <- c(13.1 , 12.5 , 8.3 , 6.4 , 9.1 , 10.5 ,
          10.8 , 17.9 , 22.3)
mean(x3_2)

x2_5 <- c (3500 ,3200 ,2100 ,500 ,1800 ,2100 ,5600 ,4500 ,1400 ,1200 ,
           1500 ,2200 ,3100 ,1500 ,2800 ,1100 ,5200 ,4500 ,5400 ,800)
x2_5

mean(x2_5)
summary(x2_5)

x_o = c(0,1500, 3000, 4500, 6000)
x2_5_kl <- cut(x2_5, x_o , right = FALSE)

n_j <- table(x2_5_kl)
n_j

x_mean_j <- tapply(x2_5, INDEX = x2_5_kl, FUN = mean)
x_mean_j

x_mean_kl <- sum(n_j/sum(n_j) * x_mean_j)
x_mean_kl


x_mean_approx_j <- x_o[-5] + (x_o[-1] - x_o[-5]) / 2
x_mean_approx_j

x_mean_approx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j)
x_mean_approx_kl


x3_7 <- c (1.8 ,2.8 ,3.0 ,3.0 ,3.9 ,2.1 ,1.3 ,1.4 ,2.5 ,2.1 ,3.3 ,3.2 ,
           0.3 , -4.3 ,2.1 ,1.5)



wa <- x3_7 / 100 + 1 ### scalling the vector for the negative valu
x_geom <- prod(wa)^(1/length(wa))
x_geom ### geometric mean
mean(wa) ### arithmetic mean


x3_8 <- c (81538.6 , 81817.5 , 82012.2 , 82057.4 , 82037 ,82163.5 ,
           82259.5 , 82440.3 , 82536.7 , 82531.7 , 82500.8)

wa <- x3_8[-1] / x3_8[-length(x3_8)] ### real number converted to growth factor
wa
x_geom <- prod(wa)^(1/length(wa))
x_geom
mean(wa)

x_1 <- c(10.4 , 9.9 ,10.3 , 9.8 , 9.9 , 10.3 , 10.4 , 10.1 , 10.2 ,
         9.7)
x_2 <- c(81 , 80.3 , 80, 79.9 , 79.8 , 80.6 , 80.3 , 80.2 , 80.3 ,
         80.6)

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

########################################FACTORIZE for geometric mean ###########
wa <- x_1[-1] / x_1[-length(x_1)]
wa
x_1_geom <- prod(wa)^(1/length(wa))
x_1_geom

mean(wa)
###############################################################################

x3_11 <- c (3500 ,3200 ,2100 ,500 ,1800 ,2100 ,5600 ,8500 ,1400 ,1200)
x_o <- c (0 ,2500 ,5000 ,7500 ,10000)
x3_11_kl <- cut(x3_11, x_o, right = FALSE )

x3_11

x3_11_kl

table(x3_11_kl)

p_j <- prop.table(table(x3_11_kl))
p_j

n_j <- p_j * length(x3_11)
n_j

x_mean_j <- tapply(x3_11, INDEX = x3_11_kl, FUN = mean)

x_var_j <- (n_j - 1) / n_j * 
            tapply(x3_11, INDEX = x3_11_kl, FUN = var)
x_mean_j
x_var_j



x_var_j[is.na(x_var_j)] <- 0
x_var_j

x_var_between <- sum(p_j * (x_mean_j - mean(x3_11))^2)
x_var_within <- sum(p_j * x_var_j)

x_var_kl <- x_var_between + x_var_within
x_var_kl


x_mean_approx_j <- x_o[-5] + (x_o[-1] - x_o[-5]) / 2

x_mean_approx_kl <- sum(p_j * x_mean_approx_j)

x_mean_approx_j
x_mean_approx_kl

x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)
x_var_approx_kl

###############################################################################

getwd()
#rm(list = ls())
load("Example2-1.RData")
sum(table(Unemployment$Gender))


x2_2 <- factor (c("ECO", "ECO", "ECO", "BA", "SOC", "MAT",
                  " BMAT ", " BMAT ", "ECO", "ECO", "ECO", "SOC",
                  "SOC", "ECO", "ECO", "SOC", "MAT", "CS",
                  "ECO", "ECO"),
                levels = c("BA", "ECO", "SOC", "MAT", " BMAT ", "CS")

)
x2_2

(n_j <- table(x2_2))
(p_j <- prop.table(n_j))
(p_j*100)

x_o <- c(0,15, 25, 45, 65, Inf)
(age_class <- cut(Unemployment$Age ,  x_o, right = FALSE))

(table(age_class))
(prop.table(table(age_class)))

length(age_class)

attach(Unemployment)
summary(Income)

x_o_new <- c(200, 350, 500, 650, 800, Inf)
(income_class <- cut(Income, x_o_new, right = FALSE))
summary(income_class)
table(income_class)


load("Example2-4.RData")
head(Housing)

App_pj <- t(apply(Housing[, 2:3], MARGIN = 2, FUN = prop.table))
App_pj

(colnames(App_pj) <- Housing$Number_of_rooms)

par(mfrow = c(1, 3))

barplot(App_pj,beside = TRUE)
barplot(App_pj)
barplot(apply(App_pj, 2, prop.table))
par(mfrow = c(1, 1))

sum_west <- sum(Housing$West)
sum_east <- sum(Housing$East)

radius_west <- min(1, sqrt(sum_west / sum_east ))
radius_east <- min(1, sqrt(sum_east / sum_west ))

par(mfrow = c(1, 2))
pie(Housing$West, col = rainbow(n = 7),
    radius = radius_west,
    labels = Housing$Number_of_rooms
    ) 

title("Western Germany", line = -4)

pie(Housing$East, col = rainbow(n = 7),
    radius = radius_east,
    labels = Housing$Number_of_rooms
    )
title("Eastern Germany", line = -4)
par(mfrow = c(1, 1))


x2_5 <- c (3500 ,3200 ,2100 ,500 ,1800 ,2100 ,5600 ,4500 ,1400 ,1200 ,
           1500 ,2200 ,3100 ,1500 ,2800 ,1100 ,5200 ,4500 ,5400 ,800)
(ant <- (1:length(x2_5))/length(x2_5))

par(mfrow = c(1, 3))
#plot()
ecdf(x2_5)
?ecdf
?plot

plot(ecdf(x2_5),
     xlab = "income",
     ylab = expression(F[n](x)),
     main = "Emperical distribution function",
     col = "blue")

?points

(c(0, ant[-length(ant)]))
points(sort(x2_5), c(0, ant[-length(ant)]), col = "blue")














































