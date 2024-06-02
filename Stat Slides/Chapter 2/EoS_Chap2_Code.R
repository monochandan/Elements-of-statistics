################################################################################
# Elements of Statistics                                                       #
# Chapter 2: Methods of Data Collection and Visualisation                      #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 2.2 One-dimensional frequency distributions
####################\\\\\\\\\\//////////########################################

#setwd("path") # Choose your working directory on your own.
load("Example2-1.RData")
table(Unemployment$Gender)
length(Unemployment$Gender)

x2_2 <- factor(c("ECO", "ECO", "ECO", "BA", "SOC", "MAT",
                   "BMAT", "BMAT", "ECO", "ECO", "ECO", "SOC",
                   "SOC", "ECO", "ECO", "SOC", "MAT", "CS",
                   "ECO", "ECO"),
                       levels = c("BA", "ECO", "SOc", "MAT", "BMAT", "CS")
                   )
n_j <- table(x2_2)
p_j <- prop.table(n_j)
n_j
p_j
p_j * 100

x_o <- c(0, 15, 25, 45, 65, Inf)
age_class <- cut(x = Unemployment$Age,
                breaks = x_o,
                right = FALSE
                )
table(age_class)
length(age_class)


attach(Unemployment)
summary(Income)
x_o_new <- c(200, 350, 500, 650, 800, Inf)
income_class <- cut(x = Income,
                        breaks = x_o_new,
                        right = FALSE
                        )
summary(income_class)


load("Example2-4.RData")
App_pj <- t(apply(Housing[, 2:3], MARGIN = 2, FUN = prop.table))
colnames(App_pj) <- Housing$Number_of_rooms
round(App_pj, digits = 4) # previous slide
par(mfrow = c(1, 3))
barplot(App_pj, beside = TRUE)
barplot(App_pj)
barplot(apply(App_pj, 2, prop.table))
par(mfrow = c(1, 1))


sum_west <- sum(Housing$West)
sum_east <- sum(Housing$East)
radius_west <- min(1, sqrt(sum_west / sum_east))
radius_east <- min(1, sqrt(sum_east / sum_west))
par(mfrow = c(1, 2))
pie(Housing$West,
    col = rainbow(n = 7),
    radius = radius_west,
    labels = Housing$Number_of_rooms
    )
title("Western Germany", line = -4)
pie(Housing$East,
    col = rainbow(n = 7),
    radius = radius_east,
    labels = Housing$Number_of_rooms
    )
title("Eastern Germany", line = -4)
par(mfrow = c(1, 1))


x2_5 <- c(3500, 3200, 2100, 500, 1800, 2100, 5600, 4500, 1400, 1200,
          1500, 2200, 3100, 1500, 2800, 1100, 5200, 4500, 5400, 800
          )
ant <- (1:length(x2_5)) / length(x2_5)
par(mfrow = c(1, 3))
plot(ecdf(x2_5), xlab = "Income", ylab = expression(F[n](x)), 
     main = "Empirical distribution function", col = "blue"
     )
points(sort(x2_5), c(0, ant[-length(ant)]), col = "blue")
x_o <- c(0, 1000, 2000, 3000, 4000, 5000, 6000) # alte Grenzen ueberschrieben
x2_5_kl <- cut(x2_5, x_o, right = FALSE)
F_j <- cumsum(prop.table(table(x2_5_kl)))
plot(x = c(0, 0), main = "... for 6 classes",
     xlab = "Income", ylab = expression(F[n](x)),
     xlim = c(0, 6000), ylim = c(0, 1),
     type = "n"
     )
lines(x = x_o, y = c(0, F_j), col = "blue")
points(x = x_o, y = c(0, F_j), col = "blue")
hist(x2_5, probability = TRUE,
     xlab = "Income", ylab = "Density",
     main = "Histogram", col = "#FFB000"
     )
par(mfrow = c(1, 1))


load("Example2-6.RData")
x_o <- c(2, 5, 10, 12, 14, 16, 18, 20, 25, 30, 40)
hist(Time, breaks = x_o, right = FALSE,
     main = "", xlim = c(0, 50), ylim = c(0, 0.1),
         xlab = "Time to completion in minutes", ylab = "Density"
     )
F_j <- cumsum(prop.table(table(cut(Time, x_o, right = FALSE))))
plot(x = c(0,0), type = "n",
     main = "Empirical distribution function",
     ylab = "F(x)", xlab = "x",
     xlim = c(0, 50), ylim = c(0, 1)
     )
lines(x = c(0, x_o, 50), y = c(0, 0, F_j, 1), col = "blue")
points(x = c(0, x_o, 50), y = c(0, 0, F_j, 1), col = "blue")


par(mfrow = c(1, 2))
hist(x2_5, probability = TRUE, breaks = 11,
     xlim = c(0, 6000), right = TRUE, col = "#FFB000",
     xlab = "Income (right = T)", ylab = "Density",
     main = "",
     ylim = c(0, 0.0006)
     )
lines(density(x2_5, n = 50), lwd = 4)
hist(x2_5, probability = TRUE, breaks = 11,
     xlim = c(0, 6000), right = FALSE, col = "#FFB000",
     xlab = "Income (right = F)", ylab = "Density",
     main = "",
     ylim = c(0, 0.0006)
)
lines(density(x2_5, n = 50), lwd = 4)
par(mfrow = c(1, 1))


####################//////////\\\\\\\\\\########################################
# 2.3 Two-dimensional frequency distributions
####################\\\\\\\\\\//////////########################################

#####
### Just for interest, not obligatory
### Attention: This will write directly into PDF and not into Panel 3
#####

load("Example2-8.RData")
FQtable_new <- as.matrix(FQtable[1:2, 1:5])
pdf("MZ_2D.pdf", width = 10, height = 7)
barplot(FQtable_new, beside = TRUE,
        xlab = "Age class", ylab = "Unemployed"
        )
legend("topleft", legend = c("M", "F"),
       fill = c("black", "grey")
       )
dev.off()


#install.packages("scatterplot3d") # install this package, if necessary.
library(scatterplot3d)
old_names <- colnames(FQtable_new)
pdf("MZ_3D.pdf", width = 10, height = 7)
scatterplot3d(x = rep(0:4, each = 2),
              y = rep(1:2, 5),
              z = as.vector(FQtable_new),
              type = "h",
              xlab = "Age class",
              ylab = "",
              zlab = "Unemployed",
              x.ticklabs = 
              c(old_names[1], "",
                old_names[3], "",
                old_names[5], "",
                old_names[2], "",
                old_names[4], ""
                ),
              y.ticklabs =
              c("M", rep("", 4), "F"),
              lwd = 5,
              color = 4
              )
dev.off()
