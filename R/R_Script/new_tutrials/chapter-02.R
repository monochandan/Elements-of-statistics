getwd()

load("Example2-1.RData")

head(Unemployment)

table(Unemployment$Gender) ### absolute frequency unemployed person gender-wise

length(Unemployment$Gender)

#table(Unemployment)
################################################################################
# factor is used to group data
numbers <- c(3, 10, 1, 3, 7, 7, 5, 1)
a_factor <- factor(numbers) ### 1, 3, 5, 7, 10 --- unique levels
a_factor
######################

###### nominal veriable --- equal / not equal ---
x2_2 <- factor(c("ECO", "ECO", "ECO", "BA", "SOC", "MAT",
                 "BMAT", "BMAT", "ECO", "ECO", "ECO", "SOC",
                 "SOC", "ECO", "ECO", "SOC", "MAT", "CS",
                 "ECO", "ECO"),
          levels = c("BA", "SOC", "ECO", "MAT", "BMAT", "CS"))





n_j <- table(x2_2) ### absolute frequency
p_j <- prop.table(n_j)  ### relative frequency

n_j 

p_j

x2_2

p_j * 100 ### percentage of share

###############################################################################
#### distribution of unemployed by age class ---- matric veriable 

### interval scaled veriable

table(Unemployment$Age)

x_o <- c(0, 15, 25, 45, 65, Inf) ### for grouping the age ----classes has created

age_class <- cut(x = Unemployment$Age, breaks = x_o, right = FALSE)

table(age_class)

length(age_class)

attach(Unemployment)
summary(Age)
###############################################################################

####  Ratio scaled veriable  --- Income

attach(Unemployment)
summary(Income)

#table(Unemployment$Income)

x_o_new <- c(200, 350, 500, 650, 800, Inf)
income_class <- cut(x = Income, breaks = x_o_new, right = FALSE)
summary(income_class)


################################################################################

load("Example2-4.RData")

head(Housing)

##################################### BAR CHART
App_pj <- t(apply(Housing[, 2:3], MARGIN = 2, FUN = prop.table)) ### MARGIN = 1/2. row/column
colnames(App_pj) <- Housing$Number_of_rooms

App_pj

Housing[, 2:3] # 2nd and 3rd column --- west, east
str(Housing)

par(mfrow = c(1, 3))
barplot(App_pj, beside = TRUE)
barplot(App_pj) ## default -- stacked bar plot
barplot(apply(App_pj, 2, prop.table))
par(mfrow = c(1, 1))


################################## PIE CHART

sum_west <- sum(Housing$West)
sum_east <- sum(Housing$East)

radius_west <- min(1, sqrt(sum_west/sum_east))
radius_east <- min(1, sqrt(sum_east/sum_west))

sum_west
sum_east

radius_west
radius_east

par(mfrow = c(1, 2)) ## 1 row 2 columns
pie(Housing$West,
    col = rainbow(n = 7),
    radius = radius_west,
    labels = Housing$Number_of_rooms)
title("West Germany", line = -4)


pie(Housing$East,
    col = rainbow(n = 7),
    radius = radius_east,
    labels = Housing$Number_of_rooms)
title("East Germany", line = -4)
par(mfrow = c(1, 1))

################################################################################

x2_5 <- c (3500 ,3200 ,2100 ,500 ,1800 ,2100 ,5600 ,4500 ,1400 ,1200 ,
           1500 ,2200 ,3100 ,1500 ,2800 ,1100 ,5200 ,4500 ,5400 ,800)

ant <- (1:length(x2_5)) / length(x2_5)
ant ## 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 
## 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00

par(mfrow = c(1, 3))
#ecdf(x2_5) ### 500,    800,   1100,  ...,   5400,   5600
plot(ecdf(x2_5), xlab = "Income",
     ylab = expression(F[n](x)),
     main = "Emperical Distribution Function",
     col = "blue")

points(sort(x2_5), c(0, ant[-length(ant)]), col = "blue")

ant[-length(ant)] ## 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70
#### 0.75 0.80 0.85 0.90 0.95 ---- not giving last point

x_o <- c(0, 1000, 2000, 3000, 4000, 5000, 6000) ##  6 class / groups

x2_5_kal <- cut(x2_5, breaks = x_o, right = FALSE)
x2_5_kal
F_j <- cumsum(prop.table(table(x2_5_kal))) ### empirical distribution function
F_j #  0.10   0.40  0.60  0.75  0.85  1.00

table(x2_5_kal) # absolute frequency

prop.table(table(x2_5_kal)) ### relative frequency

#### then F_j
############################### EDF
plot(x = c(0, 0), main = "... for 6 classes",
     xlab = "Income",
     ylab = expression(F[n](x)),
     xlim = c(0, 6000),
     ylim = c(0,1),
     type = "n")
lines(x = x_o, y = c(0, F_j), col = "blue")
points(x = x_o, y = c(0, F_j), col = "blue")

######################################## HISTOGRAM
hist(x2_5, probability = TRUE, xlab = "Income", ylab = "Density", 
      main = "Histogram", col = "#FFB000") # the bars depict --- prop.table(table(x2_5_kal))
#### using relative frequency
par(mfrow = c(1, 1))



################################################################################

########################## HISTOGRAM
load("Example2-6.RData")
factor(Time)

x_o <- c(2,5,10, 12, 14, 16, 18, 20, 25, 30, 40) ### classes for group

hist(Time, breaks = x_o, 
     right = FALSE,
     main = "histohram",
     xlim = c(0,50),
     ylim = c(0, 0.1),
     xlab = "Time to completion in minute",
     ylab = "Density")
#par(mfrow = c(1, 1))

############################# EDF

F_j <- cumsum(prop.table(table(cut(Time, breaks = x_o, right = FALSE))))

plot(x = c(0,0), type = "n",
     main = "Emperical distribution function ",
     ylab = "F(x)",
     xlab = "x",
     xlim = c(0, 50),
     ylim = c(0, 0.1),
     )
lines(x = c(0,x_o, 50), y = c(0,0, F_j, 1), col = "blue")
points(x = c(0,x_o, 50), y = c(0,0, F_j, 1), col = "blue")


################################################################################

################ HISTOGRAM VS KERNEL DENSITY ESTIMATION
x <- c(3500 , 3200 , 2100 , 500 , 1800 , 2100 , 5600 , 4500 , 1400 ,
       1200 , 1500 , 2200 , 3100 , 1500 , 2800 , 1100 , 5200 , 4500 ,
       5400 , 800)

hist(x, probability = TRUE, breaks = 11, xlim = c(0,6000), right = FALSE,
     col = "#FFB000", main = " ", xlab = "Income (right = F)",
     ylab = "Density")
lines(density(x, n = 50, from = 0, to = 6000), lwd = 4)


hist(x, probability = TRUE, breaks = 11, xlim = c(0,6000), right = TRUE,
     col = "#FFB000", main = " ", xlab = "Income (right = F)",
     ylab = "Density")
lines(density(x, n = 50, from = 0, to = 6000), lwd = 4)


