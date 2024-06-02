rm(list = ls())

##### 11 march #########################################################
#1.1
RData
median(RData$EfficiencyClass)

#1.2 entropy
(n_j <- table(RData$Colour))
(p_j <- prop.table(n_j))
(entrp <- -sum(p_j * log(p_j)))
round(entrp, 4)

##1.3
mean(RData$Price)

##1.4
(emp_var_price <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price))

(emp_var <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price))

## 1.5
(iqr <- quantile(RData$Price, probs = c(0.25, 0.75), type = 5))
(range <- iqr[2] - iqr[1])
round(range, 4)

(qntl <- summary(RData$Price))
(rng <- qntl[5] - qntl[2])


## 2.1
rm(list=ls())
RData

x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
(x_class <- cut(RData$NetIncome, x_o, right = FALSE))
(n_j <- table(x_class))
(p_j <- prop.table(n_j))
n <- length(x_o)
(x_mean_approx_j <- x_o[-n] + (x_o[-1] - x_o[-n])/2)
(x_mean_approx_class <- sum(p_j * x_mean_approx_j))

## 2.2

(x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2))

## 2.3
hist(RData$NetIncome, x_o)

## 3.1
rm(list = ls())

RData

x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
x_class <- cut(RData$Weight, x_o, right = FALSE)

(n_j <- table(x_class))
(p_j <- prop.table(n_j))
n <- length(x_o)
(x_mean_approx_j <- x_o[-n] + (x_o[-1]  - x_o[-n]) / 2)
(x_mean_approx_class <- sum(p_j * x_mean_approx_j))
round(x_mean_approx_class, 4)

#3.2

x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2)
round(x_var_approx_class, 4)

#3.3
p <- 0.5
(f_j <- cumsum(p_j))

j <- 3
(linear_interpol <- x_o[j] + (x_o[j+1] - x_o[j]) * (p  - f_j[j-1]) / (f_j[j] - f_j[j-1]))
round(linear_interpol, 4)

## 6.1
(dklm <- sum(48000, 47000, 26000, 60000)/ 4)

## 6.2 
achjkl <- c(4.7, 9.4, 3.69, 6.34, 4.83, 2.61)
(n <- length(achjkl))
(herom_mean <- n / sum(1/achjkl))
round(herom_mean, 4)
########################################################################
######################################
######## 11 march ################
###############
#########
####
##

################################## 11 march ##################################
## 1.1

rm(list = ls())
head(RData)
(table(RData$Colour))


## 1.2
(p_j <- prop.table(table(RData$Colour)))

(entrp <- -sum(p_j * log(p_j)))
round(entrp, 4)

## 1.3
mean(RData$Price)

## 1.4
(emp_var <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price))

## 1.5
(z <- median(RData$Price))
(mld <- 1/length(RData$Price) * sum(abs(RData$Price - z)))


## 2.1
rm(list = ls())
head(RData)

x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
(x_class <- cut(RData$NetIncome, x_o, right = FALSE))

(n_j <- table(x_class))
(p_j <- prop.table(n_j))
n <- length(x_o)
(x_mean_approx_j <- x_o[-n] + (x_o[-1] - x_o[-n])/2)
(x_mean_approx_class <- sum(n_j / sum(n_j) * x_mean_approx_j))

## 2.2
(x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2))

##2.3
hist(RData$NetIncome, x_o)


# 3.1
rm(list = ls())
RData

x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
(x_class <- cut(RData$Weight, x_o, right = FALSE))

n <- length(x_o)
(n_j <- table(x_class))
(p_j <- prop.table(n_j))

(x_mean_approx_j <- x_o[-n] + (x_o[-1] - x_o[-n])/2)
(x_mean_approx_class <- sum(p_j * x_mean_approx_j))

## 3.2

(x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2))

## 3.3
(f_j <- cumsum(p_j))

j <- 4
p <- 0.75
(x_75 <- x_o[j] + (x_o[j+1] - x_o[j]) * (p - f_j[j-1])/ (f_j[j] - f_j[j-1]))
round(x_75, 4)


## 5.1

(dijn <- sum(41000 , 52000, 68000, 104000) / 4)

## 5.2
(u_rate <- c(8.31, 6.77, 3.24, 4.11, 4.04, 8.24))
(n <- length(u_rate))
(herm_mean <- 6 / sum(1/u_rate))
round(herm_mean, 4)
###########################################
###########################
####### 11 march ##########
#############
######
###
##
#


head(RData)

str(RData$EfficiencyClass)

str(RData)
#colour ---             nominal veriable --- mode()
# efficiency class ---  ordinal veriable ---- median()
#price ---------        ratio scaled veriable --- mean() --- arithmetic mean
# 01 ####################################################################
median(RData$EfficiencyClass) # for ordinal veriable ---- median
median(RData$EfficiencyClass)

###########################################################################

# 02 ################################################################
install.packages("entropy")
library(entropy)
entropy_value <- entropy(RData$Colour)
round(entropy_value, 4)
#################################################################################
## The entropy is used to measure the variation of nominal as well as ordinal variables.
## Therefore, determine the entropy for the ordinal variable.

## colour is ordional variable
n_j <- table(RData$EfficiencyClass)
p_j <- prop.table(n_j)
entrp <- -sum(p_j * log(p_j))
round(entrp, 4)

(entrp <- - sum( prop.table(table(RData$EfficiencyClass))  * log(prop.table(table(RData$EfficiencyClass)))))

# 03 #############################################################################
mean(RData$Price) ##### for metric veriable --- mean
#####################################################################################

## Calculate a suitable measure of location for the metric variable.
mean(RData$Price)


##### emperical veriance ######
x_1 <- c(10.4 , 9.9 ,10.3 , 9.8 , 9.9 , 10.3 , 10.4 , 10.1 , 10.2 ,9.7)
mean_x_1 <- mean(x_1)
length(x_1)
var(x_1)
(9/10) * var(x_1) #### emparical veriance

####################################################
##  Now, calculate the value of the empirical variance s???2 of the metric variable.
(emp_var_price <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price))
round()

#04 ################################################################################

rm(list = ls())

price_value <- RData$Price
price_value
(length(price_value) - 1) / length(price_value)  * var(price_value)
######################################################################################
RData
## calculate the value of the interquartile range of the metric variable. 
(qntl <- quantile(RData$Price, probs = c(0.25, 0.75), type = 1))
(iqr_range <- qntl[2] - qntl[1])

(quant <- quantile(RData$Price, probs=seq(0,1,0.25), na.rm=F, type=5))
IQR.price<-as.numeric(quant[4]-quant[2])
IQR.price

# 05 #####################################################################################
###################################################### mean linear deviation #######
#price

mean_price <- mean(RData$Price)
absolute_difference <- abs(RData$Price - mean_price)
mean_linear_deviation <- (1/length(RData$Price)) * absolute_difference
mean_linear_deviation
mean(mean_linear_deviation)

############################################################################################
str(RData)
head(RData)

################################################################################
################################################################################

getwd()
rm(list = ls())

#load("~/EOS/prep/R_Script/new_tutrials/1.1Pillar_01_QP_01_00_20151130025.RData")
head(RData)

# Calculate a suitable measure of location for the variable EfficiencyClass

median(RData$EfficiencyClass)
ln(10)

#  The entropy is used to measure the variation of nominal as well as ordinal variables.
#  Therefore, determine the entropy for the nominal variable.

n_j <- table(RData$Colour)
n_j
p_j <- prop.table(n_j)
p_j
sum(p_j)

install.packages("SciViews")
library("SciViews")

ln(10)
################## ans ###################
ent <- -sum(p_j * log10(p_j))
ent
round(ent, 4)
###################################
n_j
p_j

freq <- table(RData$Colour) /length(RData$Colour)
freq
-sum(freq * ln(freq))
entropy.empirical(freq, unit = "log10")

# Calculate a suitable measure of location for the metric variable

head(RData)
mean(RData$Price)


# Now, calculate the value of the empirical variance s???2 of the metric variable


price_mean <- mean(RData$Price)

var_from_mean<- (RData$Price - price_mean)^2

emp_var <- sum(var_from_mean) / length(RData$Price) 
emp_var

# calculate the value of the mean linear deviation of the metric variable

########################## ANS 1 ############################
deviation <- abs( RData$Price  - price_mean )
#mean_linear_deviation <- sum(deviation )/length(RData$Price)
mean_linear_deviation <- (1/length(RData$Price)) * sum(deviation)
mean_linear_deviation

#################################### ANS 2 #####################
mean_price <- mean(RData$Price)
absolute_difference <- abs(RData$Price - mean_price)
mean_linear_deviation <- (1/length(RData$Price)) * absolute_difference
mean_linear_deviation
mean(mean_linear_deviation)


################################################################################


head(RData)

# Calculate the approximate arithmetic mean for 
# the income in Paula's village by using the classified data

x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
x_1_2_kl <- cut(RData$NetIncome, x_o, right = FALSE)
x_1_2_kl
n_j <- table(x_1_2_kl)
n_j

x_mean_approx_j <- x_o[-7] + (x_o[-1] - x_o[-7])/2 
x_mean_approx_kl <- sum(n_j / sum(n_j) * x_mean_approx_j) 
x_mean_approx_kl


## Calculate the approximate arithmetic mean for the income in Paula's 
## village by using the classified data.
x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
x_class <- cut(RData$NetIncome, x_o, right = FALSE)
(n_j <- table(x_class))
(p_j <- prop.table(n_j))

(p_j <- prop.table(table(x_class)))
(n_j <- p_j * length(RData$NetIncome))

len <- length(x_o)

(x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2)
(x_mean_approx_class <- sum(p_j * x_mean_approx_j))

# Now calculate the approximate variance for classified data

p_j <- prop.table(table(x_1_2_kl))
p_j
sum(p_j)
x_mean_approx_kl <- sum(p_j * x_mean_approx_j)
x_mean_approx_kl
x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)
x_var_approx_kl

## Now calculate the approximate variance for classified data
(x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2))


#  Illustrate the income distribution by means of a histogram and 
# compare it to those four histograms 
# in the figure above. Which of the four histograms matches your histogram?


hist(RData$NetIncome,breaks = x_o, probability = TRUE, xlab = "Net Income", ylab = "density"
     , main = "histogram", ncol = "#FFB000")


## Illustrate the income distribution by means of a histogram and compare it to those
## four histograms in the figure above. Which of the four histograms matches your histogram?

hist(RData$NetIncome, breaks = x_o)
########################################## 3 ####################################

rm(list = ls())
head(RData)

# Calculate the approximate arithmetic mean for the weight using
# your previously classified data

x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
x_1_3_kl <- cut(RData$Weight, x_o, right = FALSE)
x_1_3_kl
n_j <- table(x_1_3_kl)
n_j

x_mean_approx_j <- x_o[-7] + (x_o[-1] - x_o[-5]) / 2
x_mean_approx_j

x_mean_approx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j)
x_mean_approx_kl


### Calculate the approximate arithmetic mean for the weight using your previously classified data.

x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
len <- length(x_o)
x_class <- cut(RData$Weight, x_o, right = FALSE)

(n_j <- table(x_class))
(p_j <- prop.table(n_j))

(x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2)
(x_mean_approx_class <- sum(p_j * x_mean_approx_j))
## Now calculate the approximate variance 
## for the weight using your previously classified data.

p_j <- prop.table(table(x_1_3_kl))
x_mean_approx_kl <- sum(p_j * x_mean_approx_j)
x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)
round(x_var_approx_kl, 4)

## Now calculate the approximate variance for the weight using your previously classified data

(x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2))

## Furthermore, calculate the approximate 0.25-quantile on the basis 
## of the classified data by using a linear interpolation

summary(RData)
quantile(x = RData$Weight, probs = c(0.25), type = 1)

### using linear interpolation ----

x_o
F_j <- cumsum(p_j)
F_j


ant <- (1 : length(x_o)) / length(x_o)
ant
j <- 2 ## from x_o
x_o
F_j
ant


x_approx_quantile <- x_o[j] + (x_o[j+1] - x_o[j]) * 
                    (0.25 - F_j[j-1]) / (F_j[j] - F_j[j-1]) 
round(x_approx_quantile, 4)

## Furthermore, calculate the approximate 0.5-quantile on the 
## basis of the classified data by using a linear interpolation.
x_o
(f_j <- cumsum(p_j))
j <- 3
round(x_approx_quantile <- x_o[j] + (x_o[j+1] - x_o[j]) * (0.50 - f_j[j-1]) /  ( f_j[j] - f_j[j-1]), 4)
### 5 #########################################################################

?cut
x_o <- c(110, 255, 55, 30, 50)
ant <- (1 : length(x_o))/ length(x_o)
ant

######################################################

getwd()

#rm(list = ls())
m6 <- matrix(c(110, 255, 55, 30, 50, 465, 15, 5, 10, 5, 150, 320, 20, 5, 5, 310, 100, 5, 70, 15), ncol = 4)
colnames(m6) <- c("university1", "university2", "university3", "university3")
m6
rownames(m6) <- c("[0,200)", "[200,250)","[250,300)", "[300,400)","[400,500)")
mode(m6)
m6
uni1_vector <- (m6[,"university1"])
x_o <- c(0,200,250,300,400,500)
ratio_uni1 <- uni1_vector / 500 

ratio_uni1
hist(ratio_uni1, breaks = x_o, probability = TRUE)

############################################################################

# Please use a suitable measure of location to calculate the average number
# of unemployed people in the regions B, J, L and M

bjlm <- c(56000, 18000, 63000, 72000)
sum(bjlm)/4

## Please use a suitable measure of location to calculate the average number 
## of unemployed people in the regions B, G, L and O

(bglo <- c(59000, 73000, 53000, 41000))
round(sum(bglo/4), 4)

##  Please calculate the average unemployment rate in the regions C, E, I, J, 
##  L and N on the basis of a fitting measure of location. The unemployment 
## rate (UR) is defined as the proportion of the number of unemployed persons (U) 
## and the number of employed persons (E). According to that, the following holds: 
# UR = U/E


## arithmetic mean --- if information we have belongs to denominator
## harmonic mean ----- if information we have belongs to numerator


## here we have information of U (numertaor) --- means harmonic mean

U_Rate <- c(2.6, 3.71, 3.57, 1.79, 6.26, 6.32)
n <- 6
harmonic_mean <- n/sum(1/U_Rate)
##round(harmonic_mean * 100, 4) --- wrong

round(harmonic_mean, 4) ## ---- right


## Please calculate the average unemployment rate in the regions E, F, G, H, L and O on 
## the basis of a fitting measure of location. The unemployment rate (UR) is defined as the proportion
## of the number of unemployed persons (U) and the number of employed persons (E). 
## According to that, the following holds: UR = U/E. Please state your result in percent!
(u_rate_efghlo <- c(11.2, 7.07, 7.27, 5.7, 5.23, 4.03))
n <- length(u_rate_efghlo)
harmonic_mean <- n/sum(1/u_rate_efghlo)
round(harmonic_mean, 4)
###############################################################################
######################################### 2 (mark : 95) ########################
##############################################################################
#rm(list = ls())
head(RData)

# Calculate a suitable measure of location for the variable EfficiencyClass
# ordinal scaling -- median
median(RData$EfficiencyClass)

# The entropy is used to measure the variation of nominal as well as ordinal 
#variables. Therefore, determine the entropy for the ordinal variable.
table(RData$EfficiencyClass)

(entrp <- - sum( prop.table(table(RData$EfficiencyClass))  * log(prop.table(table(RData$EfficiencyClass)))))

# Calculate a suitable measure of location for the metric variable. 
# price is the matric veriable
mean(RData$Price)

# Now, calculate the value of the empirical variance s???2 of the metric variable.

mean_price <- mean(RData$Price)
(empirical_veriance <- sum(RData$Price^2 - mean_price^2) / length(RData$Price))

(variance <- (length(RData$Price) - 1)/length(RData$Price) * var(RData$Price))
# In addition, calculate the value of the mean linear deviation of the metric variable

rm(list = ls())
RData
mean_price <- mean(RData$Price)
absolute_difference <- abs(RData$Price - mean_price)
mean_linear_deviation <- (1/length(RData$Price)) * sum(absolute_difference)
mean_linear_deviation
#mean(mean_linear_deviation)


z<- median(RData$Price)

# or alternatively 
z <- as.numeric(quant[3])
z <- quantile(RData$Price, probs = 0.5, type = 7)

MLD <- 1/length(RData$Price) * sum(abs(RData$Price - z))
MLD

# Please complete the following sentence: The median...





# Calculate the approximate arithmetic mean for the income in 
# Paula's village by using the classified data.
head(RData)

x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
x_1_2_kl <- cut(RData$NetIncome, x_o, right = FALSE)
x_1_2_kl
(n_j <- table(x_1_2_kl))
(x_mean_j <- tapply(RData$NetIncome, x_1_2_kl, FUN = mean))
(x_mean_kl <- sum(n_j/sum(n_j) * x_mean_j))

### aproximate mean
(x_mean_approx_j <- x_o[-7] + (x_o[-1] - x_o[-7])/2)
(x_mean_approx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j))




#  Now calculate the approximate variance for classified data.

x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
x_1_2_kl <- cut(RData$NetIncome, x_o, right = FALSE)
x_1_2_kl

p_j <- prop.table(table(x_1_2_kl))
n_j <- p_j * length(RData$NetIncome)

x_mean_j <- tapply(RData$NetIncome, x_1_2_kl, FUN = mean)

x_var_j <- (n_j - 1) / n_j *
            tapply(RData$NetIncome, x_1_2_kl, FUN = mean)
x_var_j

x_var_between <- sum(p_j * (x_mean_j - mean(RData$NetIncome))^2)
x_var_within <- sum(p_j * x_var_j)
x_var_kl <- x_var_between + x_var_within
x_var_kl

## approximate the veriance 

x_mean_approx_j <- x_o[-7] + (x_o[-1] - x_o[-7]) / 2
x_mean_approx_kl <- sum(p_j * x_mean_approx_j)
x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)
x_var_approx_kl




#  Illustrate the income distribution by means of a histogram and 
# compare it to those four histograms in the figure above. Which of 
# the four histograms matches your histogram?

hist(RData$NetIncome, breaks = x_o, probability = TRUE, xlab = "Net Income", ylab = "density"
     , main = "histogram", ncol = "#FFB000")

################################################################################


# Calculate the approximate arithmetic mean for the weight using your previously classified data
#rm(list = ls())

data<-as.matrix(RData)
#data
x_o_2<-c(40,50,62.5,75,87.5,100,130)
data_binned_2<-cut(data,breaks=x_o_2,right=T)
n_j_2<-table(data_binned_2)
p_j_2<-prop.table(n_j_2)
k_2<-length(x_o_2)
x_prime_2<-x_o_2[-k_2]+(x_o_2[-1]-x_o_2[-k_2])/2
#x_mean_j_2<-tapply(data,INDEX=data_binned_2,FUN=mean) #true class mean
approx.ar.mean.weight<-sum(n_j_2/sum(n_j_2)*x_prime_2) #OR
approx.ar.mean.weight_2<-sum(p_j_2*x_prime_2)
approx.ar.mean.weight



head(RData)
x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
x_1_3_kl <- cut(RData$Weight, x_o, right=FALSE)

n_j <- table(x_1_3_kl)

x_mean_j <- tapply(RData$Weight, x_1_3_kl, FUN = mean)

x_mean_kl <- sum(n_j / sum(n_j) * x_mean_j)
x_mean_kl


##approximate mean
x_mean_approx_j <- x_o[-7] + (x_o[-1] - x_o[-7]) / 2
x_mean_approx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j)
x_mean_approx_kl
round(x_mean_approx_kl, 4)








# Now calculate the approximate variance for the weight using your previously classified data.


rm(list = ls())
RData
(x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130))
(x_1_3_kl <- cut(RData$Weight, x_o, right=FALSE))

(p_j <- prop.table(table(x_1_3_kl)))
n_j <- p_j * length(RData$Weight)

x_mean_j <- tapply(RData$Weight, x_1_3_kl, FUN = mean)
x_var_j <- (n_j - 1) / n_j *
            tapply(RData$Weight, x_1_3_kl, FUN = var)
x_var_j

x_var_between <- sum(p_j * (x_mean_j - mean(RData$Weight))^2)
x_var_within <- sum(p_j * x_var_j)

x_var_kl <- x_var_between + x_var_within
x_var_kl

## approximate veriance

emp.var.weight<-sum(p_j_2*(x_prime_2-approx.ar.mean.weight)^2)
emp.var.weight



x_mean_approx_j <- x_o[-7] + (x_o[-1] - x_o[-7]) / 2
x_mean_approx_kl <- sum(p_j * x_mean_approx_j)

x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)
x_var_approx_kl



# Furthermore, calculate the approximate 0.25-quantile on the basis of the classified
# data by using a linear interpolation


x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
(p_j <- prop.table(table(x_1_3_kl)))
(f_j <- cumsum(p_j))

j <- 2

x_first_quamtile <- x_o[j] +(x_o[j+1] - x_o[j]) * 
                    (0.25 - f_j[j-1])/ (f_j[j] - f_j[j-1])
round(x_first_quamtile, 4)

###############################################################################


m6 <- matrix(c(275, 200, 10, 5, 10, 185, 100, 140, 15, 60, 470, 10, 5, 10, 5, 145, 260, 65, 25, 5), ncol = 4)
colnames(m6) <- c("university1", "university2", "university3", "university4")
m6
rownames(m6) <- c("[0,200)", "[200,250)","[250,300)", "[300,400)","[400,500)")
mode(m6)
m6
x_o <- c(0,200,250,300,400,500)

uni1_vector <- (m6[,"university1"])
uni1_vector
ratio_uni1 <- (uni1_vector / 500 )/ 100

uni2_vector <- (m6[,"university2"])
uni2_vector
ratio_uni2 <- (uni2_vector / 500 )/ 100

uni3_vector <- (m6[,"university3"])
uni3_vector
ratio_uni3 <- (uni3_vector / 500 )/ 100

uni4_vector <- (m6[,"university4"])
uni4_vector
ratio_uni4 <- (uni4_vector / 500 )/ 100




m7 <- matrix(c(ratio_uni1, ratio_uni2, ratio_uni3, ratio_uni4), ncol = 4)
colnames(m7) <- c("university1", "university2", "university3", "university4")
rownames(m7) <- c("[0,200)", "[200,250)","[250,300)", "[300,400)","[400,500)")
m7



df1 <- data.frame(
  university1 = ratio_uni1,
  university2 = ratio_uni2,
  university3 = ratio_uni3,
  university4 = ratio_uni4)

df1$university1
hist(df1$university1,breaks = x_o, probability = TRUE)
?hist

###############################################################################

#  Please use a suitable measure of location to calculate the average number 
# of unemployed people in the regions D, F, K and N

#253

dfkn <- c(35000, 24000, 14000, 34000)
(sum(dfkn) / 4)



# Please calculate the average unemployment rate in the regions D, G, H, I, K and O on
# the basis of a fitting measure of location. The unemployment rate (UR) is defined
# as the proportion of the number of unemployed persons (U) and the number of 
# employed persons (E). According to that, the following holds: UR = U/E. Please state your result in percent!

## arithmetic mean --- if information we have belongs to denominator
## harmonic mean ----- if information we have belongs to numerator

U_Rate_DGHIKO <- c(3.49, 3.87, 1.95, 2.51, 1.36, 2.33)
n <- 6
harmonic_mean <- n/sum(1/U_Rate_DGHIKO)
round(harmonic_mean, 4)

# Which of the following statements regarding the relation between 
# the arithmetic, geometric and harmonic mean is correct? 


###############################################################################
####################################### 3 mark (100)  ##############################
###############################################################################

#rm(list = ls())

head(RData)

# Calculate a suitable measure of location for the variable Colour.
# Subsequently, name the number belonging to the value.

# colour is the nominal variable --- mode() -- most frequently occurring value in the dataset.

#mode(RData$Colour)
data <- data.frame(RData)
(table(data$Colour))

# The entropy is used to measure the variation of nominal 
# as well as ordinal variables. Therefore, determine the entropy for the nominal variable.

## ordinal variable --- EfficiencyClass

(p_j <- prop.table(table(data$EfficiencyClass)))
(entrp <- - sum( prop.table(table(RData$Colour))  * log(prop.table(table(RData$Colour)))))

(entropy <- -sum(p_j * log(p_j)))
round(entropy, 4)

# Calculate a suitable measure of location for the metric variable. 

##  matric veriable is price ---- mean()
mean(RData$Price)

# Now, calculate the value of the empirical variance s???2 of the metric variable.

##  matric veriable is price

(s_star_squared <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price))

#  In addition, calculate the value of the interquartile range of the metric variable.

summary(RData$Price)
(IQR <- 238250 - 147750) # 3rd quartile - first quartile ---90500 

(quantile(RData$Price, probs = c(0.25,0.75), type = 1))
238000 - 147000 # --- 91000

#  We have a left-skewed, unimodal distribution of a metric variable. Which of the following statements is true? 

# none of the statement is true --- answer given
# the value of the arithmetic mean is always larger than the value of the mode
# the value of the median is always smaller than the value of the arithmetic mean
# the value of the mode is always smaller than the value of the median

#  Calculate the approximate arithmetic mean for the income in 
# Paula's village by using the classified data.
#rm(list = ls())
x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
x_1_2_kl <- cut(RData$NetIncome, x_o , right = FALSE)
(n_j <- table(x_1_2_kl))
len <- length(x_o)
len
x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2
(x_mean_approx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j))
round(x_mean_approx_kl,4)

# Now calculate the approximate variance for classified data.

(p_j <- prop.table(table(x_1_2_kl)))
(n_j <- p_j * length(RData$NetIncome))

(x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2)
(x_mean_approx_kl <- sum(p_j * x_mean_approx_j))

(x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2))

# Illustrate the income distribution by means of a histogram 
# and compare it to those four histograms in the figure above. 
# Which of the four histograms matches your histogram?

hist(RData$NetIncome, x_o)


# Calculate the approximate arithmetic mean for the weight using your previously classified data.
#rm(list = ls())

x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
x_1_3_kl <- cut(RData$Weight, x_o, right = FALSE)
len <- length(x_o)
x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2
(n_j <- table(x_1_3_kl))
x_mean_approx_kl <- sum(n_j / sum(n_j) * x_mean_approx_j)
x_mean_approx_kl

# Now calculate the approximate variance for the weight using your previously classified data.

p_j <- prop.table(table(x_1_3_kl))
n_j <- p_j * length(RData$Weight)


x_mean_approx_j <- x_o [-len] + (x_o[-1] - x_o[-len]) / 2
x_mean_approx_kl <- sum(p_j * x_mean_approx_j)
x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2)

x_var_approx_kl

# calculate the approximate 0.25-quantile on the basis of the classified data by using a linear interpolation

x_o
(f_j <- cumsum(p_j))
j <- 2
x_first_quantile <- x_o[j] +(x_o[j+1] - x_o[j]) * 
  (0.25 - f_j[j-1])/ (f_j[j] - f_j[j-1])
round(x_first_quantile, 4)



# Please use a suitable measure of location to calculate the average number
# of unemployed people in the regions E, I, J and O

EIJO <- c(77000, 61000, 76000, 33000)
(avg_EIJO <- sum(EIJO) / 4)

# Please calculate the average unemployment rate in the regions C, D, F, J, K 
# and M on the basis of a fitting measure of location. The unemployment rate (UR) 
# is defined as the proportion of the number of unemployed persons (U) and the number 
# of employed persons (E). According to that, the following holds: UR = U/E. 
# Please state your result in percent

## given numerator -- harmonic mean

U_rate_CDFJKM <- c(5.02, 6.95, 7.85, 7.48, 6.98, 5.96)
n <- 6
harmonic_mean <- n/sum(1/U_rate_CDFJKM)
round(harmonic_mean, 4)



# Which of the following statements regarding the relation between the arithmetic, 
#geometric and harmonic mean is correct?


# ---- the value of the inverse of the arithmetic mean is always less or at most equal 
#      to the value of the harmonic mean

# ---- the value of the arithmetic mean is always less or equal to the value of the harmonic mean

# ---- the value of the arithmetic mean is always less or not equal to the value of the geometric mean

# ---- the value of the inverse of the arithmetic mean is always greater or at 
#      least equal to the inverse of the geometric mean


###############################################################################
###############################################################################
###############################################################################

rm(list = ls())

RData


# Calculate a suitable measure of location for the variable Colour. 
# Subsequently, name the number belonging to the value

## most frequently occuring colour
getMode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
colur <- c(RData$Colour)
mode_color <- getMode(colur)
mode_color

## most frequently occuring colour
n_j <- table(RData$Colour)
n_j


# The entropy is used to measure the variation of nominal as well as ordinal variables.
# Therefore, determine the entropy for the nominal variable. 

n_j <- table(RData$Colour)
p_j <- prop.table(n_j)

entropy <-  - sum(p_j * log(p_j))

round(entropy , 4)

#  Calculate a suitable measure of location for the metric variable

mean(RData$Price)


# Now, calculate the value of the empirical variance s???2 of the metric variable
mean_price <- mean(RData$Price)
(emp_var <- (1/length(RData$Price)) * sum(RData$Price^2 - mean_price^2)) ## 3213148400
(empirical_veriance <- sum(RData$Price^2 - mean_price^2) / length(RData$Price)) ## 3213148400

(emp_var_new <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price)) ## 3213148400

#  In addition, calculate the value of the mean linear deviation of the metric variable.

mean_price <- mean(RData$Price)
absolute_difference <- abs(RData$Price - mean_price)
mean_linear_deviation <- (1/length(RData$Price)) * sum(absolute_difference)
mean_linear_deviation
 #### 48916.2

z <- as.numeric(quant[3])
z <- quantile(RData$Price, probs = 0.5, type = 7)

MLD <- 1/length(RData$Price) * sum(abs(RData$Price - z))
MLD
## 48840


#rm(list = ls())

head(RData)

## Calculate the approximate arithmetic mean for the income in Paula's village by using the classified data

x_o  <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
x_1_2_kl <- cut(RData$NetIncome, x_o, right = FALSE)
(n_j <- table(x_1_2_kl))
(x_mean_approx_j <- x_o[-length(x_o)] + (x_o[-1] - x_o[-length(x_o)])/2)
(x_mean_approx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j))

##  Now calculate the approximate variance for classified data.

(p_j <- prop.table(table(x_1_2_kl)))
(x_mean_approx_j <- x_o[-length(x_o)] + (x_o[-1] - x_o[-length(x_o)]) / 2)
(x_mean_approx_kl <- sum(p_j * x_mean_approx_j))
(x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_approx_kl)^2))

##  Illustrate the income distribution by means of a histogram and compare it to those
## four histograms in the figure above. Which of the four histograms matches your histogram?

hist(RData$NetIncome, x_o)




#rm(list = ls())

## Calculate the approximate arithmetic mean for the weight using your previously classified data.

head(RData)
x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
x_1_3_kl <- cut(RData$Weight, x_o, right = FALSE)
(n_j <- table(x_1_3_kl))
(x_mean_approx_j <- x_o[-length(x_o)] + (x_o[-1] - x_o[-length(x_o)]) / 2)
(x_mean_apporx_kl <- sum(n_j/sum(n_j) * x_mean_approx_j))


##  Now calculate the approximate variance for the weight using your previously classified data

(p_j <- prop.table(table(x_1_3_kl)))
(x_mean_approx_j <- x_o[-length(x_o)] + (x_o[-1] - x_o[-length(x_o)]) / 2)
(x_mean_apporx_kl <- sum(p_j * x_mean_approx_j))
(x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_apporx_kl)^2))

##  Furthermore, calculate the approximate 0.75-quantile on the basis of the 
##  classified data by using a linear interpolation.


x_o
(f_j <- cumsum(p_j))
j <- 2
x_first_quantile <- x_o[j] +(x_o[j+1] - x_o[j]) * 
  (0.25 - f_j[j-1])/ (f_j[j] - f_j[j-1])
round(x_first_quantile, 4)



x_o
(f_j <- cumsum(p_j))
j <- 4
x_third_quantile <- x_o[j] + (x_o[j+1] - x_o[j]) * (0.75 - f_j[j-1])/(f_j[j] - f_j[j-1])
round(x_third_quantile, 4)



## Please use a suitable measure of location to calculate the average number of
## unemployed people in the regions A, E, K and N. 


AEKN <- c(91000, 83000, 80000, 64000)
(avg_EIJO <- sum(AEKN) / 4)

## Please calculate the average unemployment rate in the regions D, G, J, K, M 
## and O on the basis of a fitting measure of location. The unemployment rate (UR)
## is defined as the proportion of the number of unemployed persons (U) and the number
## of employed persons (E). According to that, the following holds: UR = U/E. Please 
## state your result in percent!

## given numerator (proportion od unemployed person) -- weighted harmonic mean
U_rate_DGJKMO <- c(8.91, 9.72, 8.04, 8.02, 8.31, 4.96)
n <- length(U_rate_DGJKMO)
(hermonic_mean <- n/sum(1/U_rate_DGJKMO))
round(hermonic_mean, 4)

##  Which of the following statements regarding the relation between the arithmetic,
## geometric and harmonic mean is correct? 



rm(list = ls())


################################################################################
###############################################################################
############################# 4 (mark: ) #####################################


#########################################  question 1 ##########################

rm(list = ls())

## a) (8 points) Calculate a suitable measure of location for the variable EfficiencyClass.

head(RData)
median(RData$EfficiencyClass)

head(RData)
median(RData$EfficiencyClass)


##  (12 points) The entropy is used to measure the variation of nominal as well as ordinal variables. 
 ## Therefore, determine the entropy for the ordinal variable.

n_j <- table(RData$EfficiencyClass)
p_j <- prop.table(n_j)

entrp <- -sum(p_j * log(p_j))
round(entrp,4)

(entrop <- - sum( prop.table(table(RData$EfficiencyClass))  * log(prop.table(table(RData$EfficiencyClass)))))


###########

n_j <- table(RData$EfficiencyClass)
p_j <- prop.table(n_j)
entropy = -sum(p_j * log(p_j))
round(entropy, 4)



## c) (8 points) Calculate a suitable measure of location for the metric variable.

mean(RData$Price)

head(RData)

mean(RData$Price)


## d) (14 points) Now, calculate the value of the empirical variance s???2 of the metric variable.

(emp_var_price <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price))


#####

emp_var_price <- (length(RData$Price) - 1) / length(RData$Price) * var(RData$Price)
round(emp_var_price,4)


## e) (14 points) In addition, calculate the value of the interquartile range of the metric variable.
summary(RData$Price)
##(IQR <- (241250 - 145000))
#max(RData$Price) - min(RData$Price)
IQR(RData$Price,type = 5)

##first_qntl <- quantile(RData$Price, probs = c(0.25), type = 1)
##third_qntl <- quantile(RData$Price, probs = c(0.75), type = 1)
##(IQR1 <- third_qntl - first_qntl)

## e) (14 points) In addition, calculate the value of the mean linear deviation of the metric variable.

length((RData$Price))


mean_price <- mean(RData$Price)
absolute_difference <- abs(RData$Price - mean_price)
mean_linear_deviation <- 1/200 * sum(absolute_difference)
mean_linear_deviation

##### MLD for metric veriable

z <- median(RData$Price)
mld <- 1/length(RData$Price) * sum(abs(RData$Price - z))
round(mld, 4)









#(1/200 * (sum(RData$Price - mean(RData$Price))))

(1/200) * (sum(abs(RData$Price - mean_price)))


## f) (4 points) Please complete the following sentence: The relative frequency of the values within the classes of a metric variable...


## it reflected by the means of the area of the rectangle of a histogram



############################## Question 2 ######################################

rm(list = ls())

## a) (20 points) Calculate the approximate arithmetic mean for the income in Paula's village by using the classified data

head(RData)

x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
class_x <- cut(RData$NetIncome, x_o, right = FALSE)

n_j <- table(class_x)

p_j <- prop.table(table(class_x))

len <- length(x_o)

x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2
x_mean_approx_class <- sum(p_j * x_mean_approx_j)
round(x_mean_approx_class, 4)

#######

head(RData)
x_o <- c(500, 1000, 1750, 2500, 3500, 4500, 7000)
data_class <- cut(RData$NetIncome, x_o, right = FALSE)

n_j <- table(data_class)

p_j <- prop.table(table(data_class))

len <- length(x_o)


x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2
x_mean_approx_class <- sum(p_j * x_mean_approx_j)
round(x_mean_approx_class, 4)










## b) (26 points) Now calculate the approximate variance for classified data.


(p_j <- prop.table(table(class_x)))

x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2)
round(x_var_approx_class, 4)

####

x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2)
round(x_var_approx_class, 4)




## c) (14 points) Illustrate the income distribution by means of a histogram and compare 
## it to those four histograms in the figure above. Which of the four histograms matches your histogram?

hist(RData$NetIncome, x_o)


hist(RData$NetIncome, x_o)


################################# Question 3 ######################################################
rm(list = ls())

## a) (20 points) Calculate the approximate arithmetic mean for the weight using your previously classified data.
head(RData)

x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
class_x <- cut(RData$Weight, x_o, right = FALSE)

n_j <- table(class_x)

p_j <- prop.table(table(class_x))

len = length(x_o)

x_mean_approx_j <- x_o[-len] + (x_o[-1] - x_o[-len]) / 2
x_mean_approx_class <- sum(p_j * x_mean_approx_j)
round(x_mean_approx_class, 4)

######

head(RData)
x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
data_class <- cut(RData$Weight, x_o, right = FALSE)

n_j <- table(data_class)
p_j <- prop.table(n_j)

len <- length(x_o)

x_mean_approx_j <-  x_o[-len] + (x_o[-1] - x_o[-len]) / 2
x_mean_approx_class <- sum(p_j * x_mean_approx_j)
round(x_mean_approx_class, 4)










## b) (26 points) Now calculate the approximate variance for the weight using your previously classified data.

x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2)
round(x_var_approx_class, 4)#



## 
x_var_approx_class <- sum(p_j * (x_mean_approx_j - x_mean_approx_class)^2)
round(x_var_approx_class, 4)
## c) (14 points) Furthermore, calculate the approximate 0.75-quantile on the basis of the classified data by using a linear interpolation.


(f_j <- cumsum(p_j))
j <- 3
x_third_qntl <- x_o[j] + (x_o[j+1] - x_o[j]) * (0.75 - f_j[j-1]) / (f_j[j] - f_j[j-1])
round(x_third_qntl, 4)


### 0.5 quantile using linear interpolation
x_o <- c(40, 50, 62.5, 75, 87.5, 100, 130)
(f_j <- cumsum(p_j))
j <- 3
x_second_qntl <- x_o[j] + (x_o[j+1] - x_o[j]) * (0.5 - f_j[j-1]) / (f_j[j] - f_j[j-1])
round(x_second_qntl, 4)



##x_0.75 <- x_o[3] + ((x_o[4]-x_o[3])*(0.75-f_j[2])/(f_j[3]-f_j[2]))
##x_0.75


########################### Question 6  ########################################

## a) (20 Points) Please use a suitable measure of location to calculate 
## the average number of unemployed people in the regions C, I, J and L.

no_unemp_per <- c(81000, 89000, 53000, 26000)

avg <- sum(no_unemp_per) / 4
round(avg, 4)



### B D G J

unemp <- c(111000, 86000, 92000, 63000)
avg <- sum(unemp) / 4
round(avg, 4)


## b) (30 Points) Please calculate the average unemployment rate in the regions
## E, F, H, I, K and O on the basis of a fitting measure of location. 
## The unemployment rate (UR) is defined as the proportion of the number of 
## unemployed persons (U) and the number of employed persons (E). According to that, 
## the following holds: UR = U/E. Please state your result in percent
unemp_rate <- c(7.31, 8.98, 2.75, 5.3, 6.5, 4.56)
n <- 6

harmonic_mean <- n / sum(1/unemp_rate)

round(harmonic_mean, 4)

#### F G H J L N
unemp_rate <- c(9.15, 9.1, 8.72, 6.33, 9.01, 7.28)
n <- 6
harmonic_mean <- n/sum(1/unemp_rate)
round(harmonic_mean, 4)



## c) (10 Points) Which of the following statements regarding the relation between
## the arithmetic, geometric and harmonic mean is correct?

rm(list = ls())

###############################################################
################################## 5 ()
















