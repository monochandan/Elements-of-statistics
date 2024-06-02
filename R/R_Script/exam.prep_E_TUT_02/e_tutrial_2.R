rm(list = ls())
# In order to analyse the relation between movie genre and 
# field of study, calculate the value of the standardised contingency coefficient




###################################  11 march #################################
#1.1
(m <- matrix(c(52, 240, 16, 68, 201, 29, 103, 206, 85), ncol = 3))
(n_j_k <- margin.table(m))
(sum(m))
#(c <- summary(n_j_k)$statistic)
(chisq <- chisq.test(m)$statistic)
(kk <- sqrt(chisq / (n_j_k + chisq)))
(m1 <- min(dim(m)))
(k_max <- sqrt((m1-1)/ m1))
(k_star <- kk/ k_max)
round(k_star, 4)

## 2.1
(pth <- c(111, 193, 77, 74, 73, 138, 64))
(pa <- c(171, 235, 178, 117, 181, 171, 120))


## 2.2
(emp_cov <- 1/7 * sum((pth - mean(pth)) * (pa - mean(pa))))

# 2.3
(cor_pearson <- cor(pth, pa, method = "pearson"))
round(cor_pearson, 4)

## 2.4
round(0.7436^2, 4)

## 2.5
n <- 100
(rnk <- 1-((6*46147)/(n * (n^2-1))))
round(rnk, 4)


## 3.1

rm(list = ls())
RData
(marg_i4 <- 29 + 3 + 16 + 66 + 39 + 28)

##3.2

(i5_1_njk <- 307 - (56 + 65 + 53 + 19 + 27))

## 3.3
n_j <- 140
n_k <- 184
(n_j_k_star <- (140 * 184) / 1201)
(p_j_k_star <- n_j_k_star / 1201)

## 3.4 

(p_j_given_k <- 26 / 250)

## 3.5
(RData[5,1] <- 87)
RData
(f_j_k <- t(apply(apply(RData, 2, cumsum), 1, cumsum)))
round(f_j_k/1201, 4)

## 5.1
RData

(cor_pearson <- cor(RData$TotalJumpingDistance, RData$Score, method = "pearson"))
round(cor_pearson, 4)

## 5.2

model <- lm(RData$Score~RData$TotalJumpingDistance)
summary(model)
(intercept_a <- summary(model)$coeff[1, 1])
(slop_b <- summary(model)$coeff[2, 1])

## 5.3
(r_squiare <- summary(model)$r.squared)
round(r_squiare, 4)

## 5.4
#residuals(model)
#sum(residuals(model))

round((sum(model$residuals)),4)

## 5.5
round(sum((model$residuals)^2), 4)

## 5.6

(y <-  intercept_a + (slop_b * 230))

################################################
###################################
############ 11 march ###########
############
#####
###
#



########################## 11 march ###########################################

#1
(m <- matrix(c(53, 233, 23, 62, 222, 4, 68, 118, 218), ncol = 3))
colnames(m) <- c("Action", "Fantasy", "Comedy") 
rownames(m) <- c("Economics", "Business", "Social Sciences")

(n_j_k <- margin.table(m))

(sum(m))

(chsq <- chisq.test(m)$statistic)
(kk <- sqrt(chsq / (sum(m) + chsq )))
(m1 <- min(dim(m)))
(k_max <- sqrt((m1-1)/m1))
(k_star <- kk / k_max)
round(k_star, 4)

## 2.1
(pth <- c(90, 63, 88, 68, 58, 58, 125))
(ap <- c(171, 122, 111, 136, 156, 145, 253))
(emp_cov <- sum((pth - mean(pth)) * (ap - mean(ap))) / 7)
round(emp_cov, 4)

##2.2

(rsp <- cor(rank(pth), rank(ap), method = "spearman"))
round(rsp, 4)

## 2.3
coeff <- 0.7321
(r_squar <- coeff^2)
round(r_squar, 4)


## 2.4


rnk <- 42413
n <- 100

(rsp <- 1 - ((6 * rnk) / (n * (n^2 - 1))))
round(rsp, 4)


## 3.1
rm(list = ls())
(50 + 13 + 42 + 51 + 29 + 18)

RData

## 3.2
(n_j <- 108 - (6 + 20 + 28 + 6 + 17))

RData[4, 6] <- 31
RData
##(sum(RData))
(n_4_6 <- RData[4,6])
(p_4_6 <- n_4_6 /904) 
round(p_4_6, 4)

(RData <- addmargins(RData))
#3.3
(n_j_k_star <- (108 * 159)/904)
round(n_j_k_star, 4)



## 3.4
##(sum(RData[2,])
(RData[2,4])
(p_k_given_j <- RData[2,4] / 201)
round(p_k_given_j, 4)

#3.5.

(f_j_k <- t(apply(apply(RData/904, 2, cumsum), 1, cumsum)))
round(f_j_k, 4)


# 5.1
rm(list = ls())
head(RData)

(cor_person <- cor(RData$TotalJumpingDistance, RData$Score, method = "pearson"))
round(cor_person, 4)

## 5.2
model <- lm(RData$Score ~ RData$TotalJumpingDistance)
summary(model)
intercept_a <- summary(model)$coeff[1, 1]
(slop_b <- summary(model)$coeff[2,1])
round(slop_b, 4)

## 5.3
(r_squared <- summary(model)$r.squared)
round(r_squared, 4)

## 5.4
(jumping_distance <- RData$TotalJumpingDistance / 1000)
(new_model <- lm(jumping_distance ~ RData$Score))
(r_squared <- summary(new_model)$r.squared)

round(r_squared, 4)


## 5.5
round(sum((model$residuals)^2), 4)

##5.6
x <- 291
(y <- intercept_a + (slop_b * x))
##########################################
############## 11 march ################
######################
############
######
####

############################ ans : 1 ######## ##### 
m6 <- matrix(c(50, 239, 6, 74, 210, 5, 52, 192, 172), ncol = 3)
colnames(m6) <- c("Action", "Fantasy", "Comedy")
#m6
rownames(m6) <- c("Economic", "Business","Social Science")
mode(m6)
m6

save(m6, file = "tutrial_2_question_1.RData")

load("tutrial_2_question_1.RData")
m6
mode(m6)
#n_j_k <- addmargins(m6)
#n_j_k
#n_j_k_star <- margin.table(n_j_k,1) %*% t(margin.table(n_j_k,2)) / margin.table(n_j_k)
#n_j_k_star

chisq <- chisq.test(m6)$statistic
chisq
n_j
kk <- sqrt(chisq/sum(m6) + chisq)
M <- min(dim(m6))
K_max <- sqrt((M-1)/M)
K_star <- kk/K_max

kk
M
K_max
K_star

round(K_star, 4)

#####

m1 <- matrix(c(53, 228, 16, 65, 206, 25, 42, 195, 170), ncol = 3)
m1

m2 <- rbind(c(53, 65, 42), c(228, 206, 195), c(16, 25, 170))
colnames(m2) <- c("Action", "Fantasy", "Comedy")
rownames(m2) <- c("Economics", "Business", "Social Sciences")
m2

chisq <- chisq.test(m2)$statistic
chisq

margin.table(m2) ## 1000

sum(m2) # 1000

(kk <- sqrt(chisq/(margin.table(m2) + chisq)))
(M <- min(dim(m2)))
(K_Max <- sqrt((M-1) / M))
(K_Star <- kk / K_Max)
round(K_Star, 4)



## In order to analyse the relation between movie genre and field of study, 
## calculate the value of the standardised contingency coefficient

(m <- matrix(c(56, 241, 4, 62, 220, 11, 109, 112, 185), ncol = 3))
(colnames(m) <- c("Action", "Fantasy", "Comedy"))
#m6
(rownames(m) <- c("Economic", "Business","Social Science"))
m
(n_j_k <- margin.table(m))
(chsq <- chisq.test(m)$statistic)
(kk <- sqrt(chsq / (sum(n_j_k) + chsq)))
(m1 <- min(dim(m)))
(k_max <- sqrt((m1 - 1) / m1))
(k_star <- kk / k_max)
round(k_star, 4)
####################################### ans 2 #################################

data<-rbind(c(58,70,66),c(229,224,101),c(18,24,210))


data<-rbind(c(50,74,52),c(239,210,192),c(6,5,172)) 
data
colnames(data) <- c("Action", "Fantasy", "Comedy")
rownames(data) <- c("Economics","Business","Social sciences")
data
str(data)
str(n_j_k)
chisq <- chisq.test(data)$statistic
chisq
kk <- sqrt(chisq/sum(data) + chisq)
M <- min(dim(data))
K_max <- sqrt((M-1)/M)
K_star <- kk/K_max

kk
M
K_max
K_star


n_j_k
data

#  Please calculate the empirical covariance between 
# the variables preparation time in hours and achieved points.

pth <- c(49, 110, 45, 159, 32, 78, 70)
pa <- c(146, 190, 170, 202, 132, 210, 104)
sum((pth - mean(pth)) * (pa - mean(pa)))/7

## Please calculate the empirical covariance between the variables preparation time in hours and achieved points.

pth <- c (57, 91, 52, 34, 40, 86, 68)
pa <- c(133, 204, 162, 201, 133, 134, 255)

(emp_cov <- sum((pth - mean(pth)) * (pa - mean(pa)))/7)


###
pth <- c(43,12,78,74,121,77,85)
ap <- c(201,170,203,136,233,158,213)

emp_cov <- sum((mean(pth) - pth) * (mean(ap) - ap)) / 7
round(emp_cov, 4)


# Please calculate Pearson's correlation coefficient 
# for the variables preparation time in hours and achieved points.

cor_person <- cor(pth, pa, method = "pearson")
round(cor_person, 4)

## spearman

rank_cor_coeff <- cor(pth, ap, method = "spearman")
round(rank_cor_coeff, 4)

## Please calculate Spearman's rank correlation coefficient between the variables preparation time in hours and achieved points
(rank_corr_coeff <- cor(rank(pth), rank(pa), method = "spearman"))
round(rank_corr_coeff, 4)

(rank_corr_coeff <- cor(rank(pa), rank(pth), method = "spearman"))
round(rank_corr_coeff, 4)

#  You already know that the achieved number of points is linearly 
# dependent on the time spent preparing. Based on that knowledge, 
# you estimate a linear regression model for all 100 students and 
# get a value of R2= 0.3301 for the coefficient of determination. 
# According to this, which of the following values is a plausible 
# choice for Pearson's correlation coefficient in the whole sample?




###  In addition, you calculated Pearson's correlation coefficient over 
### all 100 students for the same set of variables as before and got a value of 0.4224.
## Which one of the following values matches the one of the coefficient of determination?



## 0.8216,
## 0.1784, ------- right ans
round(0.4224^2, 4)
## 0.1677, 
## -0.1784


## you calculated Pearson's correlation coefficient over all 100 students for the same set of variables
## as before and got a value of 0.101
## Which one of the following values matches the one of the coefficient of determination?

cor_coeff <- 0.101
round(cor_coeff^2, 4)


## Finally, you want to calculate Spearman's rank correlation coefficient based on the data
## for all 100 students interviewed. You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 161804.
n <- 100
(r_s_p <- 1 - ((6*161804)/(n * (n^2 - 1))))

round(r_s_p, 4)



# Finally, you want to calculate Spearman's rank correlation coefficient 
# based on the data for all 100 students interviewed. 
# You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 66655.

## n<-length(pth)
## SP<-cor(rank(pth),rank(pa),method = "spearman")
## SP         #OR

## Sp<-1-(6*sum((rank(pth)-rank(pa))^2)/(n*(n^2-1)))
## Sp


n <- length(pth)

rsp <- 1 - ((6*66655)/(n * (n^2 - 1)))
rsp
rsp <- cor(rank(pth), rank(pa), method = "spearman")
rsp
round(rsp, 4)




### 
spearman_rnk_cor_coeff <- cor(rank(pth), rank(ap), method = "spearman")
round(spearman_rnk_cor_coeff, 4)



another_one <- 1 - (6 * 87594) / (100 * (100^2 - 1))
round(another_one, 4)




###############################  Question 3 ##################################

rm(list = ls())

RData

## a) (10 Points) Please calculate the missing value of the marginal distribution in the table
(17 + 64 + 21 + 63 + 19) - 259
RData[5,6] <- 75

##  Please calculate the missing value of the marginal distribution in the table

#(RData <- margin.table(RData))
(val <- 125 - (38 + 11 + 26 + 39 + 10))
RData[is.na(RData)] <- val
(RData <- addmargins(RData))
RData

#(data <- data.frame((RData)))
#(data[is.na(data)] <- val)
#data
#(addmargins(data))

###
mat <- matrix(c(18, 15, 36, 25, 17, 20,
                35, 35, 26, 36, 35, 39,
                26, 11,  3, 20, 39, 25,
                38, 11, 26, 39, NA, 10,
                4, 10,  7, 37, 22, 32), 
              nrow = 5, byrow = TRUE)

# Replace NA values with a specific value (e.g., 0)
mat[is.na(mat)] <- 0

# Print the modified matrix
print(mat)
### --- 276

## b)(14 Points) Please calculate the relative joint frequency pjk for the missing cell in the table at hand
n_j_k <- 75
n <- 1501

(p_j_k <- n_j_k / n)
round(p_j_k , 4)

## Please calculate the relative joint frequency pjk for the missing cell in the table at hand.
(n <- 698)
(n_j_k <- 1)
(p_j_k <- n_j_k / n)
round(p_j_k , 4)
## c)(12 Points) For now, we assume independence of the two variables. Please calculate 
## the absolute frequency n???jk, with j=2 and k=1 under the assumption of independence.

j_2 <- 320
k_1 <- 290

n_star_j_k <- (j_2 * k_1) / n
round(n_star_j_k, 4)

##  For now, we assume independence of the two variables.
## Please calculate the absolute frequency n???jk, with j=1 and k=3 under the assumption of independence

(n_j_k_star <- margin.table(RData, 1) %*% t(margin.table(RData, 2)) / margin.table(RData))
round(n_j_k_star, 4)

j <- 131
k <- 98
n <- 698
(n_j_k_star_1_3 <- j * k / n)
round(n_j_k_star_1_3, 4)

## d) (14 Punkte) Please calculate the conditional relative frequency pj|k with j=1 and k=1.

n_j_k <- 59
n_k <- 290

p_j_given_k <- n_j_k / n_k
round(p_j_given_k, 4)

## Please calculate the conditional relative frequency pj|k with j=3 and k=1
#(margin.table(RData[1],2))
RData[3,1]
k_1 <- 121
#(p_j_k <- RData / margin.table(RData))
(p_j_given_k <- RData[3,1] / 121)
round(p_j_given_k, 4)

## e) (10 Points) You are interested in the bivariate empirical distribution function as well.
## Please calculate the value of Fn(x,y) at x=I3 and y=R.
RData

f_j_k <- t(apply(apply(RData, 2, cumsum), 1, cumsum))
round(f_j_k/698, 4)

##You are interested in the bivariate empirical distribution function as well. Please calculate the value of Fn(x,y) at x=I2 and y=B

#(f_j_k_new <- t(apply(apply(p_j_k, 2, cumsum), 1, cumsum)))
#round(f_j_k_new, 4)





######### Question 4 ########

x <- c(0,2, 4, 6, 8, 10, 12)
y <- c(0,5, 10, 15, 20, 25, 30)


a <- 	-1.1215
b <- -1.8548

plot(x, y, type = "p")
abline(a, b, col= "red")

######################### Question 5 ##################################


rm(list = ls())
RData

## a) (8 points) Calculate the relation between the reached score and the total
## jumping distance by means of the rank correlation coefficient of Spearman.
head(RData)
n <- length(RData$TotalJumpingDistance)
rank_cor_coeff_spearman <- cor(rank(RData$TotalJumpingDistance), rank(RData$Score), method = "spearman")
round(rank_cor_coeff_spearman, 4)

new_rank <- 1 - (6 * sum((rank(RData$TotalJumpingDistance) - rank(RData$Score))^2)) / (n * (n^2 - 1))
round(new_rank,4)


##  Calculate the relation between the reached score
## and the total jumping distance by means of the rank correlation coefficient of Spearman

(rank_cor_coeff_spr <- cor(rank(RData$TotalJumpingDistance), rank(RData$Score), method = "spearman"))

round(rank_cor_coeff_spr, 4)






## b) (10 points) You assume a linear relationship between the total jumping distance 
## and the reached score (dependent variable). Hence, set up a suitable linear regression 
## model and calculate the intercept by means of the method of ordinary least squares. 

regression_model <- lm(RData$Score ~ RData$TotalJumpingDistance)
summary(regression_model)

## intercept -- a
## slop --- b
intercept_a <- summary(regression_model)$coeff[1,1]
slop_b <- summary(regression_model)$coeff[2,1]
round(intercept_a, 4)


## You assume a linear relationship between the total jumping distance and the reached score (dependent variable). Hence,
## set up a suitable linear regression model and calculate the slope by means of the method of ordinary least sqares.

reg <- lm(RData$Score~ RData$TotalJumpingDistance)
reg

intercept_a <- summary(reg)$coeff[1,1]
slop_b <- summary(reg)$coeff[2,1]
round(slop_b, 4)
## c) (6 points) Furthermore, determine the value of the coefficient of determination of your regression model. 

coeff_of_det_r_2 <- summary(regression_model)$r.squared
round(coeff_of_det_r_2, 4)

## Furthermore, determine the value of the coefficient of determination of your regression model
coeff_det <- summary(reg)$r.squared
round(coeff_det, 4)


## d) (4 points) State the value of the sum of all residuals of your regression model. 
residuals <- sum(resid(regression_model))
round(residuals, 4)
0


# Now assume that the total jumping distance should be measured in kilometres instead of metres.
## Which value does the coefficient of determination have after this transformation?
(data <- RData$TotalJumpingDistance / 100)
(reg_1 <- lm(RData$Score ~ data))
(coeff_det_new <- summary(reg_1)$r.squared)
round(coeff_det_new, 4)

## e) (10 points) State the value of the sum of the squared residuals of your regression model.
round(sum((regression_model$residuals)^2), 4)

## State the value of the sum of the squared residuals of your regression model

round((sum((reg$residuals)^2)),4)

## f) (16 points) Assume that an athlete reaches a total jumping distance of 275 metres.
## Which score do you expect for this athlete, if you take your calculated regression model as a basis. 

x <- 275


y <- intercept_a + (slop_b * x)
round(y, 4)


## Assume that an athlete reaches a total jumping distance of 224 metres.
## Which score do you expect for this athlete, if you take your calculated regression model as a basis

x <- 224
y <- intercept_a + (slop_b * x)
round(y, 4)


## g) (6 punkte) How does the variance of the variable total jumping distance change if this variable is measured in inches instead of metres? Hint: 1 metre = 39.37 inches
