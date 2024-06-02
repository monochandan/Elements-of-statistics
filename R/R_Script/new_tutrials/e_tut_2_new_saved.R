#rm(list = ls())
# In order to analyse the relation between movie genre and 
# field of study, calculate the value of the standardised contingency coefficient

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


# Please calculate Pearson's correlation coefficient 
# for the variables preparation time in hours and achieved points.

cor_person <- cor(pth, pa, method = "pearson")
round(cor_person, 4)

#  You already know that the achieved number of points is linearly 
# dependent on the time spent preparing. Based on that knowledge, 
# you estimate a linear regression model for all 100 students and 
# get a value of R2= 0.3301 for the coefficient of determination. 
# According to this, which of the following values is a plausible 
# choice for Pearson's correlation coefficient in the whole sample?


# how to calculate pearson correlation coefficient if we know one veriable is 
# dependent on other veriable and the r square value is 0.3301 
# (coefficient of determination)

R_squared <- 0.3301
correlation <- sqrt(0.3301)
round(correlation, 4)

#cor_person <- cor(pth, pa, method = "pearson")






# Finally, you want to calculate Spearman's rank correlation coefficient 
# based on the data for all 100 students interviewed. 
# You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 66655.

## n<-length(pth)
## SP<-cor(rank(pth),rank(pa),method = "spearman")
## SP         #OR

## Sp<-1-(6*sum((rank(pth)-rank(pa))^2)/(n*(n^2-1)))
## Sp

#rm(list = ls())
n <- length(pth)

rsp <- 1 - ((6*66655)/(n * (n^2 - 1)))
rsp
rsp <- cor(rank(pth), rank(pa), method = "spearman")
rsp
round(rsp, 4)


##  Please calculate the missing value of the marginal distribution in the table.

contingency_table <- matrix(c(10, 10, 20, 20, 30, 10), nrow = 2)
contingency_table
addmargins(contingency_table)
rownames(contingency_table) <- c("A1","A2")
colnames(contingency_table) <- c("B1","B2","B3")
contingency_table
new_table <- addmargins(contingency_table)
new_table
new_table[1,2] <- NA
new_table
(60 * 40) / 100
######################## ANswer #############################
RData
data<-data.frame(RData)
data<-as.matrix(data)
data
addmargins(data)
####################################
# Please calculate the relative joint frequency pjk 
# for the missing cell in the table at hand

292 - (81 + 51 + 33 + 55) 


# For now, we assume independence of the two variables.
# Please calculate the absolute frequency n???jk, with j=5 and k=2
# under the assumption of independence
data
RData
data[5,4] <- 72 ###  adding missing value, otherwise sum will be NA
data
##### ANSWER 1 --- for individual col and row
j <- 5
k <- 2
n_j <- sum(data[j,])
n_k <- sum(data[,k])
margin.table(data)
n_j_k_star <- (n_j * n_k) / margin.table(data)
round(n_j_k_star, 4)

#### ANSWER 2 --- for all cell of the matrix
n_j_k_star <- margin.table(data,1) %*% t(margin.table(data,2)) / margin.table(data)

## Error in apply(x, margin, sum) : 'MARGIN' does not match dim(X)
#margin.table(data, 1)
round(n_j_k_star, 4)




#  Please calculate the conditional relative frequency pk|j with j=4 and k=1

j <- 4
k <- 1
n_j_k <- sum(data[j,k])
n_j <- sum(data[j,])
p_k_j <- n_j_k / n_j
round(p_k_j, 4)




# You are interested in the bivariate empirical distribution function 
# as well. Please calculate the value of Fn(x,y) at x=I3 and y=R

F_j_k<-t(apply(X=apply(X=data,MARGIN = 2,FUN=cumsum),MARGIN=1,FUN=cumsum))
round(F_j_k / margin.table(data), 4)




x1 <- 2
y1 <- 3
x2 <- 5
y2 <- 7

# Calculate slope (m)
slope <- (y2 - y1) / (x2 - x1)

# Calculate intercept (c)
intercept <- y1 - slope * x1

# Plotting the points
plot(x = c(x1, x2), y = c(y1, y2), xlim = c(0, 7), ylim = c(0, 8), 
     xlab = "X-axis", ylab = "Y-axis", main = "Regression Line Through 2 Points")

# Draw the regression line
abline(a = intercept, b = slope, col = "red")


#  Calculate the relation between the reached score and the 
# total jumping distance by means of the correlation coefficient of Bravais-Pearson
RData

cor_BP <- cor(RData$Score, RData$TotalJumpingDistance, method = "pearson")
round(cor_BP, 4)

# You assume a linear relationship between the total jumping distance 
# and the reached score (dependent variable). Hence, set up a suitable 
# linear regression model and calculate the intercept by means 
# of the method of ordinary least squares

res <- lm(RData$Score ~ RData$TotalJumpingDistance)
res




?lm
# Furthermore, determine the value of the coefficient of determination 
# of your regression model.
summary(res)

# State the value of the sum of all residuals of your regression model

0

# State the value of the sum of the squared residuals of your regression model

sum(residuals(res)^2)

# Assume that an athlete reaches a total jumping distance of 250.5 metres. 
# Which score do you expect for this athlete, if you take your calculated 
# regression model as a basis.
x <- 250.5
#x<-279
#a<-2.04642
#b<- -273.73025
#y<-a*x+b
#y
a <- 2.0392
b <- -279.8465 
y <- a*x + b
y


#  How does the value of the coefficient of determination change if the 
# jumping distance is measured in inch instead of metres? 
# Hint: 1 metre = 39.37 inches.

################################################################################
################################################################################
################################################################################

# In order to analyse the relation between movie genre and field of study,
# calculate the value of the standardised contingency coefficient.

m8 <- matrix(c(53, 228, 9, 60, 215, 18, 132, 107, 178), ncol = 3)
m8
colnames(m8) <- c("Action", "Fantasy", "Comedy")
rownames(m8) <- c("Economics", "Business", "Social Sciences")
m8
chsq <- chisq.test(m8)$statistic
chsq
kk <- sqrt(chsq/(sum(m8) + chsq))
M  <- min(dim(m8))
k_max <- sqrt((M-1)/M)
k_star <- kk/k_max

round(k_star, 4)


#  Please calculate the empirical covariance between 
# the variables preparation time in hours and achieved points

pth <- c(34, 37, 85, 67, 34, 116, 42)
pa <- c(132, 166, 218, 180, 135, 235, 138)
e_cv <- sum((pth - mean(pth)) * (pa - mean(pa))) / 7
round(e_cv, 4)

# Please calculate Pearson's correlation coefficient for 
# the variables preparation time in hours and achieved points.

(cor_bp <- cor(pth, pa, method = "pearson"))
round(cor_bp,4)

# Suppose that several students spent the same amount of time 
# for preparation and also end up with the same amount of points 
# achieved in the exam. You plan to calculate 
# the Spearman's rank correlation coefficient in order 
# to assess the relation between the two variables. Which problem links to this procedure?

# quadratic contingency table
# negative variance
# bindings
# negative correlation

# Finally, you want to calculate Spearman's rank correlation coefficient based
# on the data for all 100 students interviewed. You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 13758.

n <- length(pth)

rsp <- 1 - ((6*66655)/(n * (n^2 - 1)))
rsp
rsp <- cor(rank(pth), rank(pa), method = "spearman")
rsp
round(rsp, 4)



cor_sp <- cor(rank(pth), rank(pa), method = "spearman")
cor_sp

one <- 6 * 13578 
two <- 7 * (7^2 - 1)
rsp <- 1 - (one/two)
rsp



# Please calculate the missing value of the marginal distribution in the table

RData
addmargins(RData)

# Please calculate the absolute joint frequency njk for the missing cell in the table at hand.
data<-data.frame(RData)
j <- 5
k <-1

#n_j <- sum(data[j,])
#n_k <- sum(data[k,])
#n_j_k <- (n_j * n_k) / 1201

307 - (56+65+53+19+27)

data[j,k] <- 87
data

# For now, we assume independence between the two variables. 
# Please calculate the relative frequency p???jk, with j=1 and k=3 under the assumption of independency
data<-as.matrix(data)
j <- 1
k <- 3

n_j_k <- data[j,k]

p_j_k <- n_j_k / sum(data)
round(p_j_k, 4)

#  Please calculate the conditional relative frequency pj|k with j=1 and k=4.

j <- 1
k <- 4
n_j_k <- data[j,k]
cond_p_k_j <- n_j_k / sum(data[j,])
round(cond_p_k_j, 4)

#  You are interested in the bivariate empirical distribution function as well. 
#  Please calculate the value of Fn(x,y) at x=I2 and y=R.

F_j_k<-t(apply(X=apply(X=data,MARGIN = 2,FUN=cumsum),MARGIN=1,FUN=cumsum))
round(F_j_k / margin.table(data), 4)



#rm(list = ls())


# Calculate the relation between the reached score and the total jumping distance
# by means of the rank correlation coefficient of Spearman.

RData
cor_BP <- cor(RData$Score, RData$TotalJumpingDistance, method = "spearman")
round(cor_BP, 4)

#  You assume a linear relationship between the total jumping distance and
# the reached score (dependent variable). Hence, set up a suitable linear
# regression model and calculate the intercept by means of the method of ordinary least squares. 

res <- lm(RData$Score ~ RData$TotalJumpingDistance)
res

#  Furthermore, determine the value of the coefficient of determination of your regression model
summary(res)

# Now assume that the total jumping distance should be measured in kilometres instead of metres.
# Which value does the coefficient of determination have after this transformation?

 ## meter to km  --- divided by 1000
# same value from summary(res)
## km to meter --- multiply by 1000


#  State the value of the sum of the squared residuals of your regression model.

0 
#  Assume that an athlete reaches a total jumping distance of 234.5 metres.
# Which score do you expect for this athlete, if you take your calculated regression model as a basis. 

x <- 234.5
#x<-279
#a<-2.04642
#b<- -273.73025
#y<-a*x+b
#y
a <- 2.205
b <- -309.514
y <- a*x + b
y


# If the correlation coefficient of Bravais-Pearson takes on the value zero, this always implies that...

## there is no linear relationship between  variables --- answer
## there is no casual but a linear relationship between the veriables
## the variables are independent of each other
## there is a casual relation ship between variables



###############################################################################
###############################################################################
###############################################################################

# In order to analyse the relation between movie genre and field of study,
# calculate the value of the standardised contingency coefficient. 

m9 <- matrix(c(56, 225, 27, 73, 220, 24, 109, 200, 65), ncol = 3)
colnames(m9) <- c("Action", "Fantasy", "Comedy")
rownames(m9) <- c("Economics", "Business", "Social Sciences")
m9

chsq <- chisq.test(m9)$statistic
KK <- sqrt(chsq/(sum(m9) + chsq))
M <- min(dim(m9))
K_max <- sqrt((M-1) / M)
K_star <- KK/K_max
round(K_star, 4)


##  (12 Points) Please calculate the empirical covariance between the variables 
## preparation time in hours and achieved points.

pth <- c(71, 77, 104, 40, 93, 56, 66)
pa <- c(173, 129, 188, 132, 207, 208, 196)

emperical_covariance <- sum((pth - mean(pth)) * (pa - mean(pa))) / 7

round(emperical_covariance, 4)

## Please calculate Pearson's correlation coefficient for the 
## variables preparation time in hours and achieved points

cor_pearson <- cor(pth, pa, method = "pearson")
round(cor_pearson,4)



## In addition, you calculated Pearson's correlation coefficient over all 100 students 
## for the same set of variables as before and got a value of 0.3381. Which 
## one of the following values matches the one of the coefficient of determination? 

r_squared <- (0.3381)^2
round(r_squared, 4)

##  Finally, you want to calculate Spearman's rank correlation coefficient based
##  on the data for all 100 students interviewed. You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 104790.
n <- length(pth)
numerator <- 6 * 104790
denominator <- n * (n^2 - 1)
r_sp <- 1 - (numerator / denominator)
round(r_sp, 4)


## Please calculate the missing value of the marginal distribution in the table.
RData
data<-data.frame(RData)
addmargins(RData)

##  Please calculate the relative joint frequency pjk for the missing cell in the table at hand.
405 - (75 + 32 + 100 + 107)
RData[5, 4] <- 91
RData

##  For now, we assume independence of the two variables. 
## Please calculate the absolute frequency n???jk, with j=3 and k=1 under the assumption of independence.

j <- 3
k <- 1
margin.table(RData)
n_j_k <- (sum(RData[j,]) * sum(RData[, k])) / margin.table(RData)
round(n_j_k,4)


## Please calculate the conditional relative frequency pk|j with j=3 and k=1

j <- 3
k <- 1
p_cond_k_j <- RData[j,k] / sum(RData[j,])
round(p_cond_k_j, 4)

##  You are interested in the bivariate empirical distribution function as well.
##  Please calculate the value of Fn(x,y) at x=I3 and y=A.

f_j_k <- t(apply(apply(RData, 2, cumsum), 1, cumsum))
round(f_j_k/margin.table(RData), 4)


## Calculate the relation between the reached score and the total jumping distance by
## means of the correlation coefficient of Bravais-Pearson
RData
cor_bp <- cor(RData$Score, RData$TotalJumpingDistance, method = "pearson")
round(cor_bp, 4)

## You assume a linear relationship between the total jumping distance and the
## reached score (dependent variable). Hence, set up a suitable linear regression
## model and calculate the intercept by means of the method of ordinary least squares.
?lm
res <-lm(RData$TotalJumpingDistance~RData$Score)
res

## Furthermore, determine the value of the coefficient of determination of your regression model.
summary(res)

## Now assume that the total jumping distance should be measured in kilometres
## instead of metres. Which value does the coefficient of determination have after this transformation?
data1<-RData$TotalJumpingDistance/1000
data1
score <- RData$Score
res1<-lm(score~data1)
res1
summary(res1)

##  State the value of the sum of the squared residuals of your regression model.

sq_residuals <- sum(residuals(res)^2)
round(sq_residuals, 4)

## Assume that an athlete reaches a total jumping distance of 234.5 metres.
## Which score do you expect for this athlete, if you take your calculated regression model as a basis
summary(res)
intercept <- 134.56818
slop <- 0.48597
x <- 234.5
y <- slop*x + intercept
round(y, 4)

## The correlation coefficient of Spearman.


################################################################################
##################################### 4 mark( 73.08 )    ##############################
################################################################################

#  In order to analyse the relation between movie genre and field of study,
#  calculate the value of the standardised contingency coefficient.

mm1 <- matrix(c(50, 234, 17, 73, 203, 28, 58, 208, 129), ncol = 3)
mm1
colnames(mm1) <- c("Action","Fantasy", "Comedy")
rownames(mm1) <- c("Economics", "Business", "Social Sciences")
mm1
chsq <- chisq.test(mm1)$statistic
chsq
KK <- sqrt(chsq / (sum(mm1) + chsq ))
M <- min(dim(mm1))
K_max <- sqrt((M-1) / M)
K_star <- KK / K_max
round(K_star, 4)

#  Please calculate the empirical covariance between the variables preparation
#  time in hours and achieved points.

pth <- c(152, 56, 101, 138, 105, 123, 102)
pa <- c(238, 172, 186, 199, 170, 203, 145)
(rnk_pth <- rank(pth))
(rnk_pa <- rank(pa))
cor_sp <- cor(rnk_pth, rnk_pa, method = "spearman")
cor_sp

len <- length(pth)
 
empirical_cov <- sum((pth - mean(pth)) * (pa - mean(pa))) / len
round(empirical_cov, 4 )


#  Please calculate Pearson's correlation coefficient for the
#  variables preparation time in hours and achieved points. 

cor_bp <- cor(pth, pa, method = "pearson")
round(cor_bp,4)

# Suppose that several students spent the same amount of 
# time for preparation and also end up with the same amount 
# of points achieved in the exam. You plan to calculate 
# the Spearman's rank correlation coefficient in order 
# to assess the relation between the two variables. 
# Which problem links to this procedure?

pth <- c(152, 152, 152, 152, 152, 152, 152)
pa <- c(238, 238, 238, 238, 238, 238, 238)
(rnk_pth <- rank(pth))
(rnk_pa <- rank(pa))
cor_sp <- cor(rnk_pth, rnk_pa, method = "spearman")
cor_sp


# Finally, you want to calculate Spearman's rank correlation coefficient
# based on the data for all 100 students interviewed. 
# You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 56934. 


numerator <- 6 * 56934
denominator <- 100*(100^2 - 1)

cor_sp <- 1 - (numerator / denominator)
round(cor_sp,4)

# Please calculate the missing value of the marginal distribution in the table.
rm(list = ls())
RData
addmargins(RData)

#  Please calculate the absolute joint frequency njk for the missing cell in the table at hand.
205 - (51 + 12 + 12 + 57 + 45)
RData[2,4] <- 28
RData
addmargins(RData)

#  For now, we assume independence between the two variables. Please calculate 
#  the relative frequency p???jk, with j=1 and k=1 under the assumption of independency.
#rm(list = ls())

data<-as.matrix(RData)
j <- 1
k <- 1

n_j_k <- data[j,k]

p_j_k <- n_j_k / sum(data)
round(p_j_k, 4)

n_j_k <- RData[1,1]
n <- margin.table(RData)
p_j_k <- n_j_k / n
round(p_j_k , 4)

#p_j <- 8/230
#p_k <- 8 / 190

#(230/1201) + (190/1201)

#p_j_k <- (p_j * p_k) / 1201
#p_j_k
p_j <- prop.table(RData[j,])
p_j
p_k <- prop.table(RData[,k])
p_k
p_j_k <- sum(p_j)/n + sum(p_k)/n
p_j_k


n<-1201
n_jk_star<-(230*190)/n
p_jk<-n_jk_star/n
p_jk




#  Please calculate the conditional relative frequency pj|k with j=1 and k=2
j <- 1
k <- 2

p_k_j <- RData[j,k] / sum(RData[,k])
round(p_k_j,4)

#  You are interested in the bivariate empirical distribution function as well.
#  Please calculate the value of Fn(x,y) at x=I3 and y=B.

j <- 3
k <- 4

p_j_k <- prop.table(RData)
p_j_k
f_j_k <- t(apply(apply(p_j_k, 2, cumsum), 1, cumsum))
f_j_k
round(f_j_k,4)


#rm(list = ls())

#  Calculate the relation between the reached score and the total
# jumping distance by means of the rank correlation coefficient of Spearman.

RData
rnk_score <- rank(RData$Score)
rnk_dist <- rank(RData$TotalJumpingDistance)

cor_sp <- cor(rnk_score, rnk_dist, method = "spearman")
round(cor_sp,4)

# You assume a linear relationship between the total jumping distance and
# the reached score (dependent variable). Hence, set up a suitable linear
# regression model and calculate the intercept by means of the method of ordinary least squares. 

res <- lm(RData$Score~RData$TotalJumpingDistance)
res

# determine the value of the coefficient of determination of your regression model.
summary(res)


# State the value of the sum of all residuals of your regression model.
sum_of_res <- sum(RData$Score - residuals(res))
sum_of_res

# State the value of the sum of the squared residuals of your regression model. 

sum_of_sqrd_res <- sum(residuals(res)^2)
round(sum_of_sqrd_res,4)

#  Assume that an athlete reaches a total jumping distance of 261.5 metres.
# Which score do you expect for this athlete, if you take your calculated regression model as a basis

summary(res)
intercept <- -252.6364
slop <- 1.9672
x <- 261.5
y <- slop*x + intercept
round(y, 4)



# How does the value of the coefficient of determination change
# if the jumping distance is measured in inch instead of metres? Hint: 1 metre = 39.37 inches.

# no change


###############################################################################
#################################### 6 (mark: )################################
################################################################################

#  In order to analyse the relation between movie genre and field of study,
# calculate the value of the standardised contingency coefficient.

mm5 <- matrix(c(57, 241, 19, 73, 201, 17, 71, 117, 204), ncol = 3)
colnames(mm5) <- c('Action', 'Fantasy', 'Comedy')
rownames(mm5) <- c('Economics', 'Business', 'Social Sciemces')
mm5
(chsq <- chisq.test(mm5)$statistic)
(KK <- sqrt(chsq/ (sum(mm5)  + chsq))) 
(M <- min(dim(mm5)))
(K_max <- sqrt((M - 1) / M))
(K_Star <- KK/K_max)
round(K_Star, 4)

chsq <- chisq.test(mm5)$statistic
chsq
KK <- sqrt(chsq / (sum(mm5) + chsq ))
M <- min(dim(mm5))
K_max <- sqrt((M-1) / M)
K_star <- KK / K_max
round(K_star, 4)



##  Please calculate the empirical covariance between the variables preparation time in hours and achieved points.

pth <- c(41, 104, 42, 48, 128, 93, 85)
pa <- c(173, 163, 167, 161, 217, 136, 102)

(emp_cov <- 1/length(pth) * (sum((pth - mean(pth)) * (pa - mean(pa)))))

## Please calculate Pearson's correlation coefficient for the variables preparation time in hours and achieved points.

(cor_p <- cor(pth, pa, method = 'pearson'))
## Suppose that several students spent the same amount of time for preparation and also end up with the
## same amount of points achieved in the exam. You plan to calculate the 
## Spearman's rank correlation coefficient in order to assess the relation between
## the two variables. Which problem links to this procedure?

# bindings


## Finally, you want to calculate Spearman's rank correlation coefficient based
### on the data for all 100 students interviewed. You already know that: ???100i=1[Rk(xi)???Rk(yi)]2 = 145463. 

n <- 100
(r_s_p <- 1 - ((6 * 145463) / (n * (n^2 - 1))))  
round(r_s_p, 4)


#rm(list = ls())

RData

#  Please calculate the missing value of the marginal distribution in the table.
sum(126, 87, 28, 6, 138)

# Please calculate the relative joint frequency pjk for the missing cell in the table at hand
376 - (48 + 41 + 36 + 126)
j <- 5
k <- 1
RData[j,k] <- 125
(margin.table(RData))

(p_j_k <- RData[j,k] / margin.table(RData))
round(p_j_k,4)

#  For now, we assume independence of the two variables. 
# Please calculate the absolute frequency n???jk, with j=3 and k=5 under the assumption of independence.

j <- 3
k <- 5
(sum(RData[j,]))
(n_j_k_star <- (sum(RData[j,]) * sum(RData[,k])) / margin.table(RData))
round(n_j_k_star,4)

# Please calculate the conditional relative frequency pj|k with j=2 and k=3. 

j <- 2
k <- 3
(p_k_j <- RData[j,k] / sum(RData[j,]))
round(p_k_j, 4)


#  You are interested in the bivariate empirical distribution function as well.
# Please calculate the value of Fn(x,y) at x=I2 and y=R. 


j <- 2
k <- 2
(p_j_k <- prop.table(RData))
(f_j_k <- t(apply(apply(p_j_k, 2, cumsum), 1, cumsum)))
round(f_j_k, 4)

#rm(list = ls())
# Calculate the relation between the reached score and the total jumping distance by means of the rank correlation coefficient of Spearman. 

RData

cor_sp <- cor(RData$TotalJumpingDistance, RData$Score, method = 'spearman')
round(cor_sp, 4)

# You assume a linear relationship between the total jumping distance and the reached score
# (dependent variable). Hence, set up a suitable linear regression model and calculate the
# intercept by means of the method of ordinary least squares. 

r_m  <- lm(RData$Score~RData$TotalJumpingDistance) 

summary(r_m)
-309.514 

# Furthermore, determine the value of the coefficient of determination of your regression model.
0.7713
#  Now assume that the total jumping distance should be measured in kilometres instead of metres.
# Which value does the coefficient of determination have after this transformation? 
0.7713

#  State the value of the sum of the squared residuals of your regression model.
sum_sqrd_res <- sum(residuals(r_m)^2)
round(sum_sqrd_res, 4)

#  Assume that an athlete reaches a total jumping distance of 234.5 metres. 
# Which score do you expect for this athlete, if you take your calculated regression
# model as a basis. 
x <- 234.5
intercept <- -309.514
slop <- 2.205
y <- (slop * x)  + intercept 
round(y, 4)

#  If the correlation coefficient of Bravais-Pearson takes on the value zero, this always implies that...















