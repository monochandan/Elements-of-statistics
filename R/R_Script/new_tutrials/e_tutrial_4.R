rm(list = ls())

#########################  12 march ###########################################

#1.1
urn_1 <- 8
urn_1_black <- 6
urn_1_yellow <- 2

urn_2 <- 15
urn_2_black <- 13
urn_2_yellow <- 2

urn_3 <- 12
urn_3_black <- 2
urn_3_yellow <- 10

(urn_1_black_prob <- urn_1_black / urn_1)
(urn_1_yellow_prob <- urn_1_yellow / urn_1)

(urn_2_black_prob <- urn_2_black / urn_2)
(urn_2_yellow_prob <- urn_2_yellow / urn_2)

(urn_3_black_prob <- urn_3_black / urn_3)
(urn_3_yellow_prob <- urn_3_yellow / urn_3)

(prob_1 <- urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob)
round(prob_1, 4)

## 1.2
prob_2 <- (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_black_prob) /
          ((urn_1_yellow_prob * urn_2_yellow_prob * urn_3_black_prob) +
          (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob))
round(prob_2, 4)

## 1.3
prob_3 <- (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_black_prob) +
          (urn_1_yellow_prob * urn_2_black_prob * urn_3_yellow_prob) + 
          (urn_1_black_prob * urn_2_yellow_prob * urn_3_yellow_prob) +
          (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob)
round(prob_3, 4)

#1.4
prob_4 <- (urn_1_yellow_prob * urn_2_black_prob * urn_3_yellow_prob) +
          (urn_1_black_prob * urn_2_yellow_prob * urn_3_black_prob)

round(prob_4, 4)



##2.1
total <- 100000
m1 <- 16000
m2 <- 30000
m3 <- 54000

m1_prob  <- m1 / total
m2_prob <- m2 / total
m3_prob <- m3 / total

m1_yellow <- m1 / 2
m1_green <- m1 / 2
m1_yellow_prob <- 0.5
m1_green_prob <- 0.5



(m2_red <- m2 / 4)
m2_white <- m2 / 4
m2_green <- m2/4
m2_yellow <- m2 / 4
m2_red_prob <- 0.25
m2_white_prob <- 0.25
m2_green_prob <- 0.25
m2_yellow_prob <- 0.25

m3_red <- m3 / 4
m3_white <- m3 / 4
m3_green <- m3/4
m3_yellow <- m3 / 4
m3_red_prob <- 0.25
m3_white_prob <- 0.25
m3_green_prob <- 0.25
m3_yellow_prob <- 0.25

(prob_1 <- (m1_prob * m1_yellow_prob) + (m2_prob * m2_yellow_prob) + (m3_prob * m3_yellow_prob))
round(prob_1, 4)

## 2.2
prob_2 <- (m2_prob * m2_white_prob) / ( (m2_prob * m2_white_prob) + (m3_prob * m3_white_prob))
round(prob_2, 4)

## 2.3
(total_red_yellow <- m2_red + m2_yellow + m3_red + m3_yellow)
(total_m2_m3 <- m2 + m3)
(total_orange <- (total_red_yellow / total_m2_m3) * 0.02 * 100000)


##3.1
(dice1 <- expand.grid(c(1,2,3,4,5,6)))
round(prop.table(table(dice1)), 4)

## 3.2
(dice2 <- expand.grid(lapply(X = 1:2, FUN = function(x){c(0,0,0,0,1,0)})))
(x <- prop.table(table(rowSums(dice2))))
round(x[2], 4)

##3.3

res1 <- 1/6
res2 <- 1/6 * 5/6
res3 <- 1/6 * 4/6
res4 <- 1/6 * 3/6
res5 <- 1/6 * 2/6
res6 <- 1/6 * 1/6
(res <- res1 + res2 + res3 + res4 + res5 + res6)
round(res, 4)

## 3.4
dice <- 4
rools <- 5
prob6 <- 1/6

(ex <- dice * rools * prob6)

##3.4
dice <- 5
rools_times <- 5
prob_6 <- 1/6
(ex_6 <- dice * rools_times * prob_6)
round(ex_6, 4)

## 4.1
x <- c(4, 6, 7)
y <- c(4, 5, 8)

(data <- matrix(c(0.18,0.07, 0.02, 0.14, 0.24, 0.04, 0.02, 0.02, 0.27), ncol = 3))
colnames(data) <- c("x=4", "x=6", "x=7")
rownames(data) <- c("y=4", "y=5", "y=8")


(f_y <- rowSums(data))
(f_x <- colSums(data))

(data <- addmargins(data))

(Ex_x <- weighted.mean(x, f_x))
round(Ex_x, 4)

## 4.2
(Ey_y <- weighted.mean(y, f_y))
(vary <- sum(f_y * (y - Ey_y)^2))

#4.3
fx7 <- 0.31
y4 <- (4 * 0.02)/fx7
y5 <- (5 * 0.02) / fx7
y8 <- (8 * 0.27)/ fx7 
(Ey_given_x <- (y4 + y5  +y8))
round(Ey_given_x, 4)

## 4.4
z <- 10 + (5 * y)
ey <- 5.65
varx <- 1.34

Ez <- 10 + (5 * 5.65)
round(Ez, 4)

##5.1

round((1/6 * 1/6 * 1/6), 4)

##5.2
(prob <- 3*(1/6 * 5/6 * 5/6) + 2*(1/6 * 5/6) + 1/6)
round(prob, 4)

## 5.3
e1<-1/6*1/6*1/6 ## in first rool  all cames 1
e2<-(5/6*1/6*1/6*1/6)*3 ## in first roll 2 cames 1 and other one not came 1 so again rolled that one (and this can happen for 3 dice or 3 possible combination)
e3<-(5/6*5/6*1/6*1/6*1/6)*3 ## in first rool for 2 dice it not came  1 other dice came one 1 (3 possible combination for this )
e4<-5/6*5/6*5/6*1/6*1/6*1/6 ### at first all not came 1 but second time all came one (only one combination)
ans<-e1+e2+e3+e4
round(ans, 4)

## 6.1
e_x <- 4
var_x <- 3
round(e_x/10, 4)

## 6.2
(1 - (var_x / 4^2))
#####################################
###################
##########
####
##
#



################################### 12 march #################################

urn_1 <- 8
urn_1_black <- 6
urn_1_yellow <- 2

urn_2 <- 15
urn_2_black <- 12
urn_2_yellow <- 3

urn_3 <- 12
urn_3_black <- 3
urn_3_yellow <- 9

(urn_1_black_prob <- urn_1_black / urn_1)
(urn_1_yellow_prob <- urn_1_yellow / urn_1)

(urn_2_black_prob <- urn_2_black / urn_2)
(urn_2_yellow_prob <- urn_2_yellow / urn_2)

(urn_3_black_prob <- urn_3_black / urn_3)
(urn_3_yellow_prob <- urn_3_yellow / urn_3)

(prob_1 <- urn_1_black_prob * urn_2_black_prob * urn_3_black_prob)
round(prob_1, 4)


## 1.2

prob_2 <- (urn_1_black_prob * urn_2_black_prob * urn_3_yellow_prob) /
  ((urn_1_black_prob * urn_2_black_prob * urn_3_yellow_prob) +
  (urn_1_black_prob * urn_2_black_prob * urn_3_black_prob))

round(prob_2, 4)


## 1.3
prob_3 <- (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob) +
  (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_black_prob) +
  (urn_1_black_prob * urn_2_yellow_prob * urn_3_yellow_prob) +
  (urn_1_yellow_prob * urn_2_black_prob * urn_3_yellow_prob)

round(prob_3, 4)



#1.4

(prob_4 <- urn_1_black_prob + urn_2_black_prob + urn_3_black_prob)


# 2.1

total <- 100000
m1 <- 18000
m2 <- 30000
m3 <- 52000

m1_prob  <- m1 / total
m2_prob <- m2 / total
m3_prob <- m3 / total

m1_yellow <- m1 / 2
m1_white <- m1 / 2
m1_yellow_prob <- 0.5
m1_white_prob <- 0.5



(m2_red <- m2 / 4)
m2_white <- m2 / 4
m2_green <- m2/4
m2_yellow <- m2 / 4
m2_red_prob <- 0.25
m2_white_prob <- 0.25
m2_green_prob <- 0.25
m2_yellow_prob <- 0.25

m3_red <- m3 / 4
m3_white <- m3 / 4
m3_green <- m3/4
m3_yellow <- m3 / 4
m3_red_prob <- 0.25
m3_white_prob <- 0.25
m3_green_prob <- 0.25
m3_yellow_prob <- 0.25

prob_1 <- (m1_prob * m1_yellow_prob ) + (m2_prob * m2_yellow_prob) + (m3_prob * m3_yellow_prob)
round(prob_1, 4)

## 2.2
prob_2 <- (m3_prob * m3_green_prob) /  ((m3_prob * m3_green_prob) + (m2_prob * m2_green_prob))
round(prob_2, 4)

##2.3
(total_red_yellow <- (m2_red + m2_yellow + m3_red + m3_yellow))
(total_m2_m3 <- (m2 + m3))

(total_orng <- (total_red_yellow / total_m2_m3) * 0.04 * 100000)


##3.1
(dice1 <- expand.grid(c(1,2,3,4,5,6)))
round(prop.table(table(dice1)), 4)

## 3.2
(dice2 <- expand.grid(lapply(1:2, function(x){c(0,1,0,0,0,0)})))
(x <- prop.table(table(rowSums((dice2)))))
round(x[2] + x[3], 4)

## 3.3

res1 <- 1/6
res2 <- 1/6 * 5/6
res3 <- 1/6 * 4/6
res4 <- 1/6 * 3/6
res5 <- 1/6 * 2/6
res6 <- 1/6 * 1/6
round(res1 + res2 + res3 + res4 + res5 + res6, 4)




#4.1
x <- c(2, 3, 10)
y <- c(2, 3, 9)

(data <- matrix(c(0.24, 0.14, 0.04,0.07, 0.17, 0.02, 0.03, 0.03, 0.26), ncol = 3))
rownames(data) <- c("y=2", "y=3", "y=9")
colnames(data) <- c("x=2", "x=3", "x=10")

(f_x <- colSums(data))
(f_y <- rowSums(data))

data <- addmargins(data)
data

(ex_x <- weighted.mean(x, f_x))

## 4.2
(ey_y <- weighted.mean(y, f_y))
(var_y <- sum(f_y * (y- ey_y)^2))

##4.3
fy2 <- 0.24 + 0.07 + 0.03
x2 <- (2 * 0.24) / fy2
x3 <- (3 * 0.07) / fy2
x10 <- (10 * 0.03) / fy2
round((x2 + x3 + x10), 4)

## 4.4
z <- 2 - (7 * x)

(varz <- 7^2 * 12.79)

##5.1
(5/6 * 5/6 * 5/6)

## 5.2
(3*(1/6 * 5/6 * 5/6) + 2*(1/6 * 5/6) + 1/6)

## 5.3
e1<-1/6*1/6*1/6 ## in first rool  all cames 1
e2<-(5/6*1/6*1/6*1/6)*3 ## in first roll 2 cames 1 and other one not came 1 so again rolled that one (and this can happen for 3 dice or 3 possible combination)
e3<-(5/6*5/6*1/6*1/6*1/6)*3 ## in first rool for 2 dice it not came  1 other dice came one 1 (3 possible combination for this )
e4<-5/6*5/6*5/6*1/6*1/6*1/6 ### at first all not came 1 but second time all came one (only one combination)
(ans<-e1+e2+e3+e4)
round(ans, 4)

##6.1 
6 / 10

##6.2
1 - (5 / 36)
########################################
#######################
#############
#####
###
###












#
urn_1 <- 8
urn_1_black <- 3
urn_1_yellow <- 5

urn_2 <- 15
urn_2_black <- 8
urn_2_yellow <- 7

urn_3 <- 12
urn_3_black <- 3
urn_3_yellow <- 9

(urn_1_black_prob <- urn_1_black / urn_1)
(urn_1_yellow_prob <- urn_1_yellow / urn_1)

(urn_2_black_prob <- urn_2_black / urn_2)
(urn_2_yellow_prob <- urn_2_yellow / urn_2)

(urn_3_black_prob <- urn_3_black / urn_3)
(urn_3_yellow_prob <- urn_3_yellow / urn_3)

##
urn_1 <- 8
urn_1_black <- 2
urn_1_yellow <- 6

urn_2 <- 15
urn_2_black <- 10
urn_2_yellow <- 5

urn_3 <- 12
urn_3_black <- 2
urn_3_yellow <- 10

(urn_1_black_prob <- urn_1_black / urn_1)
(urn_1_yellow_prob <- urn_1_yellow / urn_1)

(urn_2_black_prob <- urn_2_black / urn_2)
(urn_2_yellow_prob <- urn_2_yellow / urn_2)

(urn_3_black_prob <- urn_3_black / urn_3)
(urn_3_yellow_prob <- urn_3_yellow / urn_3)

## Please calculate the probability that every draw yields a black ball

(prob_all_black <- urn_1_black_prob * urn_2_black_prob * urn_3_black_prob)
round(prob_all_black, 4)

## Please calculate the probability that every draw yields a yellow ball.

prob_all_yellow <- urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob
round(prob_all_yellow,4)

## Assume that the balls taken from the third and first urn are both black.
# Please quantify the probability that the ball drawn from the remaining urn is not black.

(prob_black_3_black_1_notBlack_2 <- (urn_1_black_prob * urn_2_yellow_prob * urn_3_black_prob) /((urn_1_black_prob * urn_2_black_prob * urn_3_black_prob) +  (urn_1_black_prob * urn_2_yellow_prob * urn_3_black_prob)))
round(prob_black_3_black_1_notBlack_2, 4)

## Assume that the balls taken from the first and second urn are both black.
## Please quantify the probability that the ball drawn from the remaining urn is not black

prob_1 <- (urn_1_black_prob * urn_2_black_prob * urn_3_yellow_prob) /
          ((urn_1_black_prob * urn_2_black_prob * urn_3_yellow_prob) + (urn_1_black_prob * urn_2_black_prob * urn_3_black_prob))

round(prob_1, 4)

##  Please calculate the probability that at least two yellow balls are drawn from the three urns
(prob_at_least_2_yellow <- (urn_1_black_prob * urn_2_yellow_prob * urn_3_yellow_prob) +
                          (urn_1_yellow_prob * urn_2_black_prob * urn_3_yellow_prob) +
                          (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_black_prob) +
                          (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob))
round(prob_at_least_2_yellow, 4)

##Please calculate the probability that not more than one black ball is drawn from the three urns. 

prob_2 <- (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_yellow_prob) +
          (urn_1_black_prob * urn_2_yellow_prob * urn_3_yellow_prob) + 
          (urn_1_yellow_prob * urn_2_black_prob * urn_3_yellow_prob) +
          (urn_1_yellow_prob * urn_2_yellow_prob * urn_3_black_prob)
round(prob_2, 4)
## How many black balls would you expect in one round of drawing on average?
(drow_avg_black <- (urn_1_black_prob * urn_1_black)  + (urn_2_black_prob * urn_2_black) + (urn_3_black_prob * urn_3_black))
                  
round(drow_avg_black, 4)

##  Please calculate the probability that you draw both colours in alternating order (starting with black or yellow).

prob_3 <- (urn_1_black_prob * urn_2_yellow_prob * urn_3_black_prob) + 
          (urn_1_yellow_prob * urn_2_black_prob * urn_3_yellow_prob)
round(prob_3,4)


#####################################################
dailly_prod <- 100000

plant_1 <- 18000
plant_2 <- 32000
plant_3 <- 50000

(prob_plant_1 <- plant_1 / dailly_prod)
(prob_plant_2 <- plant_2 / dailly_prod)
(prob_plant_3 <- plant_3 / dailly_prod)

plant_1_red <- 9000
plant_1_white <- 9000

(prob_1_red <- plant_1_red / plant_1)
(prob_1_green <- plant_1_green / plant_1) 

plant_2_red <- 8000
plant_2_green <- 8000
plant_2_yellow <- 8000
plant_2_white <- 8000

(prob_2_red <- plant_2_red / plant_2)
(prob_2_green <- plant_2_green / plant_2)
(prob_2_white <- plant_2_white / plant_2)
(prob_2_yellow <- plant_2_yellow / plant_2) 

plant_3_red <- 12500
plant_3_green <- 12500
plant_3_yellow <- 12500
plant_3_white <- 12500

(prob_3_red <- plant_3_red / plant_3)
(prob_3_green <- plant_3_green / plant_3) 
(prob_3_white <- plant_3_white / plant_3)
(prob_3_yellow <- plant_3_yellow / plant_3) 


## You draw a jelly baby randomly from the daily production. 
## Please calculate the probability that this jelly baby is red.

(dorw_red_jelly <- (prob_plant_1 * prob_1_red) + (prob_plant_2 * prob_2_red) + (prob_plant_3 * prob_3_red))

## You draw a jelly baby randomly from the daily production. Please calculate the probability that this jelly baby is red

prob_1 <- (prob_plant_1 * prob_1_red) + (prob_plant_2 * prob_2_red) + (prob_plant_3 * prob_3_red)
round(prob_1, 4)

## Assume that the randomly drawn jelly baby is a white one.
## Please calculate the probability that this jelly baby was not produced on plant 2

(drow_white_jelly <- ( prob_plant_3 * prob_3_white ) / (( prob_plant_3 * prob_3_white ) + (prob_plant_2 * prob_2_white) ))

round(drow_white_jelly, 4)

## Assume that the randomly drawn jelly baby is a green one. 
## Please calculate the probability that this jelly baby was not produced on plant 3
prob_2 <- (prob_plant_2 * prob_2_green) / ((prob_plant_2 * prob_2_green) + (prob_plant_3 * prob_3_green))
round(prob_2,4)

## Because of a defect in the control system, sometimes the colours
## red and yellow mix up. This error concerns each plant to the same extent.
## In 5% of cases, an orange jelly baby is produced instead of a red or a yellow one.
### How many orange jellly babies would you expect to be produced during one daily production run?

(total_red_yellow_prodcution <- plant_2_red + plant_2_yellow + plant_3_red + plant_3_yellow)
(prob_red_yellow_in_2_machine <- total_red_yellow_prodcution / (plant_2 + plant_3))
orange_case <- 0.05
(total_orange_daily <- dailly_prod * prob_red_yellow_in_2_machine * orange_case) ## daily

## Because of a defect in the control system, sometimes the colours red and yellow mix up.
## This error concerns each plant to the same extent. In 5% of cases, an orange jelly baby is produced
## instead of a red or a yellow one. How many orange jellly babies would you expect 
## to be produced during one daily production run?

red_yellow_plant_2 <- 16000
red_yellow_plant_3  <- 25000
total_red_yellow <- red_yellow_plant_2 + red_yellow_plant_3

prob_3 <- (total_red_yellow / (32000 + 50000)) * 0.05 * 100000
round(prob_3, 4)
  
  


## Klaus rolls a single dice once. Please calculate the probability that the number 6 results.

(dice1 <- expand.grid(c(1, 2, 3, 4, 5, 6)))
round(prop.table(table(dice1)), 4)


## Klaus rolls a single dice once. Please calculate the probability that the number 3 results

(dice1 <- expand.grid((c(1,2,3,4,5,6))))
(round(prop.table(table(dice1)), 4))
## Klaus rolls a single dice once. Please calculate the probability that the number 1 results

dice1 <- expand.grid(c(1,2,3,4,5,6))
round(prop.table(table(dice1)),4)

dice1
## Now he adds a second dice to the cup and wants to roll both dice only once.
## He wonders what the probability of at least one dice showing the number 2 is.
## Please calculate the respective probability.


dice2<-expand.grid(lapply(X=1:2, FUN=function(x){c(0,1,0,0,0,0)}))
dice2
x<-prop.table(table(rowSums(dice2)))
x[2]+x[3]

## Now he adds a second dice to the cup and wants to roll both dice only once.
## He wonders what the probability of one dice showing the number 1 is.
## Please calculate the respective probability

(dice2 <- expand.grid(lapply(X = 1:2, FUN = function(x){c(1,0,0,0,0,0)})))
(x <- prop.table(table(rowSums(dice2))))
round(x[2] + x[3], 4)

### Now he adds a second dice to the cup and wants to roll both dice only once. 
## He wonders what the probability of at least one dice showing the number 6 is. 
## Please calculate the respective probability
dice2 <- expand.grid(lapply(X = 1:2, FUN = function(x){c(0,0,0,0,0,1)}))
dice2
(x <- prop.table(table(rowSums(dice2))))
round(x[2]+x[3], 4)

## Please calculate the probability that Klaus rolls in the first roll a smaller or the same number
## than/as in the second, utilising only one dice.

res.1 <- (1/6)       
res.2 <- (1/6)*(5/6) 
res.3 <- (1/6)*(4/6) 
res.4 <- (1/6)*(3/6) 
res.5 <- (1/6)*(2/6)
res.6 <- (1/6)*(1/6) 
res.c <- round(res.1+res.2+res.3+res.4+res.5+res.6, 4)
res.c
1-res.c

## Please calculate the probability that Klaus rolls in the 
## first roll a smaller or the same number than/as in the second, utilising only one dice.



## Please calculate the probability that Klaus rolls in the first roll
## a smaller or the same number than/as in the second, utilising only one dice.

res_1 <- (1/6)
res_2 <- (1/6) * (5/6)
res_3 <- (1/6) * (4/6)
res_4 <- (1/6) * (3/6)
res_5 <- (1/6) * (2/6)
res_6 <-  (1/6) * (1/6)
res <- round(res_1+res_2+res_3+res_4+res_5+res_6, 4)
res
1-res

## Finally, Klaus throws all 6 dice into the cup. He rolls the whole cup with all dices 4 times.
## On average, how many times would you expect the number 6 to result? Please calculate 
## the respective expected value

5/12
10/36
no_of_dices<-6
no_times<-4
prob.num<-1/6
exp.value<-no_of_dices*no_times*prob.num
exp.value

## Finally, Klaus throws all 6 dice into the cup. He rolls the whole cup with all dices 5 times.
## On average, how many times would you expect the number 6 to result? Please calculate the respective expected value.
dice <- 6
rolles <- 5
prob_num_6 <- 1/6
(expected_num_6 <- dice * rolles * prob_num_6)
round(expected_num_6, 4)
## Finally, Klaus throws all 4 dice into the cup. He rolls the whole cup with all dices 4 times.
## On average, how many times would you expect the number 6 to result? Please calculate the respective expected value

no_of_dices <- 4
no_of_times <- 4

num_6_prob <- 1/6
expect_num_6 <- no_of_dices * no_of_times * num_6_prob
round(expect_num_6, 4)

#########################

x<-c(0, 5, 6)
y<-c(7, 8, 9)

data<-matrix(c(0.23, 0.06, 0.05, 0.13, 0.16, 0.05, 0.02, 0.01, 0.29),nrow=3,byrow=TRUE)
data
rownames(data)<-c("Y=7","Y=8","Y=9")


colnames(data)<-c("X=0","X=5","X=6")
data

f_y<-rowSums(data)
f_y
f_x<-colSums(data)
f_x

###
x <- c(3,4,6)
y <- c(1,5,7)
data <- matrix(c(0.27,0.01, 0.06,0.17,0.11,0.06,0.03,0,0.29), nrow = 3, byrow = TRUE)
rownames(data) <- c("Y=1","Y=5","Y=7")
colnames(data) <- c("X=3","X=4","X=6")
data
f_x <- colSums(data)
f_x
f_y <- rowSums(data)
##  Please calculate the expected value of the random variable X. 

Ex_y<-weighted.mean(x=y,w=f_y)
Ex_y
Ex_x<-weighted.mean(x=x,w=f_x)
Ex_x
## Please calculate the expected value of the random variable Y. 

(Ex_y <- weighted.mean(y, f_y))

## Please calculate the expected value of the random variable X
(Ex_x <- weighted.mean(x = x, w = f_x ))

###
(Ex_y <- weighted.mean(x = y , w = f_y))

## Please calculate the variance of the random variable Y

var_x<-sum(f_x*(x-Ex_x)^2)
var_x
var_y<-sum(f_y*(y-Ex_y)^2)
var_y

## Please calculate the variance of the random variable X. 

(var_x <- sum(f_x * (x - Ex_x)^2))

## Please calculate the variance of the random variable Y.

var_y <- sum(f_y * (y- Ex_y) ^2)
var_y

## You know that the random variable X takes on the value 7. 
## Please calculate the conditional expected value E(Y|X=7) based on this additional knowledge.

fx7<-0.31
y4<-4*data[1,3]/fx7
y5<-5*data[2,3]/fx7
y8<-8*data[3,3]/fx7
E.Y.X.7<-y4+y5+y8
E.Y.X.7

## You know that the random variable Y takes on the value 8. 
## Please calculate the conditional expected value E(X|Y=8) based on this additional knowledge.

data <- addmargins(data)
data
fy_8 <- 0.34
x0 <- 0 * data[2,1] / fy_8
x5 <- 5 * data[2,2] / fy_8
x6 <- 6 * data[2,3] / fy_8

EX_Y_8 <- x0 + x5 + x6
round(EX_Y_8, 4)
## You know that the random variable X takes on the value 4. 
## Please calculate the conditional expected value E(Y|X=4) based on this additional knowledge.
(fx4 <- data[1,2] + data[2,2] + data[3,2])

y1 <- (1 * data[1,2]) / fx4
y5 <- (5 * data[2,2]) / fx4
y7 <- (7 * data[3,2]) / fx4

E_y_given_x_4 <- y1 + y5 + y7
round(E_y_given_x_4, 4)

## Now consider the transformed random variable Z=10+5Y. Please calculate the expected value E(Z)
## of this variable. You know that an expected value of E(Y)= 5.65 results for the random variable
## Y and that the random variable X has got a variance of Var(X)= 1.34.

Y<-5.65
Z<-10+ 5*Y
Z



## Now consider the transformed random variable Z=7+10X. 
## Please calculate the variance Var(Z) of this variable. 
## You know that an expected value of E(Y)= 4.28 results for 
## the random variable Y and that the random variable X has got a variance of Var(X)= 1.99


z <- 7 + ( 10 * x)
intercept_a <- 7
slop_b <- 10
Ex_y <- 4.28
var_x <- 1.99
var_z <- slop_b^2 * var_x
var_z
#4.5

## Now consider the transformed random variable Z=7+4Y. 
## Please calculate the variance Var(Z) of this variable. 
## You know that an expected value of E(X)= 3.49 results for 
## the random variable X and that the random variable Y has got a variance of Var(Y)= 0.66.

z <- 7 + 4*y
Ex <- 3.49
var_y <- 0.66

(var_z <- 4^2 * var_y)

#Please calculate the probability that the player rolls three times the number 1 in the first roll.

(prob1<-(5/6)*(5/6)*(5/6))
prob1

(prob1 <- (1/6) * (1/6) * (1/6)) 
prob1
## Please calculate the probability that the player rolls zero times the number 1 in the first roll. 

prob1<-(5/6)*(5/6)*(5/6)
prob1
round(prob1, 4)


##  Please calculate the probability that the player rolls three times the number 1 in the first roll

(prob_1 <- (1/6) * (1/6) * (1/6))

round(prob_1, 4)

##  Please calculate the probability that the number 1 results once in each of the three rolls.

prob_2 <- 3 * ((1/6) * (5/6) * (5/6)) + 2 * ((1/6) * (5/6)) + 1/6
round(prob_2,4)


##  Please calculate the probability that the goal of the game - 
## rolling three times the number 1 - is reached within no more than two rolls.

e1<-1/6*1/6*1/6 ## in first rool  all cames 1
e2<-(5/6*1/6*1/6*1/6)*3 ## in first roll 2 cames 1 and other one not came 1 so again rolled that one (and this can happen for 3 dice or 3 possible combination)
e3<-(5/6*5/6*1/6*1/6*1/6)*3 ## in first rool for 2 dice it not came  1 other dice came one 1 (3 possible combination for this )
e4<-5/6*5/6*5/6*1/6*1/6*1/6 ### at first all not came 1 but second time all came one (only one combination)
ans<-e1+e2+e3+e4
round(ans, 4)

########
# Probability of rolling a 1 on a single die
p_1 <- 1/6

# Probability of not rolling a 1 on a single die
p_not_1 <- 5/6

# Probability of rolling all three 1s in one roll
p_all_1 <- p_1^3

# Probability of rolling two 1s and not on the third, then rolling that die again and getting a 1
p_two_1_and_not_third <- p_1 * p_not_1^2 * p_1

# Probability of rolling one 1 and not on the other two, then rolling those two dice again and getting 1 on both
p_one_1_and_not_other_two <- p_1 * p_not_1^2 * p_1

# Total probability of achieving the goal in one roll
p_goal_in_one_roll <- p_all_1 + p_two_1_and_not_third + p_one_1_and_not_other_two

# Probability of not achieving the goal in one roll
p_not_achieving_goal_in_one_roll <- 1 - p_goal_in_one_roll

print(p_not_achieving_goal_in_one_roll)

########
#Please calculate the probability that the number 1 results once in each of the three rolls.

prob2<-3*((1/6)*(5/6)*(5/6)) + 2*((1/6)*(5/6)) + (1/6)
prob2

## Please calculate the probability that the number 1 results once in each of the three rolls

prob2 <- 3 * ((1/6) * (5/6) * (5/6)) + 2 * ((1/6) * (5/6)) + 1/6
round(prob2,4)
#Please calculate the probability that the goal of the game - 
#rolling three times the number 1 - is reached within no more than two rolls.
e1<-1/6*1/6*1/6 ## in first rool  all cames 1
e2<-(5/6*1/6*1/6*1/6)*3 ## in first roll 2 cames 1 and other one not came 1 so again rolled that one (and this can happen for 3 dice or 3 possible combination)
e3<-(5/6*5/6*1/6*1/6*1/6)*3 ## in first rool for 2 dice it not came  1 other dice came one 1 (3 possible combination for this )
e4<-5/6*5/6*5/6*1/6*1/6*1/6 ### at first all not came 1 but second time all came one (only one combination)
ans<-e1+e2+e3+e4
ans




## Please calculate the probability that the goal of the game - rolling three times
## the number 1 - is reached within no more than two rolls.

all_came_1 <- 1/6 * 1/6 * 1/6
two_came_1 <- (1/6 * 1/6 * 5/6 * 1/6) * 3
once_came_1 <-  (1/6 * 5/6 * 5/6 * 1/6 * 1/6 ) * 3
all_not_came_1 <- 5/6 * 5/6 * 5/6 * 1/6 * 1/6 * 1/6
prob <- all_came_1 + two_came_1 + once_came_1 + all_not_came_1
round(prob,4)



#####

Ex_X <- 5
var_X <- 5
prob_X_greater_10 <- Ex_X / 10
round(prob_X_greater_10,4)










