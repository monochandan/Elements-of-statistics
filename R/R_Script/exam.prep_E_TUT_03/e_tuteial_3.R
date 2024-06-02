rm(list = ls())

head(RData)
RData


###########################  12 march #########################################

#1.1
(ytk <- ts(RData$BIP_IT, start = c(2000, 1), end = c(2012, 4), frequency = 4))
(ytk_trend <- decompose(ytk, type = "additive")$trend)
round(ytk_trend[27], 4)


## 1.2
(ytk_norm <- decompose(ytk, type = "additive")$seasonal)
round(ytk_norm[1], 4)

##1.3.

(ytk_norm <- decompose(ytk, type= "additive")$figure)
round(sum(ytk_norm), digit = 8)

## 1.4
khacha <- 30
(zt <- ytk - ytk_norm)
(zt_khacha <- zt[khacha])
assumed_trend <- 386.7682
(random_error <- zt_khacha - assumed_trend)
round(random_error, 4)

khacha <- 26
assumed_component <- 386.7682
(error <- ytk - ytk_norm)

(error_at_khacha <- error[khacha])
(random_error_error_of_that_khacha <- error_at_khacha - assumed_component)
round(random_error_error_of_that_khacha, 4)

## 1.5
qrtr <- 3
khacha <- 55
t <- 1:length(zt)
model <- lm(zt ~ t)

intercept_a <- summary(model)$coeff[1,1]
slop_b <- summary(model)$coeff[2,1]

(y <- intercept_a + (khacha * slop_b))
(final_pred <- y + ytk_norm[3])


## 1.6
a <- 14
b <- 42

(gdp_change <- (ytk[b] / ytk[a] - 1) * 100)
round(gdp_change, 4)


## 2.1
b <- 44
r <- 3

total <- 47

round((prob_r <- r / total),4)
round((prob_b <- b / total),4)

### 2.2
prob_corr <- 0.82

(saw_brown <- (prob_corr * prob_b) + ((1-prob_corr) * prob_r))
round(saw_brown, 4)

## 2.3
(indeed_b_saw_b <- (prob_corr * prob_b) / saw_brown)
round(indeed_b_saw_b, 4)


## 3.1
urn1_total <- 8
urn1_b <- 7
urn1_y <- 1
urn1_b_prob <- urn1_b / urn1_total
urn1_y_prob <- urn1_y / urn1_total

urn2_total <- 15
urn2_b <- 5
urn2_y <- 10
urn2_b_prob <- urn2_b / urn2_total
urn2_y_prob <- urn2_y / urn2_total

urn3_total <- 12
urn3_b <- 2
urn3_y <- 10
urn3_b_prob <- urn3_b / urn3_total
urn3_y_prob <- urn3_y / urn3_total

##3.1
(prob_1 <- urn1_y_prob * urn2_y_prob * urn3_y_prob)
round(prob_1, 4)

## 3.2
prob_2 <- ( urn1_b_prob * urn2_y_prob * urn3_b_prob ) /
          (( urn1_b_prob * urn2_y_prob * urn3_b_prob ) + 
          ( urn1_b_prob * urn2_b_prob * urn3_b_prob ) )


round(prob_2, 4)

##3.3
prob_3 <- ( urn1_y_prob * urn2_y_prob * urn3_y_prob ) +
  ( urn1_b_prob * urn2_y_prob * urn3_y_prob ) +
  ( urn1_y_prob * urn2_b_prob * urn3_y_prob ) +
  ( urn1_y_prob * urn2_y_prob * urn3_b_prob )

round(prob_3, 4)

## 3.4

(prob_4 <- urn1_y_prob + urn2_y_prob + urn3_y_prob)
round(prob_4, 4)


##4.1
total <- 100000
m1 <- 15000
m2 <- 35000
m3 <- 50000

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

prob_1 <- (m1_prob * m1_yellow_prob) + (m2_prob * m2_yellow_prob) + (m3_prob * m3_yellow_prob)
round(prob_1, 4)

##4.2
prob_2 <- (m3_prob * m3_green_prob) / ((m3_prob * m3_green_prob) + (m2_prob * m2_green_prob))
round(prob_2, 4)

## 4.3
total_red_yellow <- (m2_red + m2_yellow + m3_red + m3_yellow)
total_m2_m3 <- m2 + m3

(total_orange <- (total_red_yellow / total_m2_m3) * 0.05* 100000)

################################################
#################################
###################
##########
####
##




############### 12 march ###################################################
head(RData)
##1.1
(ytk <- ts(data = RData$BIP_FI, start = c(2000, 1), end = c(2012, 4), frequency = 4))
(trend <- decompose(ytk, type = "additive")$trend)
round(42.67600, 4)

#1.2
(ytk_norm <- decompose(ytk, type = "additive")$seasonal)
(ytk_norm <- decompose(ytk, type = "additive")$figure)

round(ytk_norm[2], 4)
## 1.3
round(sum(ytk_norm), digit = 8)

## 1.4
khacha <- 9
assumed_component <- 35.4274
(error <- ytk - ytk_norm)

(error_at_khacha <- error[khacha])
(random_error_error_of_that_khacha <- error_at_khacha - assumed_component)
round(random_error_error_of_that_khacha, 4)

## 1.5
khacha <- 53
qrtr <- 1

t <- 1:length(error)

reg <- lm(error~t)
intercept_a <- summary(reg)$coeff[1,1]
slop_b <- summary(reg)$coeff[2,1]

estimate <- intercept_a + (slop_b * khacha)
final_pred <- estimate  + ytk_norm[qrtr]
round(final_pred, 4)


## 1.6
a <- 8
b <- 40

round((ytk[b] / ytk[a] - 1) * 100, 4)


## 2.1

b <- 40
r <- 8

total <- 48

round((prob_r <- r / total),4)
(prob_b <- b / total)

## 2.2
prob_correct <- 0.83

(saw_brown <- (prob_correct * prob_b) + ((1- prob_correct) * prob_r))

## 2.3
(indeed_b_saw_b <- ( prob_correct * prob_b ) / saw_brown)
round(indeed_b_saw_b, 4)
## 3.1
urn1_total <- 8
urn1_b <- 2
urn1_y <- 6
urn1_b_prob <- urn1_b / urn1_total
urn1_y_prob <- urn1_y / urn1_total

urn2_total <- 15
urn2_b <- 8
urn2_y <- 7
urn2_b_prob <- urn2_b / urn2_total
urn2_y_prob <- urn2_y / urn2_total

urn3_total <- 12
urn3_b <- 3
urn3_y <- 9
urn3_b_prob <- urn3_b / urn3_total
urn3_y_prob <- urn3_y / urn3_total

(draw_yellow <- urn1_y_prob * urn2_y_prob * urn3_y_prob)

## 3.2
prob2 <- (urn3_y_prob * urn2_y_prob * urn1_b_prob) /
          ((urn3_y_prob * urn2_y_prob * urn1_b_prob) + 
           (urn1_y_prob * urn3_y_prob * urn2_y_prob))
round(prob2, 4)

## 3.3
prob3 <- (urn1_y_prob * urn3_y_prob * urn2_y_prob) +
        (urn1_y_prob * urn3_y_prob * urn2_b_prob) + 
        (urn1_y_prob * urn3_b_prob * urn2_y_prob) +
        (urn1_b_prob * urn3_y_prob * urn2_y_prob)

round(prob3, 4)

## 3.4
prob_4 <- (urn1_y_prob * urn2_b_prob * urn3_y_prob) +
          (urn1_b_prob * urn2_y_prob * urn3_b_prob)

round(prob_4, 4)

##4.1
total <- 100000
m1 <- 18000
m2 <- 33000
m3 <- 49000

m1_prob  <- m1 / total
m2_prob <- m2 / total
m3_prob <- m3 / total

m1_red <- 18000 / 2
m1_white <- m1 / 2
m1_red_prob <- 0.5
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



(draw_red <- (m1_prob * m1_red_prob) + (m2_prob * m2_red_prob) + (m3_prob * m3_red_prob))

##4.2
(prob_1 <- (m3_prob * m3_green_prob) / ((m3_prob * m3_green_prob) + (m2_prob * m2_green_prob)))
round(prob_1, 4)

## 4.3

(total_red_yellow <- (m2_red + m3_red + m2_yellow + m3_yellow ))
(total_m2_m3 <- m2 + m3)

(prob_orng <- (total_red_yellow / total_m2_m3) * 0.02 * 100000)

#################################################
###############################
##################
#######
###
##





## you want to calculate the trend-cycle component (combination of trend and cycle) 
## of the time series. Calculate the value of the 
## trend-cycle component for the 3rd quarter 2001 by using a suitable moving average
ts_data <- ts(RData$BIP_SK, start = c(2000, 1),end = c(2013,4), frequency = 4)
head(ts_data)

ts_data
ts_data_trend <- decompose(ts_data, type = "additive")$trend
ts_comp_third <- ts_data_trend[7]
round(ts_comp_third, 4) ## 5.9752


###### trend cycle component of 4th quarter 2001

ts_data <- ts(RData$BIP_BE, start = c(2000, 1), end = c(2012, 4), frequency = 4)
head(ts_data)

ts_data_trend <- decompose(ts_data, type = "additive")$trend
ts_comp_fourth <- ts_data_trend[8]
round(ts_comp_fourth, 4)

## Now calculate the standardised seasonal component of the time series for the 1st quarter.
ts_data_trend

(ts <- decompose(ts_data, type = "additive"))
(ts_sesional <- ts$seasonal)
(first_quarter <- ts_sesional[1])
(mean_sesional <- mean(ts_sesional))
(sd_sesional <- sd(ts_sesional))
(stnd_fsrt_qrtr <- (first_quarter - mean_sesional) / sd_sesional) 
round(stnd_fsrt_qrtr, 4) ##   -1.6192  --- WRONG

###
(ts_norm<-decompose(ts_data,type="additive")$figure)
k<-1
ts_norm[1] ##  -0.6947359


### standardised seasonal component of the time series for the 3rd quarter

ts_norm <- decompose(ts_data, type = "additive")$figure
ts_norm
round(ts_norm[3], 4)


## In theory, which value would result for the sum of the standardised seasonal components of one year?

round(sum(ts_norm), digits = 8)


round(sum(ts_norm), digits = 8)

## Calculate the random error of the time series for the 2nd quarter 2009. 
## Assume that the value of the trend-cycle component for this quarter amounts to 15.7685.
ts

(zt <- ts_data - ts_norm)
t<-38
ytk.17<-15.7685
ytk.random.error<-zt[t]-ytk.17
ytk.random.error
round(ytk.random.error, 4)

### calculate the random error of the time series for the first quarter 2004. 
##Assume that the value of the trend cycle component for 
## thes quarter  amounts to 71.343

ts_error <- ts_data - ts_norm
ts_error

t <- 17
trend_cycle_component <- 71.343
random_error <- ts_error[t] - trend_cycle_component
round(random_error, 4)



##  Now forecast the value of the GDP in the 3rd quarter of 2013

#t<-1:length(zt)
#reg<-lm(zt~t)
#slope.a<-summary(reg)$coeff[2,1]
#intercept.b<-summary(reg)$coeff[1,1]
#t_est<-52+4
#k_est<-4
#z_55<-(slope.a*t_est)+intercept.b
#final.pred<-z_55+ytk_norm[k_est]
#final.pred

(t <- 1:length(zt))
(reg <- lm(zt ~ t))
(summary(reg))
(slop <- summary(reg)$coeff[2,1])
(intercept <- summary(reg)$coeff[2,1])
t <- 55
k <- 3
(pred <- (slop * t) + intercept)
(final_pred <- pred + ts_norm[k])
round(final_pred[55], 4)

### forecast for 2nd quarter 2013
t <- 1 : length(ts_error) 
#t <- 4 * 13
reg <- lm(ts_error ~ t)
summary(reg)

(intercept_a <- summary(reg)$coeff[1,1])
(slop_b <- summary(reg)$coeff[2,1])

second_qrtr_2013 <- 54
qrtr <- 3

pred <- intercept_a + (slop_b * 54 )

ts_norm[qrtr]
final_pred <- ts_norm[qrtr] + pred
final_pred
round(final_pred, 4)

## By how many percent did GDP change from Q4 2003 to Q3 2006? Please specify your result in percent!

#(ytk[36]/ytk[17]-1)*100

ts_data
## q4 2003-- 16
## q3 2006 -- 27
(change_in_gdp <- (ts_data[27] / ts_data[16] - 1)* 100)
round(change_in_gdp, 4)

### By how many percent did GDP change from Q3 2003 to Q4 2005? Please specify your result in percent!
qrtr_3rd_2003 <- 15
qrtr_4th_2005 <- 24
ts_data

change_in_gdp <- (ts_data[24] / ts_data[15] - 1) * 100
round(change_in_gdp,4)


## Please calculate the probability that the courier wore a brown uniform

#sol.brown<-42
#sol.red<-10
#sol.total<-sol.brown+sol.red
#prob.sol.brown<-sol.brown/sol.total
#prob.sol.red<-sol.red/sol.total

sol_brown <- 43
sol_red <- 7
(total <- sol_brown + sol_red)
(prob_brown <- sol_brown / total)
(prob_red <- sol_red / total)

#### Please calculate the probability that the courier wore a red uniform. 
(sol.brown <- 45)
(sol.red <- 4)
(total <- sol.brown + sol.red)
(prob_brown <- sol.brown / total)
(prob_red <- sol.red / total)

round(prob_red,4)

## Please calculate the probability that the witness saw a courier wearing a red uniform. 

#chances.corr<-85/100
#saw.brown<- (chances.corr*prob.sol.brown)+((1-chances.corr)*prob.sol.red)
#saw.red<-(chances.corr*prob.sol.red)+((1-chances.corr)*prob.sol.brown)

chance_witness_correct <- 85 / 100

(saw_red<-(chance_witness_correct*prob_red)+((1-chance_witness_correct)*prob_brown))
(saw_brown<- (chance_witness_correct*prob_brown)+((1-chance_witness_correct)*prob_red))


## Please calculate the probability that the witness saw a courier wearing a red uniform. 

(chance_witness_correct <- 80/100)

sow_red <- (chance_witness_correct * prob_red) + ((1 - chance_witness_correct) * prob_brown)
round(sow_red, 4)

## Please calculate the probability that the witness saw a courier wearing a brown uniform.

sow_brown <- (chance_witness_correct * prob_brown) + ((1 - chance_witness_correct) * prob_red)
round(sow_brown, 4)






##  Please calculate the probability that the courier was indeed wearing a brown uniform,
##  given that the witness saw a courier wearing a brown uniform.


ind.red.saw.brown<-((1-chances.corr)*prob.sol.red)/saw.brown   #indeed red given saw brown
(ind_brown_saw_brown<-(chance_witness_correct*prob_brown)/saw_brown)  #indeed brown given saw brown
ind.brown.saw.red<-((1-chances.corr)*prob.sol.brown)/saw.red   #indeed brown given saw red
ind.red.saw.red<-(chances.corr*prob.sol.red)/saw.red ## indeed red given saw red 

round(ind_brown_saw_brown, 4)

##  Please calculate the probability that the courier was indeed wearing a red uniform,
##  given that the witness saw a courier wearing a brown uniform.

#(0.85*(42/52)) / ((0.85*(42/52)) + (0.15*(10/52)))

(indeed_red_saw_brown <- (0.20 * (4/49)) / ((0.20 * (4/49)) + (0.80 * (45/49))))
(indeed_red_saw_brown_1 <- ((1 - chance_witness_correct) * prob_red) / sow_brown )
round(indeed_red_saw_brown,4)

#########
urn1.total<-8
urn2.total<-15
urn3.total<-12
urn1.black<-6
urn2.black<-12
urn3.black<-4
urn1.yellow<-2
urn2.yellow<-3
urn3.yellow<-8

urn1.black.prob<-urn1.black/urn1.total
urn2.black.prob<-urn2.black/urn2.total
urn3.black.prob<-urn3.black/urn3.total

urn1.yellow.prob<-urn1.yellow/urn1.total
urn2.yellow.prob<-urn2.yellow/urn2.total
urn3.yellow.prob<-urn3.yellow/urn3.total



## 
urn1_black <- 6
urn1_yellow <- 2
urn1_total <- urn1_black + urn1_yellow

urn2_black <- 6
urn2_yellow <- 9
urn2_total <- urn2_black + urn2_yellow

urn3_black <- 3
urn3_yellow <- 9
urn3_total <- urn3_black + urn3_yellow


urn1_black_prob <- urn1_black / urn1_total
urn2_black_prob <- urn2_black / urn2_total
urn3_black_prob <- urn3_black / urn3_total

urn1_yellow_prob <- urn1_yellow / urn1_total
urn2_yellow_prob <- urn2_yellow / urn2_total
urn3_yellow_prob <- urn3_yellow / urn3_total



## Please calculate the probability that every draw yields a black ball

(draw.black<-urn1.black.prob*urn2.black.prob*urn3.black.prob)

# Please calculate the probability that every draw yields a black ball.

(drow_black <- urn1_black_prob * urn2_black_prob * urn3_black_prob)



##  Assume that the balls taken from the first and second urn are both black.
## Please quantify the probability that the ball drawn from the remaining urn is not black.

prob.draw<-(urn1.black.prob*urn2.black.prob*urn3.yellow.prob)/
                  ((urn1.black.prob*urn2.black.prob*urn3.yellow.prob)+(urn1.black.prob*urn2.black.prob*urn3.black.prob))
prob.draw 
round(prob.draw, 4)

## Assume that the balls taken from the second and first urn are both yellow
## Please quantify the probability that the ball drawn from the remaining urn is not yellow

prob_drow <- (urn1_yellow_prob * urn2_yellow_prob * urn3_black_prob) / 
                    ((urn1_yellow_prob * urn2_yellow_prob * urn3_black_prob) + (urn1_yellow_prob * urn2_yellow_prob * urn3_yellow_prob))
round(prob_brown,4)







##  Please calculate the probability that not more than one yellow ball is drawn from the three urns.

prob.draw2<-(urn1.black.prob*urn2.black.prob*urn3.yellow.prob)+(urn1.yellow.prob*urn2.black.prob*urn3.black.prob)+(urn1.black.prob*urn2.yellow.prob*urn3.black.prob)+(urn1.black.prob*urn2.black.prob*urn3.black.prob)
prob.draw2
round(prob.draw2, 4)

## Please calculate the probability that not more than one black ball is drawn from the three urns

prob_drow_2 <- (urn1_black_prob * urn2_yellow_prob * urn3_yellow_prob) +
              (urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob) +
              (urn1_yellow_prob * urn2_yellow_prob * urn3_black_prob) +
              (urn1_yellow_prob * urn2_yellow_prob * urn3_yellow_prob)

round(prob_drow_2,4)
##   How many black balls would you expect in one round of drawing on average?

prob.drow3 <- urn1.black.prob * urn2.black.prob * urn3.black.prob
round(prob.drow3, 4)


##  Please calculate the probability that you draw both colours in alternating order (starting with black or yellow)

prob_drow_3 <- (urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob) +
              (urn1_black_prob * urn2_yellow_prob * urn3_black_prob)

round(prob_drow_3, 4)


##

total_prod<-100000
p1<-15000
p2<-35000
p3<-50000
p1.prob<-p1/total_prod
p2.prob<-p2/total_prod
p3.prob<-p3/total_prod

p1.y<-0.5
p1.w<-0.5

p2.r<-0.25
p2.y<-0.25
p2.g<-0.25
p2.w<-0.25

p3.r<-0.25
p3.y<-0.25
p3.g<-0.25
p3.w<-0.25


#### 
daily_prd <- 100000

plant1_prd <- 15000
plant2_prd <- 33000
plant3_prd <- 52000

plant1_prob <- plant1_prd / daily_prd
plant2_prob <- plant2_prd / daily_prd
plant3_prob <- plant3_prd / daily_prd

plant1_yellow <- 0.5
plant1_white <- 0.5

plant2_green <- 0.25
plant2_red <- 0.25
plant2_yellow <- 0.25
plant2_white <- 0.25

plant3_green <- 0.25
plant3_red <- 0.25
plant3_yellow <- 0.25
plant3_white <- 0.25



### You draw a jelly baby randomly from the daily production. 
# Please calculate the probability that this jelly baby is yellow
prob.y.draw<-(p1.prob*p1.y)+(p2.prob*p2.y)+(p3.prob*p3.y)
prob.y.draw

## you draw a jelly baby randomly from the daily production. 
## Please calculate the probability that this jelly baby is yellow.


prob_yellow_drow <- (plant1_prob * plant1_yellow) + 
                    (plant2_prob * plant2_yellow) + 
                    (plant3_prob * plant3_yellow)

round(prob_yellow_drow, 4)

## Assume that the randomly drawn jelly baby is a green one. 
## Please calculate the probability that this jelly baby was not produced on plant 2.
prob.draw.g<-(p3.prob*p3.g)/((p3.prob*p3.g)+(p2.prob*p2.g))
prob.draw.g
round(prob.draw.g, 4)

## Assume that the randomly drawn jelly baby is a green one. 
## Please calculate the probability that this jelly baby was not produced on plant 3

prod_drow <- (plant2_prob *plant2_green) / ( (plant2_prob *plant2_green)+(plant3_prob *plant3_green))

round(prob_drow, 4)



## Because of a defect in the control system, sometimes the colours red and yellow mix up.
## This error concerns each plant to the same extent. In 5% of cases, an orange jelly baby
## is produced instead of a red or a yellow one. How many orange jellly babies would you expect
##  to be produced during one daily production run?

r.y.total<-15000+26000

prob.orange<- (r.y.total/(p2+p3))*0.05*100000
prob.orange

## Because of a defect in the control system, sometimes the colours red and yellow mix up.
## This error concerns each plant to the same extent. In 3% of cases, an orange jelly baby 
## is produced instead of a red or a yellow one. How many orange jellly babies would you expect
## to be produced during one daily production run?

(red_yellow_in_plant2 <- 33000 * 0.50) ## (red -> .25 and yellow -> .25)

(red_yellow_in_plant3 <- 52000 * 0.50) ## (red -> .25 and yellow -> .25)
(total_red_yellow <- red_yellow_in_plant2 + red_yellow_in_plant3)
(total_in_plant2_plant3 <- plant2_prd + plant3_prd)
total_orange <- (total_red_yellow / total_in_plant2_plant3) * 0.03 * 100000
round(total_orange, 4)

###############################################################################
################################### 2 #########################################
###############################################################################


#  You want to determine the trend-cycle component (combination of trend and cycle)
# of the time series. Calculate the value of the trend-cycle component for the 4th quarter
# 2006 by using a suitable moving average

RData


#ts_data <- ts(RData$BIP_SK, start = c(2000, 1),end = c(2013,4), frequency = 4)
#head(ts_data)

#ts_data
#ts_data_trend <- decompose(ts_data, type = "additive")$trend
#ts_comp_third <- ts_data_trend[7]
#round(ts_comp_third, 4)

(ts_object <- ts(RData$BIP_AT, start = c(2000, 1), end = c(2013, 4), frequency = 4))

(ts_object_trend <- decompose(ts_object, type = "additive")$trend)
round(ts_object_trend[28], 4) ## 66.2517


## Now calculate the standardised seasonal component of the time series for the 2nd quarter. 

#(ts_norm<-decompose(ts_data,type="additive")$figure)
#k<-1
#ts_norm[1]
#ts_object

(ts_standrd <- decompose(ts_object, type = "additive")$figure)
round(ts_standrd[2], 4)


## In theory, which value would result for the sum of the standardised seasonal components of one year? 
round(sum(ts_standrd))

## Calculate the random error of the time series for the 3rd quarter 2010.
## Assume that the value of the trend-cycle component for this quarter amounts to 71.9117

#(zt <- ts_data - ts_norm)
#t<-38
#ytk.17<-15.7685
#ytk.random.error<-zt[t]-ytk.17
#ytk.random.error
#round(ytk.random.error, 4)

ts_object
t <- 43
tc_comp <- 71.9117

(residual <- ts_object - ts_standrd)
(random_error <- residual[t] - tc_comp)
round(random_error, 4)


## Now forecast the value of the GDP in the 2nd quarter of 2013


#(t <- 1:length(zt))
#(reg <- lm(zt ~ t))
#(summary(reg))
#(slop <- summary(reg)$coeff[2,1])
##(intercept <- summary(reg)$coeff[2,1])
#t <- 55
#k <- 3
#(pred <- (slop * t) + intercept)
#(final_pred <- pred + ts_norm[k])
#round(final_pred[55], 4)

(t <- 1:length(residual))
(reg <- lm(residual ~ t))
(summary(reg))
(slop <- summary(reg)$coeff[2,1])
(intercept <- summary(reg)$coeff[1,1])
t <- 54
qrtr <- 2
(pred <- (slop * t) + intercept)
(final_pred <- pred + ts_standrd[qrtr])
round(final_pred, 4)

## By how many percent did GDP change from Q1 2003 to Q3 2007? Please specify your result in percent!


#ts_data
## q4 2003-- 16
## q3 2006 -- 27
#(change_in_gdp <- (ts_data[27] / ts_data[16] - 1)* 100)
#round(change_in_gdp, 4)

ts_object
## q3 2007 31
## q1 2003 13
(change_in_gdp <- (ts_object[31] / ts_object[13] - 1) * 100)
round(change_in_gdp,4)


## Please calculate the probability that the courier wore a red uniform.
brown <- 44
red <- 4

(prob_red <- 4 / 48)
prob_brown <- 44 / 48

round(prob_red, 4)



# Please calculate the probability that the witness saw a courier wearing a brown uniform.


correct <- 84 / 100

(saw_brown <- (correct * prob_brown) + ((1-correct) * prob_red))

(saw_red <- (correct * prob_red) + ((1-correct) * prob_brown))

round(saw_brown, 4)


# Please calculate the probability that the courier was indeed wearing a red uniform,
# given that the witness saw a courier wearing a red uniform.

(indeed_red_saw_red <- (correct * prob_red) / saw_red ) 
(indeed_red_saw_brown <- ((1-correct) * prob_red) / saw_brown ) 
(indeed_brown_saw_red <- ((1-correct) * prob_brown) / saw_red )
(indeed_brown_saw_brown <- (correct * prob_brown) / saw_brown ) 
round(indeed_red_saw_red, 4)



##  Please calculate the probability that every draw yields a yellow ball.

urn1_total <- 8
urn2_total <- 15
urn3_total <- 12

urn1_yellow <- 1
urn1_black <- 7

urn2_yellow <- 10
urn2_black <- 5

urn3_yellow <- 9
urn3_black <- 3

urn1_black_prob <- urn1_black / urn1_total 
urn1_yellow_prob <- urn1_yellow / urn1_total 
  
urn2_black_prob <- urn2_black / urn2_total
urn2_yellow_prob <- urn2_yellow / urn2_total

urn3_black_prob <- urn3_black / urn3_total 
urn3_yellow_prob <- urn3_yellow / urn3_total

(prob_yellow <- urn1_yellow_prob  * urn2_yellow_prob * urn3_yellow_prob)
round(prob_yellow, 4)

##  Assume that the balls taken from the second and first urn are both yellow. 
## Please quantify the probability that the ball drawn from the remaining urn is not yellow.

(prob_b_b_y <- (urn1_black_prob * urn2_black_prob * urn3_yellow_prob) / ((urn1_black_prob * urn2_black_prob * urn3_yellow_prob)+(urn1_black_prob * urn2_black_prob * urn3_black_prob)))
round(prob_b_b_y, 4)


## Please calculate the probability that not more than one yellow ball is drawn from the three urns.

(prob_c <- (urn1_black_prob * urn2_black_prob * urn3_black_prob)+
          (urn1_black_prob * urn2_black_prob * urn3_yellow_prob)+
          (urn1_black_prob * urn2_yellow_prob * urn3_black_prob)+
          (urn1_yellow_prob * urn2_black_prob * urn3_black_prob))
round(prob_c, 4)


##  Please calculate the probability that you draw both colours in alternating order
## (starting with black or yellow). 

(alternating_order <- ( urn1_black_prob * urn2_yellow_prob * urn3_black_prob ) +
                      (urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob))
round(alternating_order, 4)


################################################# 

dailly_prod <- 100000

plant_1 <- 16000
plant_2 <- 32000
plant_3 <- 52000

(prob_plant_1 <- plant_1 / dailly_prod)
(prob_plant_2 <- plant_2 / dailly_prod)
(prob_plant_3 <- plant_3 / dailly_prod)

plant_1_red <- 8000
plant_1_green <- 8000

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

plant_3_red <- 13000
plant_3_green <- 13000
plant_3_yellow <- 13000
plant_3_white <- 13000

(prob_3_red <- plant_3_red / plant_3)
(prob_3_green <- plant_3_green / plant_3) 
(prob_3_white <- plant_3_white / plant_3)
(prob_3_yellow <- plant_3_yellow / plant_3) 

## You draw a jelly baby randomly from the daily production. 
## Please calculate the probability that this jelly baby is red.
(drow_red <- (prob_plant_1 * prob_1_red) + (prob_plant_2 * prob_2_red) + (prob_plant_3 * prob_3_red))



## Assume that the randomly drawn jelly baby is a white one. 
## Please calculate the probability that this jelly baby was not produced on plant 3.

(drow_white <- (prob_plant_2 * prob_2_white) / ((prob_plant_2 * prob_2_white) + (prob_plant_3 * prob_3_white)))
round(drow_white, 4)

## Because of a defect in the control system, sometimes the colours red and yellow mix up.
## This error concerns each plant to the same extent. In 2% of cases, an orange jelly baby
## is produced instead of a red or a yellow one. How many orange jellly 
## babies would you expect to be produced during one daily production run?

##r.y.total<-15000+26000

##prob.orange<- (r.y.total/(plant_2 + plant_3))*0.05*100000
##prob.orange

(total_red_yellow_prod <- plant_2_red + plant_2_yellow + plant_3_red + plant_3_yellow)
(plant_2 + plant_3)
(prob_red_yellow_in_2_machine <- total_red_yellow_prod / (plant_2 + plant_3))
orange_prod <- 0.02

(dailly_orange_prod <- prob_red_yellow_in_2_machine * orange_prod * dailly_prod )



















