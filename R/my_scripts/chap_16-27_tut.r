rm(list = ls())

#3.1
load("Pillar_04_QP_02_00_20151129004qq.RData")
#You want to determine the trend-cycle component (combination of trend and cycle) of the 
#time series. Calculate 
#the value of the trend-cycle component for the 3rd quarter 2002 by using a suitable moving average.

ytk<-ts(data=RData$BIP_BE,start=c(2000,1),frequency = 4)
ytk
ytk_trend<-decompose(ytk,type="additive")$trend
ytk_trend
t<-11
ytk_trend[11]


###
RData
## You want to determine the trend-cycle component (combination of trend and cycle) of the time series.
## Calculate the value of the trend-cycle component for the 4th quarter 2006 by using a suitable moving average.

(y_t_k <- ts(data = RData$BIP_AT, start = c(2000,1), frequency = 4))
(y_t_k_trend <- decompose(y_t_k, type = "additive")$trend)
round(y_t_k_trend[28], 4)

#calculate the standardised seasonal component of the time series for the 2nd quarter

ytk_norm<-decompose(ytk,type="additive")$figure
k<-2
ytk_norm[2]

## Now calculate the standardised seasonal component of the time series for the 2nd quarter

(y_t_k_norm <- decompose(y_t_k, type="additive")$seasonal)
round(y_t_k_norm[2], 4)

ytk_norm<-decompose(y_t_k,type="additive")$figure
k<-2
ytk_norm[2]

#which value would result for the sum of the standardised seasonal components of one year?

# Answer is 0
0 == round(sum(ytk_norm), digits = 8)

## In theory, which value would result for the sum of the standardised seasonal components of one year?
sum(ytk_norm)
(-2.0047419 -0.2169784 +1.3892320 + 0.8324883)

#Calculate the random error of the time series for the 1st quarter 
#2004. Assume that the value of the trend-cycle component for this quarter amounts to 67.388

zt<-ytk-ytk_norm
t<-17
ytk.17<- 67.388
ytk.random.error<-zt[t]-ytk.17
ytk.random.error

##  Calculate the random error of the time series for the 3rd quarter 2010.
## Assume that the value of the trend-cycle component for this quarter amounts to 71.9117

t <- 43
(zt <- y_t_k - y_t_k_norm)
given_y_t_k_trend <- 71.9117 ##  estimate trend

(y_t_k_from_table <- zt[43])
#(y_t_k_trend_from_table <- y_t_k_trend[43])
(y_t_k_error <- y_t_k_from_table - given_y_t_k_trend)
round(y_t_k_error, 4)

# Now forecast the value of the GDP in the 4th quarter of 2013.


## 2nd quarter 2013

t<-1:length(zt)
reg<-lm(zt~t)
slope.a<-summary(reg)$coeff[2,1]
intercept.b<-summary(reg)$coeff[1,1]
t_est<-52+2
quarter_est<-2
z_55<-(slope.a*t_est)+intercept.b
final.pred<-z_55+y_t_k_norm[k_est]
round(final.pred, 4)

## Now forecast the value of the GDP in the 2nd quarter of 2013

t <- 

## By how many percent did GDP change from Q1 2003 to Q3 2007? Please specify your result in percent! 
  
a <- 13
b <- 31
round(((y_t_k[31]/ y_t_k[13] - 1)*100),4)



#By how many percent did GDP change from Q1 2004 to Q4 2008?
(ytk[36]/ytk[17]-1)*100

#3.2

# Please calculate the probability that the courier wore a brown uniform.

sol.brown<-42
sol.red<-10
sol.total<-sol.brown+sol.red
prob.sol.brown<-sol.brown/sol.total
prob.sol.red<-sol.red/sol.total

# Please calculate the probability that the witness saw a courier wearing a brown uniform.
chances.corr<-85/100
saw.brown<- (chances.corr*prob.sol.brown)+((1-chances.corr)*prob.sol.red)
saw.red<-(chances.corr*prob.sol.red)+((1-chances.corr)*prob.sol.brown)




#######
total <- 43 + 4
brown <- 43
red <- 4
chance_correct <- 83 / 100

#prob_brown <- brown / total
#prob_red <- red / total

##  Please calculate the probability that the courier wore a red uniform.
round((prob_red <- 4 / total), 4)

# Please calculate the probability that the courier wore a brown uniform. 
(prob_brown <- 43/total)


## Please calculate the probability that the witness saw a courier wearing a brown uniform
(saw_brown <- (chance_correct * prob_brown) + ((1-chance_correct) * prob_red))
round(saw_brown, 4)

## Please calculate the probability that the witness saw a courier wearing a red uniform
(saw_red <- (chance_correct * prob_red) + ((1-chance_correct) * prob_brown))
round(saw_red, 4)


## Please calculate the probability that the courier was indeed wearing a brown uniform,
## given that the witness saw a courier wearing a brown uniform.
(brown_given_brown <- (chance_correct * prob_brown) / saw_brown)
round(brown_given_brown, 4)

#Please calculate the probability that the courier was indeed wearing a 
#red uniform, given that the witness saw a courier wearing a brown uniform.

ind.red.saw.brown<-((1-chances.corr)*prob.sol.red)/saw.brown   #indeed red given saw brown
ind.brown.saw.brown<-(chances.corr*prob.sol.brown)/saw.brown   #indeed brown given saw brown
ind.brown.saw.red<-((1-chances.corr)*prob.sol.brown)/saw.red   #indeed brown given saw red
ind.red.saw.red<-(chances.corr*prob.sol.red)/saw.red           #indeed red given saw red


#3.3

urn1.total<-8
urn2.total<-15
urn3.total<-12
urn1.black<-2
urn2.black<-6
urn3.black<-4
urn1.yellow<-6
urn2.yellow<-9
urn3.yellow<-8

urn1.black.prob<-urn1.black/urn1.total
urn2.black.prob<-urn2.black/urn2.total
urn3.black.prob<-urn3.black/urn3.total

urn1.yellow.prob<-urn1.yellow/urn1.total
urn2.yellow.prob<-urn2.yellow/urn2.total
urn3.yellow.prob<-urn3.yellow/urn3.total


#Please calculate the probability that every draw yields a black ball

draw.black<-urn1.black.prob*urn2.black.prob*urn3.black.prob

#Assume that the balls taken from the first and third urn are both black. Please 
#quantify the probability that the ball drawn from the remaining urn is not black

prob.draw<-(urn1.black.prob*urn3.black.prob*urn2.yellow.prob)/((urn1.black.prob*urn3.black.prob*urn2.yellow.prob)+(urn1.black.prob*urn3.black.prob*urn2.black.prob))
prob.draw             

#Please calculate the probability that at least two black balls are drawn from the three urns.

prob.draw2<-(urn1.black.prob*urn2.black.prob*urn3.yellow.prob)+(urn1.yellow.prob*urn2.black.prob*urn3.black.prob)+(urn1.black.prob*urn2.yellow.prob*urn3.black.prob)+(urn1.black.prob*urn2.black.prob*urn3.black.prob)
prob.draw2


# Please calculate the probability that you draw both colours in alternating order (starting with black or yellow). 

prob.draw3<-(urn1.black.prob*urn2.yellow.prob*urn3.black.prob)+(urn1.yellow.prob*urn2.black.prob*urn3.yellow.prob)
prob.draw3



#####
urn1_total <- 8
urn1_black <- 3
urn1_yellow <- 5

urn1_black_prob <- urn1_black / urn1_total
urn1_yellow_prob <- urn1_yellow / urn1_total

  
urn2_total <- 15
urn2_black <- 12
urn2_yellow <- 3

urn2_black_prob <- urn2_black / urn2_total
urn2_yellow_prob <- urn2_yellow / urn2_total
  
urn3_total <- 12
urn3_black <- 4
urn3_yellow <-8

urn3_black_prob <- urn3_black / urn3_total
urn3_yellow_prob <- urn3_yellow / urn3_total


## Please calculate the probability that every draw yields a black ball
(prob_drow_black <- urn1_black_prob * urn2_black_prob * urn3_black_prob)
round(prob_drow_black, 3)

## Assume that the balls taken from the first and third urn are both black.
## Please quantify the probability that the ball drawn from the remaining urn is not black
prob_2 <- (urn1_black_prob * urn2_yellow_prob * urn3_black_prob ) /
          ((urn1_black_prob * urn2_yellow_prob * urn3_black_prob ) + (urn1_black_prob * urn2_black_prob * urn3_black_prob ))
round(prob_2, 4)

## Please calculate the probability that at least two black balls are drawn from the three urns

prob_3 <- (urn1_black_prob * urn2_black_prob * urn3_black_prob) + 
          (urn1_black_prob * urn2_yellow_prob * urn3_black_prob) +
          (urn1_black_prob * urn2_black_prob * urn3_yellow_prob) +
          (urn1_yellow_prob * urn2_black_prob * urn3_black_prob)
round(prob_3, 4)

## Please calculate the probability that you draw both colours in alternating order (starting with black or yellow). 
prob_4 <- (urn1_black_prob * urn2_yellow_prob * urn3_black_prob) + (urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob) 
round(prob_4, 4)


###### ############################## e_tut - 4 ###############################
urn1_total <- 8
urn1_black <- 6
urn1_yellow <- 2

urn1_black_prob <- urn1_black / urn1_total
urn1_yellow_prob <- urn1_yellow / urn1_total


urn2_total <- 15
urn2_black <- 7
urn2_yellow <- 8

urn2_black_prob <- urn2_black / urn2_total
urn2_yellow_prob <- urn2_yellow / urn2_total

urn3_total <- 12
urn3_black <- 4
urn3_yellow <-8

urn3_black_prob <- urn3_black / urn3_total
urn3_yellow_prob <- urn3_yellow / urn3_total

## Please calculate the probability that every draw yields a black ball

(prob_1 <- urn1_black_prob * urn2_black_prob * urn3_black_prob)
round(prob_1, 4)


## Assume that the balls taken from the third and first urn are both yellow.
## Please quantify the probability that the ball drawn from the remaining urn is not yellow

prob_2 <- ( urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob ) /
          ((urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob) + (urn1_yellow_prob * urn2_yellow_prob * urn3_yellow_prob))

round(prob_2, 4)


## Please calculate the probability that not more than one black ball is drawn from the three urns
prob_3 <- (urn1_yellow_prob * urn2_yellow_prob * urn3_yellow_prob) + 
          (urn1_black_prob * urn2_yellow_prob * urn3_yellow_prob) +
          (urn1_yellow_prob * urn2_black_prob * urn3_yellow_prob) +
          (urn1_yellow_prob * urn2_yellow_prob * urn3_black_prob)

round(prob_3, 4)

## How many yellow balls would you expect in one round of drawing on average?
prob_4 <- urn1_black_prob * urn2_black_prob * urn3_black_prob
round(prob_4, 4)


#3.4

total_prod<-100000
p1<-18000
p2<-30000
p3<-52000
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

#You draw a jelly baby randomly
#from the daily production. Please calculate the probability that this jelly baby is yellow.

prob.y.draw<-(p1.prob*p1.y)+(p2.prob*p2.y)+(p3.prob*p3.y)
prob.y.draw


#Assume that the randomly drawn jelly baby is a green one. Please calculate the 
#probability that this jelly baby was not produced on plant 2

prob.draw.g<-(p3.prob*p3.g)/((p3.prob*p3.g)+(p2.prob*p2.g))
prob.draw.g


#Because of a defect in the control system, sometimes the colours red and yellow mix up. 
#This error concerns each plant to the same extent. In 4% of cases, an orange 
# jelly baby is produced instead of a red or a yellow one.
#How many orange jellly babies would you expect to be produced during one daily production run?

r.y.total<-15000+26000

prob.orange<- (r.y.total/(p2+p3))*0.04*100000
prob.orange

#####

total_prod <- 100000
p1_total <- 20000
p2_total <- 34000
p3_total <- 46000

p1_prob <- p1_total / total_prod
p2_prob <- p2_total / total_prod
p3_prob <- p3_total / total_prod


p1_white <- 0.5
p1_yellow <- 0.5

p2_white <-.25
p2_yellow <- .25
p2_red <- .25
p2_green <- .25

p3_white <-.25
p3_yellow <- .25
p3_red <- .25
p3_green <- .25



## You draw a jelly baby randomly from the daily production. 
## Please calculate the probability that this jelly baby is yellow
prob_1 <- (p1_prob * p1_yellow)  + (p2_prob * p2_yellow) + (p3_prob * p3_yellow)
round(prob_1, 4)

## Assume that the randomly drawn jelly baby is a green one.
## Please calculate the probability that this jelly baby was not produced on plant 3.

prob_2 <- (p2_prob*p2_green) / ((p2_prob*p2_green)+(p3_prob*p3_green))
round(prob_2, 4)

## Because of a defect in the control system, sometimes the colours
## red and yellow mix up. This error concerns each
## plant to the same extent. In 4% of cases, an orange jelly
##baby is produced instead of a red or a yellow one. How many orange
## jellly babies would you expect to be produced during one daily production run?

## r.y.total<-15000+26000
## prob.orange<- (r.y.total/(p2+p3))*0.04*100000
## prob.orange


(red_yellow_total <- (p3_total / 2) + (p2_total / 2))
(p2_p3_total <- p3_total + p2_total)
(total_orange <- (red_yellow_total / p2_p3_total ) * 0.04 * 100000)

#####
total_prod <- 100000
p1_total <- 20000
p2_total <- 31000
p3_total <- 49000

p1_prob <- p1_total / total_prod
p2_prob <- p2_total / total_prod
p3_prob <- p3_total / total_prod


p1_white <- 0.5
p1_red <- 0.5

p2_white <-.25
p2_yellow <- .25
p2_red <- .25
p2_green <- .25

p3_white <-.25
p3_yellow <- .25
p3_red <- .25
p3_green <- .25

## You draw a jelly baby randomly from the daily production. Please calculate the probability that this jelly baby is red

prob_1 <- (p1_prob * p1_red ) + (p2_prob * p2_red ) + (p3_prob * p3_red )
round(prob_1, 4)


##  Assume that the randomly drawn jelly baby is a green one.
## Please calculate the probability that this jelly baby was not produced on plant 3.

prob_2 <- (p2_prob * p2_green ) / ((p2_prob * p2_green) + (p3_prob * p3_green))
round(prob_2, 4)

## Because of a defect in the control system, sometimes the colours red and yellow mix up.
## This error concerns each plant to the same extent. In 4% of cases, an orange jelly baby
## is produced instead of a red or a yellow one. How many orange jellly babies would you expect
## to be produced during one daily production run?

(red_yellow_total <- ((p2_total / 2) + (p3_total / 2)))
(p2_p3_total <- p2_total + p3_total)
(total_orange <- (red_yellow_total / p2_p3_total) * 0.04 * 100000)

#4.3
dice1<-expand.grid(c(1,2,3,4,5,6))

prop.table(table(dice1))

#Klaus rolls a single dice once. Please calculate the probability that the number 6 results

prop.table(table(dice1))


# Now he adds a second dice to the cup and wants to roll both dice only once.
# He wonders what the probability of 
# at least one dice showing the number 2 is. Please calculate the respective probability.

dice2<-expand.grid(lapply(X=1:2, FUN=function(x){c(0,1,0,0,0,0)}))
dice2
(x<-prop.table(table(rowSums(dice2))))
x[2]+x[3]


#Please calculate the probability that Klaus rolls in the 
#first roll a smaller or the same number than/as in the second, utilising only one dice.

res.1 <- (1/6)       
res.2 <- (1/6)*(5/6) 
res.3 <- (1/6)*(4/6) 
res.4 <- (1/6)*(3/6) 
res.5 <- (1/6)*(2/6)
res.6 <- (1/6)*(1/6) 
res.c <- round(res.1+res.2+res.3+res.4+res.5+res.6, 4)
res.c
1-res.c
#Finally, Klaus throws all 6 dice into the cup. He rolls the whole cup with all dices 4 times. 
#On average, how many 
#times would you expect the number 6 to result? Please calculate the respective expected value.
5/12
10/36
no_of_dices<-6
no_times<-4
prob.num<-1/6
exp.value<-no_of_dices*no_times*prob.num
exp.value

#4.4


x<-c(0,1,6)
y<-c(1,6,10)

data<-matrix(c(0.23,0.07,0.03,0.13,0.18,0.03,0.03,0.02,0.28),nrow=3,byrow=TRUE)
data
rownames(data)<-c("Y=1","Y=6","Y=10")


colnames(data)<-c("X=0","X=1","X=6")
data

f_y<-rowSums(data)
f_y
f_x<-colSums(data)
f_x




#Please calculate the expected value of the random variable X

Ex_y<-weighted.mean(x=y,w=f_y)
Ex_y
Ex_x<-weighted.mean(x=x,w=f_x)
Ex_x
#Please calculate the variance of the random variable Y

var_x<-sum(f_x*(x-Ex_x)^2)
var_x
var_y<-sum(f_y*(y-Ex_y)^2)
var_y
#You know that the random variable X takes on the value 0. Please calculate the conditional 
#expected value E(Y|X=0) based on this additional knowledge.

fx0<-0.39
y1<-1*data[1,1]/fx0
y6<-6*data[2,1]/fx0
y10<-10*data[3,1]/fx0
E.Y.X.0<-y1+y6+y10
E.Y.X.0
#Now consider the transformed random variable Z=10???8Y. Please calculate the expected 
#value E(Z) of this variable. You know that an expected value of E(Y)= 5.67 results 
#for the random variable Y and that the random variable X has got a variance of Var(X)= 7.17.
Y<-5.67
Z<-10-8*Y
Z


#4.5

#Please calculate the probability that the player rolls zero times the number 1 in the first roll.

prob1<-(5/6)*(5/6)*(5/6)
prob1

#Please calculate the probability that the number 1 results once in each of the three rolls.

prob2<-3*((1/6)*(5/6)*(5/6)) + 2*((1/6)*(5/6)) + (1/6)
prob2

#Please calculate the probability that the goal of the game - 
#rolling three times the number 1 - is reached within no more than two rolls.
e1<-1/6*1/6*1/6
e2<-(5/6*1/6*1/6*1/6)*3
e3<-(5/6*5/6*1/6*1/6*1/6)*3
e4<-5/6*5/6*5/6*1/6*1/6*1/6
ans<-e1+e2+e3+e4
ans


 