#7.1

load("Pillar_08_QP_01_00_20160115043bb.RData")

#You assume the average turnover in the USA to be exactly 717.92. 
#You checked your assumption using a hypothesis test. Its results are depicted 
#in the R output above. Please state a point estimate for the average turnover in the USA. 

attach(RData)
test<-t.test(turnover.USA,alternative="two.sided",mu=717.92,conf.level = 0.90)
test

#b) To which of the following statements - regarding the test result shown in the R 
#    output - do you agree?
# The R output displays a p-value of 0.0106. Therefore, the null hypothesis cannot be
# rejected at the given significance levels.
alpha<-0.01
p.value<-0.0106
p.value < alpha              #False Ho can not be rejected 


#The turnover figures of randomly drawn shops in Germany and in the USA are documented 
#in the attached R file. Based on this information, please calculate an estimate for 
#the variance of the estimated average turnover in Germany. Use sampling without replacement.
N.germany<-270
N.USA<-226
mean.germany<-mean(turnover.Germany)
mean.USA<-mean(turnover.USA)
n.germany<-length(turnover.Germany)
n.USA<-length(turnover.USA)

varX.germany<-(1/(n.germany-1))*sum((turnover.Germany-mean.germany)^2)
varX.USA<-(1/(n.USA-1))*sum((turnover.USA-mean.USA)^2)

var.mean.germany<-(1/n.germany)*varX.germany*((N.germany-n.germany)/(N.germany-1))
var.mean.USA<-(1/n.USA)*varX.USA*((N.USA-n.USA)/(N.USA-1))



#You assume that the average turnover in the USA is 1595.1 less than the average 
#turnover in Germany. Check your assumption using a hypothesis 
#test and a significance level of ??= 0.1. Please state the resulting absolute test value. 

test2<-t.test(turnover.USA,turnover.Germany,conf.level = 0.90,mu= -1595.1 , alternatives="less")
ans<-abs(test2$statistic)
ans

#Referring to your test result in exercise part d): 

#please calculate the estimate for the variance of the difference-in-means estimator.

abs.test<-test2$statistic
ans.e<-((mean.germany-mean.USA-1595.1)/abs.test)^2
ans.e

(test2$stderr)^2




#7.2

load("Pillar_08_QP_03_00_20160115024.RData")
attach(RData)
#Please calculate an estimate for the variance of the estimatior for the average 
#weight of gummy bears in the sample. In order to do so, use sampling without replacement.

N<-1332
n<-90
mean.weight<-mean(weight)
varX.weight<-(1/(n-1))*sum((weight-mean.weight)^2)
var.mean.weight<-(1/n)*varX.weight*((N-n)/(N-1))


#If the variance of the weight of the gummy bears produced is not 
# higher than 161.617339 it follows that the plant produces with a 
# sound level of accuracy. In order to test the precision of the machine, a 
# hypothesis test is conducted (see R output). Please calculate the missing value in the R output. 

test_stat<-(n-1)/ 214.430937*var(weight)
test_stat

## another 
p value  -from pic given 
qchisq(p,df=n-1)

#Please calculate the critical value at which the hypothesis stated in the R output is rejected
alpha<-0.05
crit.value<-qchisq(p = alpha , df = (n - 1))
crit.value


#he plant has to produce equal shares of gummy bears of each colour. 
#Use the information given in the sample to test the hypothesis (??= 0.1) 
#the same amount of gummy bears in each colour. Please state the resulting test statistic.

data<-table(color)
n<-length(color)
theta<-rep(1/4,4)
theta

test_stat<-chisq.test(data,p=theta*n,rescale.p = TRUE,correct = FALSE)$statistic
test_stat

#Referring to exercise part d): To which of the following statements do you agree? In case the null hypothesis is rejected 
#at several significance levels, always select the smallest one at which the null is still reject


crit_reg <- qchisq(p = alpha, df = 3)
crit_reg

test_stat > crit_reg
