#5.2


#Please calculate the expected value of the random variable Y3

k2<-5                        #f-distribution
exp.y3<-k2/(k2-2)
exp.y3

# Please calculate the variance of the random variable Y1

k<-8                #std normal distribution

var.y1<-k
var.y1

#Please state the number of degrees of freedom of the random variable Y2

k<-5 # chisquare distribution no of terms =k
degree<-k

#Please state the 10%-quantile of the random variable Y4.

k<-5
qt(p=0.10,df=k)            #quantile for t distribution

qchisq(p=.90,df=5)         #quantile for chi distribution
#Please state the 1%-quantile of the random variable Y1

qnorm(p=0.01,mean=0,sd=sqrt(8))     #quantile for normal distribution

qnorm(p=0.05,mu,sqrt(var))




#6.1

N<-1200
n<-78

# Please use an unbiased estimator to reach an estimate for the population total

sample.mean<-25607/n                  
pop.tot.mean<-(N/n)*25607

#Please calculate a variance estimator for the sample mean.

var.X<-(1/(n-1))*1728328.22
var.sample.mean<-(1/n)*var.X*((N-n)/(N-1))
var.pop.tot.mean<-(N^2/n)*var.X*((N-n)/(N-1))

#Please calculate the value of the upper bound of a two-sided 95 percent 
#confidence interval for the sample mean.

alpha<-0.95
C.upper.sm<-sample.mean + qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.sample.mean)
C.lower.sm<-sample.mean - qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.sample.mean)

C.upper.pop<-pop.tot.mean + qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.pop.tot.mean)
C.lower.pop<-pop.tot.mean - qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.pop.tot.mean)


#From another sample you know that x¯¯¯= 301.97 and s2= 174.41. 
#Please determine the minimal sample size needed to ensure a two-sided 90 percent 
#confidence interval which is not longer than 6. Please assume sampling with replacement here.
alpha<-0.05
301.97-235.79 
s.squ<- sqrt(174.41)
res<-((2*qnorm(1-(alpha/2))*(s.squ/7))^2)          #finding root n 
res
alpha<-0.05
d<-7
quantile<-qnorm(p=1-alpha/2)
sigma<-sqrt(174.41)
n_min <- ceiling ((2 * quantile * sigma / d )^2)
n_min

#6.2

N<-3900
n<-160
#Please use an unbiased estimator to estimate the 
#total of the annual gross salaries of the graduates of university Y.

sample.mean.X<-6396000/n
sample.mean.Y<-8032448/n
pop.tot.mean.X<-(N/n)*6396000
(pop.tot.mean.Y<-(N/n)*8032448)

#Please calculate the lower bound of a two-sided 90 percent 
#confidence interval for the average annual gross salary of the graduates of university Y
alpha<-0.05
var.XUni<-(1/(n-1))*622097138 
var.YUni<-(1/(n-1))*601129632

var.sample.meanX<-(1/n)*var.XUni
var.sample.meanY<-(1/n)*var.YUni
var.pop.tot.meanX<-(N^2/n)*var.XUni
var.pop.tot.meanY<-(N^2/n)*var.YUni

C.lower.X.sm<-sample.mean.X - qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.sample.meanX)
(C.upper.X.sm<-sample.mean.X + qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.sample.meanX))
C.lower.Y.sm<-sample.mean.Y - qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.sample.meanY)
C.upper.Y.sm<-sample.mean.Y + qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.sample.meanY)

C.lower.X.popt<-pop.tot.mean.X - qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.pop.tot.meanX)
C.upper.X.popt<-pop.tot.mean.X + qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.pop.tot.meanX)
C.lower.Y.popt<-pop.tot.mean.Y - qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.pop.tot.meanY)
C.upper.Y.popt<-pop.tot.mean.Y + qnorm(p=(1-(alpha/2)),mean=0,sd=1)*sqrt(var.pop.tot.meanY)

# c) Describe how the length of the confidence interval of part b) would change if the sample 
#    size was 50 instead of 200 - all else being equal.
# Result: 1/4 times the sample size -> interval length increased by 50%


#You assume that the salaries differ significantly across the two universities. 
#Test the hypothesis that the difference between the average annual gross salary 
#of university X and that of university Y is significantly smaller than -10500. 
#Please calculate the test statistic 
#corresponding to your hypothesis. Please note: The variances of the two samples are unequal!

delta<- -11000
(final.res<-(sample.mean.X-sample.mean.Y-delta)/sqrt(var.sample.meanX+var.sample.meanY))

(sqrt(var.sample.meanX+var.sample.meanY))

(sqrt(var.sample.meanX) * sqrt(var.sample.meanY))

qnorm(0.95)^2








