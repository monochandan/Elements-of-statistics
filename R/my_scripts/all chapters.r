#1.1
attach(RData)
head(RData)
n_j<-table(color)
which(n_j==max(n_j))
median(EfficiencyClass)
nj<-table(EfficiencyClass)
pj<-prop.table(nj)
entropy<--sum(pj*log(pj))
entropy
mean(Price)
n<-length(Price)
n
emp.var<-(1/n)*sum((Price-mean(Price))^2)
emp.var
quant<-quantile(Price,probs = seq(0,1,.25),na.rm = F,type = 5)
IQR<-as.numeric(quant[4]-quant[2])
IQR
#1.2
attach(RData)
head(RData)
x_o<-c(500,1000,1750,2500,3500,4500,7000)
income_binned<-cut(NetIncome,breaks = x_o,right = T)
income_binned
data<-data.frame(RData)
data<-as.matrix(data)
hist(data,breaks = x_o,right = T)
nj<-table(income_binned)
pj<-prop.table(nj)
n<-length(x_o)
x_prime<-x_o[-n]+( x_o[-1] - x_o[-n]) / 2
am<-sum(pj*x_prime)
am
av<-sum(pj*(x_prime-am)^2)
av
#1.3
attach(RData)
head(RData)
x<-c(40,50,62.5,75,87.5,100,130)
weight_binned<-cut(Weight,breaks = x,right = T)
n<-length(x)
nj<-table(weight_binned)
pj<-prop.table(nj)
x_prime<-x[-n]+(x[-1]-x[-n])/2
am<-sum(pj*x_prime)
am
av<-sum(pj*(x_prime-am)^2)
av
F_j<-cumsum(pj)
p<-0.75
j<-4
x_median <- x[j] + ( x[j+1] - x[j]) *
  (0.75 - F_j[j-1]) /( F_j [j] - F_j[j-1])
#1.6
U<-c(76000,41000,57000,89000)
mean(U)
M<-c(4.31,7.56,5.44,4.22,7.77,11.87)
n<-length(M)
har<-n/sum(1/M)
har
#2.1
data<-rbind(c(59,66,77),c(250,203,130),c(14,24,177))
data
n_j_k<- margin.table (data,1)%*%t( margin.table (data,2)) / margin.table (data)
n_j_k
chisq<-sum((data-n_j_k)^2/n_j_k)
chisq <- chisq.test(data)$statistic
chisq
K <- sqrt ( chisq /( sum (data) + chisq ))
M <- min ( dim (data))
K_max <- sqrt ((M -1) /M)
K_star <- K/ K_max
K_star
#2.2
time<-c(83,77,65,64,38,57,106)
points<-c(216,185,205,164,168,107,221)
n<-length(time)
emp.covar<-1/n*sum((time-mean(time))*(points-mean(points)))
emp.covar
pearson<-cor(time,points,method = "pearson")
pearson
cor(time,points)
sqrt(0.3926)
1-((6*66583)/(100*(100^2-1)))
#2.3
(RData)
addmargins(RData)
259-(19+63+21+64+17)
RData[5,6]=75
RData
addmargins(RData)
75/1501    # relative joint frequency pjk 
(320*290)/1501 #absolute frequency n???jk, 
(59)/290
F_j_k <- t( apply ( apply (RData , 2, cumsum ), 1, cumsum ))
F_j_k
F_j_k<-t(apply(X=apply(X=RData,MARGIN = 2,FUN=cumsum),MARGIN=1,FUN=cumsum))
F_j_k
F_j_k/1501
#2.5
attach(RData)
head(RData)
cor(rank(TotalJumpingDistance),rank(Score),method = "spearman")
a<-lm(Score~TotalJumpingDistance)
summary(a)
sum(residuals(a)^2)
x<-240
a<-2.0253
b<--276.8824
y<-a*x+b
y
#3.1
attach(RData)
head(RData)
ytk<-ts(BIP_DK,start=c(2000,1),frequency = 4)
ytrend<-decompose(ytk,type = "additive")$trend
ytrend
ynorm<-decompose(ytk,type="additive")$figure
ynorm
zt<-ytk-ynorm
error<-zt-55.0134
error
t<-1:length(zt)
reg<-lm(zt~t)
summary(reg)
b<-42.6258
a<-0.3926
k<-52+2
y<-a*k+b
GDP<-y+ynorm[2]
GDP
ytk
((45.6012/42.7805)-1)*100
#3.2
43/51
(.16*8)/(.84*43+.16*8)
#3.3
(3*11*8)/(8*15*12)
(3/8)+(11/15)+(8/12)
((5*11*8)+(3*4*8)+(3*11*4)+(3*11*8))/(8*15*12)
5/8
#3.4
(30+32+53)/400
32/(32+53)
#4.1


(5*9*4)/(8*15*12)
(9*5*8)/(9*5*8+5*9*4)
(3*9*4+4*5*6+5*8*9+4*5*9)/(8*15*12)
(5/8)+(9/15)+(4/12)
#4.2
(86+34)/400
34/(34+46)
1/6
#4.3
11/36
5/12
#4.4


x<-c(1,3,6)
y<-c(0,2,3)
data<-matrix(c(0.28,0.02,0.03,0.18,0.12,0.03,0.05,0.01,0.28),nrow = 3,byrow = T)
data
fx<-colSums(data)
fy<-rowSums(data)
meanx<-weighted.mean(x=x,w=fx)
meanx

meany<-weighted.mean(x=y,w=fy)
meany
var<-sum(fy*(y-meany)^2)
var
(2*0.03+3*0.28)/(0.03+0.03+0.28)
6*1.68+7


#4.5
125/216
(75+60+36)/216
e1<-1/6*1/6*1/6
e2<-1/6*1/6*1/6*5/6*5/6*5/6
e3<-1/6*1/6*1/6*5/6*3
e4<-1/6*1/6*1/6*5/6*5/6*3
e1+e2+e3+e4
#4.6
8/11
1-(6/64)


#5.1
u<-c(2/10,-3/10,9/10,7/10)
sum(u)
sum(u^2)
k<-rep(1/6,6)
sum(k^2)


#5.2
mu<-11*18+6*11+10*20-2*6
mu
k1<-10
k2<-5
VarX<-2*((k1+k2???2)/(k1*(k2???4)))*(k2/k2-2)^2
VarX
65/9
qchisq(p=0.01,df=5)
var<-11*11+36*49+900+4*36
var
qnorm(p=0.1,mu,sqrt(var))




#6.1
N<-1200
n<-72
sm<-22271/n
sm
pop.total<-N*sm
var<-1488481.65/(n-1)
var.pop.total<-N^2/n*var*(N-n)/(N-1)
var.pop.total
alpha<-0.1
c.lower<-pop.total-qnorm(p=1-(alpha/2),mean = 0,sd=1)*sqrt(var.pop.total)
c.lower
alpha <- 0.01
Quantile <- qnorm (p = 1 - alpha / 2)
sigma <-sqrt(289.51)
d <- 3
n_min <- ceiling ((2 * Quantile * sigma / d )^2)
n_min



#6.2
N<-3300
n<-120
smx<-4810957/n
smy<-5966307/n
tot.smx<-N*smx
tot.smy<-N*smy
varx<-486693544/(n-1)
vary<-455582199/(n-1)
var.smx<-1/n*varx
var.smy<-1/n*vary
alpha<-0.05
c.uppery<-smy+qnorm(p=1-(alpha/2),mean=0,sd=1)*sqrt(var.smy)
(smx-smy+9000)/sqrt(var.smx+var.smy)


#6.3
u<-c(3/7,-9/7,-2/7,-10/7)
sum(u)
sum(u^2)
p<-rep(1/5,5)
sum(p^2)


#7.1
attach(RData)
head(RData)
Ng<-337
Nus<-200
n<-60
varg<-1/(n-1)*sum((turnover.Germany-mean(turnover.Germany))^2)
sm.varg<-1/n*varg*(Ng-n)/(Ng-1)
test<-t.test(turnover.Germany,turnover.USA,alternative = "greater",conf.level = 0.95,mu=2118.32)
test$statistic
abs(test$statistic)
((mean(turnover.USA)-mean(turnover.Germany)+2118.32)/test$statistic)^2


#7.2
attach(RData)
head(RData)
N<-1007
n<-85
sm<-mean(weight)
var<-1/(n-1)*sum((weight-sm)^2)
var.sm<-1/n*var*(N-n)/(N-1)
(n-1)/121.764507*var(weight)
qchisq(p=0.1,df=n-1)
data<-table(color)
k<-length(color)
alpha<-0.1
d<-rep(1/4,4)
chisq.test(data,p=d,rescale.p = T,correct = F)
chisq.test()