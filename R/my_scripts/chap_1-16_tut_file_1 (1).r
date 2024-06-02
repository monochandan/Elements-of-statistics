# 1.1 

load("Pillar_01_QP_01_00_20151130008aa.RData")
data<-data.frame(RData)
data
#mode of the effciency class

median(data$EfficiencyClass)

#entropy of nominal variable
n_j <- table(RData$EfficiencyClass)
p_j<-prop.table(n_j)
p_j
entropy<- -sum(p_j*log(p_j))
entropy

#suitable measure of location of metric varia ble

mean(data$Price)

#emperical varience s*2
n<-length(data$Price)
emp.price<-1/n*sum((data$Price-mean(data$Price))^2)  #or 
emp.price_b<-(n-1)/n*var(data$Price)
emp.price

#interquantile range of maetric variable
quant <- quantile(data$Price, probs=seq(0,1,0.25), na.rm=F, type=5)
IQR.price<-as.numeric(quant[4]-quant[2])
IQR.price
#mean linear deviation
z<- median(data$Price)

# or alternatively 
z <- as.numeric(quant[3])
z <- quantile(data$Price, probs = 0.5, type = 7)

MLD <- 1/length(data$Price) * sum(abs(data$Price - z))
MLD

#1.2

load("C:/Users/sanaa/OneDrive/Desktop/Elements of Stas/Etutorial1/data2.RData")
data<-data.frame(RData)
data<-as.matrix(data)
data

#grouping the data
x_o<-c(500,1000,1750,2500,3500,4500,7000)
data_binned<-cut(RData$NetIncome,breaks=x_o,right=T)
data_binned
n_j<-table(data_binned)
n_j
p_j<-prop.table(n_j)

k<-length(x_o)

x_prime<-x_o[-k]+(x_o[-1]-x_o[-k])/2
x_prime

a
#x_mean_j<-tapply(data,INDEX=data_binned,FUN=mean) #true class mean

approx.ar.mean<-sum(p_j*x_prime)
approx.ar.mean

#approximate variance for classified data
emp.classified<-sum(p_j*(x_prime-approx.ar.mean)^2)
emp.classified


#histogram for grouped data
hist(data,breaks=x_o,right=T)

#1.3

load("Pillar_01_QP_03_00_20151130023.RData")
data<-data.frame(RData)
data

#grouping the data
data<-as.matrix(RData)
data
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

#variance of classified data
emp.var.weight<-sum(p_j_2*(x_prime_2-approx.ar.mean.weight)^2)
emp.var.weight

#calculate the approximate 0.5-quantile on the basis of
#the classified data by using a linear interpolation.

F_j<-cumsum(p_j_2)
F_j
p<-0.5
x_o_2
j<-3
quantile <- x_o_2[j]+((x_o_2[j+1]-x_o_2[j])*((p-F_j[j-1])/(F_j[j] - F_j[j-1])))



#1.6


#calculate the average number of unemployed people in the regions B, I, J and K
data<-c(76000,41000,57000,89000)
sum(data)/4


#alculate the average unemployment rate in the regions A, B, G, I, L 
#and O on the basis of a fitting measure of location. The unemployment rate 
#(UR) is defined as the proportion of the number of 
#unemployed persons (U) and the number of employed persons 
#(E). According to that, the following holds: UR = U/E. 
#Please state your result in percent! 

#using harmonic mean
n<-6
data<-c(4.31,7.56,5.44,4.22,7.77,11.87)
data

har.mean<-n/(sum(1/data))
har.mean


#2.1

#calculate the value of the standardised contingency coefficient.

data<-rbind(c(58,70,66),c(229,224,101),c(18,24,210))
data
colnames(data)<-c("Action","Fantasy","Comedy")
rownames(data)<-c("Economics","Business","Social science")
data
#addmargins(data)
#n_jk_star<-margin.table(data,1)%*%t(margin.table(data,2))/margin.table(data)
#chisq<-sum((data-n_jk_star)^2/n_jk_star)
chisqare <-chisq.test(data)$statistic
chisqare
K<-sqrt(315.4441/((margin.table(data)+315.4441)))
K
M<-min(dim(data))
M
K_max<-sqrt((M-1)/M)
K_star<-K/K_max
K_star

#2.2

pth<-c(52,78,31,116,39,71,55)
pa<-c(160,200,178,218,125,138,136)
# empirical covariance between the variables 
#preparation time in hours and achieved points.
sum((pth-mean(pth))*(pa-mean(pa)))/7

#calculate Spearman's rank 
#correlation coefficient between the 
#variables preparation time in hours and achieved points.
n<-length(pth)
SP<-cor(rank(pth),rank(pa),method = "spearman")
SP         #OR

Sp<-1-(6*sum((rank(pth)-rank(pa))^2)/(n*(n^2-1)))
Sp

# sum of squares diff between X and y is given 62189

SP<-1-((6*62189)/(100*(100^2-1)))
SP

#2.3

load("Pillar_03_QP_06_00_20151129029.RData")
data<-data.frame(RData)
data<-as.matrix(data)

#calculate the missing value of the marginal distribution in the table
addmargins(data)

#calculate the absolute joint frequency njk for the missing 
#cell in the table at hand. 
268-44-82-7-81-41

#e assume independence between the two variables. Please 
#calculate the relative frequency p???jk, with j=4 and k=1 
#under the assumption of independency.
n<-1499
n_jk_star<-(321*298)/1499
p_jk<-n_jk_star/n
p_jk

#calculate the conditional relative frequency pj|k with j=1 and k=1
n_jk<-78
n_k<-298
pjk<-n_jk/n_k
pjk

#bivariate empirical distribution function as well. 
#Please calculate the value of Fn(x,y) at x=I3 and y=A.

data[5,5]=13
data
data
F_j_k<-t(apply(X=apply(X=data,MARGIN = 2,FUN=cumsum),MARGIN=1,FUN=cumsum))
F_j_k
F_j_k/1499

#2.5

load("Pillar_04_QP_01_00_20151129016.RData")
RData
head(RData)
attach(RData)

#Calculate the relation between the reached score and the 
#total jumping distance by means of the correlation 
#coefficient of Bravais-Pearson. 

cor(Score,TotalJumpingDistance)

#You assume a linear relationship between the total 
#jumping distance and the reached score (dependent variable). 
#Hence, set up a suitable linear regression model and calculate 
#the slope by means of the method of ordinary least sqares. 

res<-lm(Score~TotalJumpingDistance)
#slope <- cor(TotalJumpingDistance,Score) * (sd(Score) / sd(TotalJumpingDistance))
#slope
#Furthermore, determine the value of the 
#coefficient of determination of your regression model.
summary(res)

#Now assume that the total jumping distance should be measured 
#in kilometres instead of metres. Which value does 
# the coefficient of determination have after this transformation?

data1<-TotalJumpingDistance/1000
data1
res1<-lm(Score~data1)
res1
summary(res1)
#State the value of the sum of the squared residuals 
# of your regression model. 

round(sum(residuals(res)), 4)


#Assume that an athlete reaches a total jumping distance 
#of 279 metres. Which score do you expect for this athlete, 
#if you take your calculated regression model as a basis.
x<-279
a<-2.04642
b<- -273.73025
y<-a*x+b
y
