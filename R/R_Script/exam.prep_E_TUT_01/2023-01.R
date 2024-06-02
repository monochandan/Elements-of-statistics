
median(RData$Colour)


n_j <- table(RData$Colour)
p_j <- prop.table(n_j)
entropy <- -sum(p_j *log(p_j))
entropy

RData
mean(RData$Price)

N = 200
(N-1/N) * var(RData$Price)

IQR(RData$Price,type = 5)

RData

x_o <- c (500 ,1000 ,1750 ,2500,3500,4500,7000)
x2_5_kl <- cut(RData$NetIncome, x_o, right = TRUE )
n_j <- table (x2_5_kl)
p_j <- prop.table(n_j)
p_j
x_mean_approx_j <- x_o[ -7]+(x_o[-1]-x_o[ -7])/2
x_mean_aprrox_kl <- sum(n_j/sum(n_j) * x_mean_approx_j)
x_mean_aprrox_kl

x_var_approx_kl <- sum(p_j * (x_mean_approx_j - x_mean_aprrox_kl )^2)
x_var_approx_kl

hist(RData$NetIncome,breaks = x_o, right = T)

RData

x_o <- c (40 ,50 ,62.5 ,75,87.5,100,130)
x2_5_kl <- cut(RData$Weight, x_o, right = TRUE )
n_j <- table (x2_5_kl)
p_j <- prop.table(n_j)
p_j
x_mean_approx_j <- x_o[ -7]+(x_o[-1]-x_o[ -7])/2
x_mean_aprrox_kl <- sum(n_j/sum(n_j) * x_mean_approx_j)
x_mean_aprrox_kl

va = sum(p_j * (x_mean_approx_j - x_mean_aprrox_kl)^2)
va

Fj <- cumsum(p_j)
Fj

x_0.75 <- x_o[3] + ((x_o[4]-x_o[3])*(0.75-Fj[2])/(Fj[3]-Fj[2]))
x_0.75


69+111+108+90
378/4

data <- c(7.01,7.86,8.43,7.84,7.57,8.89)
data
hm <- 6/sum(1/data)
hm






--------------------------------------------------------------------------------
  
RData
n = table(RData$EfficiencyClass)
p = prop.table(n)
p
e = -sum(p * log(p))
e
mean(RData$Price)


199/200 * var(RData$Price)

1/200 *(sum(sum(RData$Price) - mean(RData$Price)))


x = c(500,1000,1750,2500,3500,4500,7000)
x1 = cut(RData$NetIncome,x,right=T)
n = table(x1)
j = prop.table(n)

x2 = x[-7] + (x[-1]-x[-7])/2
x3 = sum(n/sum(n) * x2)

x4 = sum(j * (x2-x3)^2 )
x4

hist(RData$NetIncome,x,right=T)











x = c(40,50,62.5,75,87.5,100,130)
y = cut(RData$Weight,x,right= T)
n = table(y)
p = prop.table(n)
p

x1 = x[-7] + (x[-1]-x[-7])/2
x2 = sum(n/sum(n) * x1)
x2
x3 = sum(p * (x1-x2)^2)

j = cumsum(p)
xq = x[3] + (x[4] - x[3]) * ((0.5-j[2])/ (j[3]-j[2]))



111+86+92+63
352/4

x = c(9.15,9.1,8.72,6.33,9.01,7.28)
z = 6/sum(1/x)


#---------------------------------------
RData
median(RData$EfficiencyClass)

n_j = table(RData$EfficiencyClass)
p_j = prop.table(n_j)

e = -sum(p_j * log(p_j))
e


mean(RData$EfficiencyClass)
mode(RData$EfficiencyClass)
median(RData$EfficiencyClass)

n=200

ev = 199/200*var(RData$Price)
ev

summary(RData$Price)
248250-154250


x= c(500,1000,1750,2500,3500,4500,7000)
x_o = cut(RData$NetIncome,x,right = T)

n_j = table(x_o)
p_j = prop.table(n_j)
p_j

x_mean_approx_k = x[-7]+(x[-1]-x[-7])/2
x_mean_approx_j1 = sum(n_j/sum(n_j)*x_mean_approx_k)
x_mean_approx_j1

x_mean_approx_l1 = sum(p_j*(x_mean_approx_k-x_mean_approx_j1)^2)
x_mean_approx_l1



x1 = c(40,50,62.5,75,87.5,100,130)
x1
x_0 = cut(RData$Weight,x1,right = T)
x_0

n_j = table(x_0)
p_j = prop.table(n_j)

x_mean_approx_j2 = x1[-7]+(x1[-1]-x1[-7])/2
x_mean_approx_j2 
x_mean_approx_k2 = sum(n_j/sum(n_j)*x_mean_approx_j2)

x_var_approx_k2 = sum(p_j*(x_mean_approx_j2-x_mean_approx_k2)^2)



Fj= cumsum(p_j)
Fj

v = x1[3]+(x1[4]-x1[3])*((0.75-Fj[2])/(Fj[3]-Fj[2]))
v

(89000+80000+75000+89000)/4

x = c(8.87+7.06+2.41+8.02+7.51+10.33)
z = 6/sum(1/x)







nj = table(RData$Colour)
nj

nj = table(RData$EfficiencyClass)
pj = prop.table(nj)
e = -sum(pj * log(pj))
e
mean(RData$Price)
199/200 * var(RData$Price)

summary(RData$Price)
258750  -148750  


x = c(500,1000,1750,2500,3500,4500,7000)
x1 = cut(RData$NetIncome,x,right=T)

n = table(x1)
p = prop.table(n)

x3 = x[-7] + (x[-1] - x[-7])/2
x4 = sum(n/sum(n) * x3)
x4


x5 = sum(pj * (x3-x4)^2)
x5

hist(RData$NetIncome,breaks = x,right=T)



x = c(40,50,62.5,75,87.5,100,130)
x1 = cut(RData$Weight,x,right=T)
n = table(x1)
p = prop.table(n)

x3 = x[-7] + (x[-1]-x[-7])/2
x4 = sum(n/sum(n) * x3)
x4

x5 = sum(pj * (x3-x4)^2)
x5


f = cumsum(p)
f

i = x[3] + ((x[4]-x[3]) * ((0.75-f[2]) / (f[3]-f[2])))
i
47+67+70+66
250/4
x = c(6.88,7.07,4.74,8.02,8.71,6.98)
am = 6/sum(1/x)
am
#


