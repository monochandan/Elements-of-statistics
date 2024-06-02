# 1

d = as.table(matrix(c(50,74,52,239,210,192,6,5,172),byrow = T,ncol = 3))

ch = summary(d)$statistic
ch

k = sqrt(ch / (sum(d) + ch))
k

m = min(dim(d))
m

kmax = sqrt((m-1) / m)
kmax

kstar = k/kmax
kstar




# 2

x = c(50,102,123,78,91,84,31)
y = c(151,216,221,257,141,171,188)

n = length(x)

e = ((n-1)/n * (cov(x,y)))
e

ch = cor(x,y, method = 'pearson')
ch

sqrt(0.0925)

n = 100
s = (1 - ((6*110157) / ((n) * ((n^2)-1))) )
s




# 3

16	+
27	+
24	+
16	+
1	+
5


21+21+13+1-78
22/501
n = (67*78)/501
p = n/501


19/97
t(apply(apply(RData,2,cumsum),1,cumsum))
97/501




# 5

b = cor(RData$TotalJumpingDistance,RData$Score,method = 'pearson')
b


model <- lm(RData$Score ~ RData$TotalJumpingDistance)
summary(model)

summary(model)$r.squared
round(sum(model$residuals),4)  
round(sum((model$residuals)^2),4)  


colnames(RData) <- c("J", "S")
linmod <- lm(S~J, data= RData)
predict(linmod, newdata= data.frame(J=204))










x = as.table(matrix(c(58,70,66,229,224,101,18,24,210),byrow = T,ncol = 3))
x

ch = summary(x)$statistic
ch
k = sqrt(ch / (sum(x) + ch))
k
kmax = sqrt(2/3)
kstar = k/kmax


x = c(71,48,45,65,71,55,85)
y = c(200,178,170,164,183,180,176)

x1 = 6/7 * cov(x,y)
x1
p = cor(x,y,method = "pearson")
p


(0.2582)^2


1 - ((6*129802) / ((100) * (10000-1)))
4	+
10+	
7	+
37+	
22+	
32

114 - 22-39-35-17
1/698
98*131/698
26/121
t(apply(apply(RData,2,cumsum),1,cumsum))
226/698


b = cov(RData$TotalJumpingDistance,RData$Score,method = "spearman")
b
m = lm(RData$Score~RData$TotalJumpingDistance)
summary(m)

round(sum((m$residuals)^2),4)
colnames(RData) <- c("J", "S")
linmod <- lm(S~J, data= RData)
predict(linmod, newdata= data.frame(J=295))

