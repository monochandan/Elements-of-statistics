#  1

N = 286
n = 60
v1 = var(RData$turnover.Germany)




v1 = var(RData$turnover.Germany)/n
v12 = var(RData$turnover.USA)/n
v2 = N^2 * 1/n * v1
t.test(RData$turnover.Germany, RData$turnover.USA, mu = 1948.48)


v1 + v12


#   2

N = 1416
n = 70
v1 = var(RData$weight)
c  = (N-n)/(N-1)
v2 = 1/n * v1 *c




qchisq(0.084275,69)
qchisq(0.01,69)

chisq.test(table(RData$color))














N = 272
n = 60
v1 = var(RData$turnover.Germany)
c = (N-n)/(N-1)
var = 1/n * v1 * c


t.test(RData$turnover.Germany,RData$turnover.USA,mu=2147.96)


v2 = var(RData$turnover.USA)/n
v11 = v1/n
v2 + v11








N = 1157
n = 71
v1 = var(RData$weight)
c = (N-n)/(N-1)
var = 1/n * v1 * c


qchisq(0.1452216,70)
qchisq(0.01,70)
chisq.test(table(RData$color))





















N = 208
n = 60
v1 = var(RData$turnover.USA)
c = (N-n) /(N-1)
var = 1/n * v1 * c

t.test(RData$turnover.Germany,RData$turnover.USA,mu = 1766.81)


v2 = var(RData$turnover.Germany)
(v1+v2) / n


N = 1465
n = 62
v1 = var(RData$weight)
c = (N-n) /(N-1)
var = 1/n * v1 * c

qchisq(0.1144235,61)

qchisq(0.10,61)
chisq.test(table(RData$color))






N = 205
n = 60
v1 = var(RData$turnover.USA)
c = (N-n)/(N-1)
v2 = N^2 * v1 * 1/n

v3 = var(RData$turnover.Germany)/n
v4 = var(RData$turnover.USA)/n
v3 +v4
