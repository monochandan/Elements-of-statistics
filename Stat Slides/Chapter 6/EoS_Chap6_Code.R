################################################################################
# Elements of Statistics                                                       #
# Chapter 6: Random variables                                                  #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################



####################//////////\\\\\\\\\\########################################
# 6.1 One dimensional random variables and distributions
####################\\\\\\\\\\//////////########################################

result <- expand.grid(lapply(
  X = 1:3, 
  FUN = function(x) c("H", "T")))                
domain <- expand.grid(lapply(
  X = 1:3, 
  FUN = function(x) c(1, 0)))
number_of_hats <- rowSums(domain)

Example6_1<- data.frame(result,domain,number_of_hats)
names(Example6_1)<- c("result","","","domain","","",
                      "number_of_hats")

save(Example6_1, file="Example6-1.RData")
head(Example6_1, n = 4)


load("Example6-1.RData")
sum(Example6_1$number_of_hats == 1) / 
    length(Example6_1$number_of_hats)
sum(Example6_1$number_of_hats==0 | 
        Example6_1$number_of_hats==1) / 
    length(Example6_1$number_of_hats)
  
  
sum(Example6_1$number_of_hats > 1) / 
    length(Example6_1$number_of_hats)
sum(Example6_1$number_of_hats>0&
        Example6_1$number_of_hats<=2) / 
    length(Example6_1$number_of_hats)


load("Example6-1.RData")
  
x6_3 <- Example6_1$number_of_hats
F_x6_3 <- cumsum(prop.table(x = table(x = x6_3)))
  
plot(ecdf(x6_3), col = "blue", xlab = "x", ylab = "F(x)",
       main = "", xlim = c(0, 4))
points(0:3, c(0, F_x6_3[-4]), col = "blue")
  
F_x6_3


x6_4 <- 0:3 				
f_x6_4 <- c(0.343,0.441,0.189,0.027)	
F_x6_4 <- cumsum(f_x6_4)


plot(x = x6_4, y = f_x6_4, type = "h", lwd = 3,
     xlim = c(0,5), ylim = c(0,0.6), col = "blue", xlab="x",
     ylab = "f(x)", main = "Probability function")
points(x = x6_4, y = f_x6_4, col = "blue", pch = 19)

x_axis <- c(-1,sort(x6_4),4)
y_axis <- c(0,F_x6_4,1) 		  

plot(x = c(0,4), y = c(0,1), main = "Distribution function",
     type = "n", col = "blue", xlab = "x", ylab = "F(x)")

lines(x = x_axis[1:2],y=rep(y_axis[1],2),col="blue",lwd=2)
lines(x = x_axis[2:3],y=rep(y_axis[2],2),col="blue",lwd=2)
lines(x = x_axis[3:4],y=rep(y_axis[3],2),col="blue",lwd=2)
lines(x = x_axis[4:5],y=rep(y_axis[4],2),col="blue",lwd=2)
lines(x = x_axis[5:6],y=rep(y_axis[5],2),col="blue",lwd=2)
points(x = x_axis[1:5],y=y_axis[1:5], col="blue", pch=19)
points(x = x_axis[2:5],y=y_axis[1:4], col="blue", pch=1)


x6_5 <- 0:10

# ATTENTION: here functions
f_x <- function(x) {0.2 * 0.8^x}	

F_x <- function(x) {1 - 0.8^(x+1)}


round(f_x(x6_5), digits = 3)
round(F_x(x6_5), digits = 3)


# Distinction from the latest functions
f_x6_6 <- function(x) {0.5}

F_x6_6 <- function(x) {0.5 * x - 0.5}


F_x6_7 <- function(x) {1 - exp(-1/2 * x)}
f_x6_7 <- function(x) {1/2 * exp(-1/2 * x)}


Mean_X6_8 <- weighted.mean(x = x6_4, w = f_x6_4)
Var_X6_8 <- sum(f_x6_4*(x6_4 - Mean_X6_8)^2)
Mean_X6_8
Var_X6_8


Mean_X6_9 <- integrate(f = function(x){0.5*x}, lower = 1,
                       upper = 3)$value
Mean_X6_9			


Var_X6_9 <- integrate(f = function(x){0.5*x^2}, lower = 1,
                      upper = 3
)$value - Mean_X6_9^2
Var_X6_9


Z6_12 <- (x6_4 - Mean_X6_8) / sqrt(Var_X6_8)
round(Z6_12, digits = 3)


q_050 <- -2 * log(1 - 0.5)
q_025 <- -2 * log(1 - 0.25)
round(q_050, digits = 4) 
round(q_025, digits = 4)


####################//////////\\\\\\\\\\########################################
# 6.2 Multi-dimensional random variables and distributions
####################\\\\\\\\\\//////////########################################

omega <- expand.grid(lapply(X = 1:3,
                            FUN = function(x) c("s", "r", "w")))
Number_of_r <- rowSums(omega == "r")
Number_of_w <- rowSums(omega == "w")
X6_16 <- cbind(Number_of_r, Number_of_w)

Example6_16 <- data.frame(omega, X6_16)
names(Example6_16) <- c("Omega","","",
                        "Number_of_r","Number_of_w")	

head(Example6_16)


Number_of_s <- rowSums(omega == "s")
Example6_16 <- cbind(Example6_16, Number_of_s)
Probs <- 0.3^Example6_16$Number_of_r * 
  0.2^Example6_16$Number_of_w * 
  0.5^Example6_16$Number_of_s

Example6_16 <- cbind(Example6_16, Probs)

pos <- which(Example6_16$Number_of_r == 1 & 
               Example6_16$Number_of_w == 0)
f_1_0 <- sum(Example6_16[pos, 7])
f_1_0


X1_6_17 <- 0:3 ; X2_6_17 <- 0:3
ProbTable6_17 <- matrix(c(0.125, 0.150, 0.060, 0.008,
                          0.225, 0.180, 0.036, 0.000, 
                          0.135, 0.054, 0.000, 0.000, 
                          0.027, 0.000, 0.000, 0.000),
                        ncol = length(X2_6_17), 
                        byrow = TRUE)
dimnames(ProbTable6_17) <- list(X1_6_17, X2_6_17)                            
ProbTable_new6_17 <- addmargins(ProbTable6_17)


ProbTable_new6_17


F_x1_x2 <- t(apply(X = apply(X = ProbTable6_17,
                             MARGIN = 2, FUN = cumsum),
                   MARGIN = 1, FUN = cumsum))
F_x1_x2
F_x1_x2[rownames(F_x1_x2) == 1, colnames(F_x1_x2) == 2]


####################//////////\\\\\\\\\\########################################
# 6.3 Stochastical independence and uncorrelatedness of random vectors
####################\\\\\\\\\\//////////########################################

(ProbTable_new6_17[1,5] * ProbTable_new6_17[5,1]) ==
  ProbTable_new6_17[1,1]


ProbTable6_20 <- matrix(c(0.05,0.14,0.01,
                          0.20,0.56,0.04),
                        ncol = 3, byrow = TRUE)

X1_6_20 <- c(1, 5)
X2_6_20 <- seq(2, 6, 2)

rownames(ProbTable6_20) <- X1_6_20
colnames(ProbTable6_20) <- X2_6_20

ProbTable6_20 <- addmargins(ProbTable6_20)

ProbTable6_20             


round(ProbTable6_20[3,] * ProbTable6_20[1,4],4) ==
  ProbTable6_20[1,]
round(ProbTable6_20[3,] * ProbTable6_20[2,4],4) ==
  ProbTable6_20[2,]


f_X1_6_17 <- rowSums(ProbTable6_17)       
f_X2_6_17 <- colSums(ProbTable6_17)  

Mean_X1 <- sum(f_X1_6_17 * X1_6_17) 
Mean_X2 <- sum(f_X2_6_17 * X2_6_17)

Mean_X1_old <- Mean_X1
Mean_X2_old <- Mean_X2
Mean_X1
Mean_X2


Intermed_matrix6_21 <- matrix(
  rep(x = X2_6_17, times = length(X1_6_17)),
  ncol = length(X2_6_17), byrow = TRUE)

Cov_X1_X2 <- sum(Intermed_matrix6_21 * X2_6_17 *
                   ProbTable6_17) - Mean_X1 * Mean_X2
Cov_X1_X2_old <- Cov_X1_X2
Cov_X1_X2


i <- 1:3 ; ProbTable6_20 <- ProbTable6_20[-3,-4]
Mean_X1 <- weighted.mean(x = X1_6_20,
                         w = prop.table(ProbTable6_20[,2]))
Mean_X2 <- weighted.mean(x = X2_6_20,
                         w = prop.table(ProbTable6_20[2,]))

Cov_X1_X2 <- (sum(X1_6_20[1] * X2_6_20[i] *
                    ProbTable6_20[1,i]) +
                sum(X1_6_20[2] * X2_6_20[i] * 
                      ProbTable6_20[2,i])) - Mean_X1 * Mean_X2
Cov_X1_X2


Var_X1 <- sum(f_X1_6_17 * (X1_6_17 - Mean_X1_old)^2)

Var_X2 <- sum(f_X2_6_17 * (X2_6_17 - Mean_X2_old)^2)

Cor_X1_X2 <- Cov_X1_X2_old / (sqrt(Var_X1 * Var_X2))

round(Cor_X1_X2, digits = 4)


X6_27 <- 1:100
f_x6_27 <- rep(x = 1/100, times = 100)

Mean_X <- sum(f_x6_27 * X6_27)
Var_X <- sum(f_x6_27 * (X6_27 - Mean_X)^2)
Mean_X
Var_X


a0 <- -1
a1 <- 2 
a2 <- 4
# ATTENTION: Means etc. from Ex. 6.21!
Mean_Y <- a0 + a1 * Mean_X1_old + a2 * Mean_X2_old

Var_Y <- a1^2 * Var_X1 + 2 * a1 * a2 * Cov_X1_X2_old + 
  a2^2 * Var_X2
Mean_Y
Var_Y
  