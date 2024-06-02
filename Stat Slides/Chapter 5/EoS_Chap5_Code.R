################################################################################
# Elements of Statistics                                                       #
# Chapter 5: Probability theory                                                #
# Corresponding R Code                                                         #
# Updated: October 2021                                                        #
################################################################################


####################//////////\\\\\\\\\\########################################
# 5.1 Experiments and events
####################\\\\\\\\\\//////////########################################

Omega <- 1:6
A <- Omega[c(1,3,5)]; B <- Omega[c(2,4,6)]; C <- Omega[3]


A_Bar <- Omega[-A]
setequal(x = B, y = A_Bar)
C_Bar <- Omega[-C]
C_Bar


intersect(x = A, y = B)
sort(union(x = B, y = C))


####################//////////\\\\\\\\\\########################################
# 5.2 Probabilities
####################\\\\\\\\\\//////////########################################

Omega <- expand.grid(Dice1 = 1:6, Dice2 = 1:6)
head(Omega, n = 9)
length(Omega[,1])


Sum_of_pips <- apply(Omega, 1, sum)
Omega <- cbind(Omega, Sum_of_pips)
A <- Omega[Omega$Sum_of_pips >= 10, ]
A
length(A[,1])/length(Omega[,1])


B <- Omega[Omega$Sum_of_pips == 4, ]
B
length(B[,1])/length(Omega[,1])


####################//////////\\\\\\\\\\########################################
# 5.3 Conditional probability
####################\\\\\\\\\\//////////########################################

Prob <- length(6)/length(c(2,4,6))
Prob


load("Example5-10.RData")
p_j_k <- addmargins(p_j_k)
p_j_k


Prob_A   <- p_j_k[1,3]
Prob_A_C <- p_j_k[1,1]/p_j_k[3,1]
Prob_A_D <- p_j_k[1,2]/p_j_k[3,2]
Prob_A
Prob_A_C
Prob_A_D


Prob_C <- p_j_k[3,1]
Prob_A * Prob_C == 0.45


Doorknobs_per_machine <- c(1000,2000,3000,4000)
Number_doorknobs <- sum(Doorknobs_per_machine)
Rejects_per_machine <- c(0.08,0.05,0.03,0.02)


P_Ai   <- Doorknobs_per_machine/Number_doorknobs
P_F_Ai <- Rejects_per_machine


P_F <- sum(P_Ai * P_F_Ai)
P_F


P_Ai_F <- P_Ai * P_F_Ai / P_F 
round(P_Ai_F, 3)


P_Ai   <- c(0.3,0.5,NA)
P_B_Ai <- c(0.6,0.5,0.1)


P_Ai[3] <- 1 - P_Ai[1] - P_Ai[2]
P_Ai


P_A1_B <- P_Ai[1] * P_B_Ai[1] / sum(P_Ai * P_B_Ai)
P_A1_B


####################//////////\\\\\\\\\\########################################
# 5.4 Urn models
####################\\\\\\\\\\//////////########################################

# Attention: N and n must be defined
N <- 3
n <- 2
Number_of_variations <- N^n


set.seed(123) # starting point for (pseudo-) random number generator
draws <- sample(x = 1:100, size = 3, replace = TRUE)
draws


# Attention: N and n must be defined
Number_of_variantions <- factorial(N)/factorial(N-n)


set.seed(321) # new starting point
draws <- sample(x = 1:100, size = 3, replace = FALSE)
draws


Number_of_variantions <- choose(49,6)


# Attention: N and n must be defined
Number_of_variantions <- choose(N+n-1,n)
