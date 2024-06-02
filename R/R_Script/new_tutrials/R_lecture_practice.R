getwd()
rm(list = ls())

sink("testR.txt")
c(1:10, rep(1, 4))
sink()

c(1:10)
seq(from = 1, to = 10, by = 1)
1:10

??seq
help.start()
seq(to = 20, by = -5, length.out = 5)

#x <- c(20, 19, 25, 1)
y <- c(9, 10, 4, 28)
(z <- x + y)
save(x, file = "x1.RData")
load("x1.RData")
ls()
x

some_name <- rep(1, 4)
save(some_name, file = "some_name.RData")

new_name <- get(load("some_name.RData"))
new_name
x
sort(x)

some_vector <- c(seq(2, 20, 2), 1:10, seq(3, 30, 3))
some_vector

some_vector[c(rep(5, 5), rep(1, 5))]

(m1 <- matrix(data = 1:16, nrow = 4, byrow = TRUE))

m1[2, -2]
m1[,1:2]
m1[3:4,]

(to_manipulate <- seq(4, by = 4, length.out = 4))
m1[to_manipulate] <- 100
m1[16]

(m4 <- matrix(1:4, ncol = 2))

(rbind(m4, c(3,5)))
(cbind(m4, c(5, 6)))

m5 <- matrix(1:6, ncol = 3)
colnames(m5) <- c("var1", "var2", "var3")
#id <- c("i01", "i02")
#(m5_new <- cbind(m5))
m5
m6 <- matrix(c(101, 202, 85, 200, 54, 20, 67, 89, 90), ncol = 3)
colnames(m6) <- c("university1", "university2", "university3")
m6
rownames(m6) <- c("(0-100]", "(100-200]","(200-300]")
mode(m6)
plot(m6, xlab = )

uni_1 <- m6[,"university1"]

ant <- (1:length(uni_1))/length(uin_1)
make_table

plot(x = 1:10)
plot(x = 1:10, y = 10:1)
plot(pressure ~ temperature, data = pressure)

plot(x = 1:10, y = 10:1, pch = 11, col = "blue", cex = 2)

plot(1:10, 10:1, pch = 22, cex = 4, lwd = 5, bg = "red")
plot(1:10, 10:1, type = "l", lty = 4)

data(state)
head(state.x77[, c("Illiteracy", "Life Exp")])

ill <- state.x77[, "Illiteracy"]
lif <- state.x77[, "Life Exp"]


plot(ill, lif, main = "Literacy and life expectancy",
     sub = "US state arround 1970",
     xlab = "Share of illiteracy (%)",
     ylab = "Life Expectancy (years)", identify(Arizona))

identify(ill, lif, plot = TRUE)

?identify








