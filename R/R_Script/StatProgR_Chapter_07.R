head(read.csv("Wage.csv"))
some_data <- read.csv("Wage.csv")

plot(wage ~ educ, data = some_data)

mod <- lm(wage ~ educ, data = some_data)
mod

plot(wage ~ educ, data = some_data)
abline(mod, col = 2, lwd = 2)

predict(mod, newdata = data.frame(educ = c(10, 13, 18)))
mod$coef[1] + mod$coef[2] * c(10, 13, 18)

head(mod$residuals, 5)

sum_mod <- summary(mod)
head(names(sum_mod), 3)
sum_mod$r.squared

sum_mod

sum_mod$coefficients[, "Pr(>|t|)"] < 0.05

#install.packages("wooldridge")
library(wooldridge)
mod_new <- lm(wage ~ educ + exper, data = wage1)

predict(mod_new, newdata = data.frame(educ = 18,
                                      exper = 10))
predict(mod_new, newdata = data.frame(educ = 18,
                                      exper = 10),
        interval = "confidence"
        )
summary(mod_new)$adj.r.squared

#install.packages("car")
library(car)
head(Prestige$type)
another_mod <- lm(prestige ~ education + income + type, 
                  data = Prestige
                  )

summary(another_mod)$coefficients[, 1:2]

logit_mod <- glm(inlf ~ nwifeinc + educ + exper +
                        exper^2 + age + kidslt6 +
                        kidsge6,
                 data = mroz,
                 family = binomial(link = "logit")
                 )

summary(logit_mod)$coef

new_pred <- data.frame(nwifeinc = mean(mroz$nwifeinc) * 2,
                       educ = 18, exper = 5,
                       age = 35, kidslt6 = 2,
                       kidsge6 = 0
                       )
predict(logit_mod, newdata = new_pred)
predict(logit_mod, newdata = new_pred, type = "response")

(pred_tab <- prop.table(
             table(Obs = mroz$inlf,
                   Preds = round(fitted(logit_mod))
                   )
             ))
sum(diag(pred_tab)) / sum(pred_tab)

N <- 10000
set.seed(123)
x <- rnorm(n = N, mean = 10, sd = 2)
betas <- c(1, 3)
set.seed(124)
errors <- rnorm(N, 0, 8)
y <- cbind(1, x) %*% betas + errors

plot(x, y)
pop_mod <- lm(y ~ x)
abline(pop_mod, col = 2, lwd = 2)

R <- 2000
n <- 200
set.seed(125)
units <- lapply(rep(n, R), function(x) 
                sample(1:N, x, replace = TRUE)
                )
head(units[[1]])
head(units[[2]])

dat_df <- data.frame(x = x, y = y)
sample_slopes <- sapply(units, function(z)
                        lm(y ~ x, data = dat_df[z, ])$coef[2]
                        )
summary(sample_slopes)
pop_mod$coef

plot(density(sample_slopes), 
     main = "Sampling distribution of slope estimator")

set.seed(126); samples2 <- units[sample(1:R, 4)]; plot(x, y)
lapply(1:4, function(x) {
 points(dat_df[samples2[[x]], ], col = x + 1)
 abline(lm(y ~ x, data = dat_df[samples2[[x]], ]),
        col = x + 1, lwd = 2)})

n <- 1000; set.seed(127) 
Abscissa <- runif(n, -1, 1); Ordinate <- runif(n, -1, 1)
#install.packages("plotrix")
library(plotrix); plot(Abscissa, Ordinate, asp = 1)
draw.circle(0, 0, 1)

inside <- (Abscissa^2 + Ordinate^2) <= 1
plot(Abscissa, Ordinate, asp = 1)
draw.circle(0, 0, 1)
points(Abscissa[inside], Ordinate[inside], col = 2, lwd=1.5)
mean(inside) * 4
