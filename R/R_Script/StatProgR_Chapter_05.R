plot(x = 1:10)

plot(x = 1:10, y = 10:1)

plot(pressure ~ temperature, data = pressure)

png("A_plot.png", width = 1920, height = 1080)
plot(1:10, rep(1, 10))
dev.off()

plot(x = 1:10, y = 10:1, pch = 18, col = "blue", cex = 2)

plot(1:10, 10:1, pch = 22, cex = 4, lwd = 3, bg = "red")

plot(1:10, 10:1, type = "l")

plot(1:10, 10:1, type = "l", lty = 4, lwd = 3, las = 2)

data(state)
head(state.x77[, c("Illiteracy", "Life Exp")])
ill <- state.x77[, "Illiteracy"]
lif <- state.x77[, "Life Exp"]

plot(ill, lif, main = "Literacy and life expectancy", 
     sub = "US states around 1970",
     xlab = "Share of illiterates (%)", 
     ylab = "Life expectancy (years)"
     )

blond_females <- HairEyeColor["Blond", , "Female"]
barplot(blond_females, 
        main = "Eye colours in blond female students")

barplot(blond_females, horiz = TRUE,
        main = "Eye colours in blond female students",
        names.arg = 1:length(blond_females))

blond_students <- HairEyeColor["Blond", , ]
barplot(t(blond_students), beside = TRUE, # otherwise stacked
        legend.text = c("M", "F"), 
        main = "Eye colours in blond students")

females <- HairEyeColor[, , "Female"]
(hair <- apply(females, MARGIN = 1, FUN = sum)) # row sums
rowSums(females)
apply(females, MARGIN = 2, FUN = sum) # column sums
hair_relative <- hair / sum(hair) 
# relative frequencies of hair colours

apply(Titanic, c(1, 4), sum)

pie(hair_relative, main = "Hair colours in female students",
    col = c("black", "brown", "red", "yellow"))

length(palette())
palette()[1:4]; palette()[5:8]
head(colours(), 3) # head(colors(), 3)
colours()[62]

plot(1:8, rep(1, 8), col = 1:8, pch = 18, cex = 4,
     xlab = "", ylab = "") # no axis labels

plot(1:8, rep(1, 8), col = rainbow(8), pch = 18, cex = 4,
     xlab = "", ylab = "")

plot(1:2, 2:1, col = colours()[62], pch = 16, cex = 8,
     xlab = "", ylab = "", ylim = c(0, 3)) # y-axis limits

plot(1:5, rep(1, 5), 
     col = rgb(0.6, 0.2, 0.8, alpha = seq(0.2, 1, 0.2)), 
     pch = 17, cex = 6, xlab = "", ylab = "")

col2rgb("cornflowerblue")

histogram <- hist(state.x77[, "Population"] / 1000,
                  main = "Histogram of US state populations", 
                  xlab = "Millions (estimate from 1975)",
                  freq = FALSE) # areas sum to one

str(histogram)
breaks <- histogram$breaks
density <- histogram$density
sum((breaks[-1] - breaks[-length(breaks)]) * density)

hist(state.x77[, "Population"] / 1000, col = "yellow",
     main = "Histogram of US state populations", 
     xlab = "Millions (estimate from 1975)",
     freq = FALSE, breaks = c(0, 1:5, 10, 20, 25))

boxplot(chickwts$weight ~ chickwts$feed, 
        xlab = "Feed type", ylab = "Weight (g)")

par(mfrow = c(2, 2)) # 2 rows, 2 columns, by rows
plot(1:10, 10:1); plot(1:10, 1:10)
plot(1:10, 1:10); plot(1:10, 10:1)
par(mfrow = c(1, 1)) # reset once done

par(mfrow = c(1, 2))
plot(1:10, 1:10)
par(mar = c(10, 4, 8, 2)); plot(1:10, 1:10)
par(mfrow = c(1, 1))

plot(iris)

# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(iris[, 1:3], color = 4)

dat <- LifeCycleSavings[, c("pop15", "sr")]
plot(dat$pop15, dat$sr, main = "Savings and age",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
abline(h = mean(dat$sr), v = mean(dat$pop15), lwd = 3)
points(dat[c(which.max(dat$sr), which.min(dat$pop15)), ], 
       pch = 8, cex = 4, lwd = 6, col = 2)

plot(dat$pop15, dat$sr, main = "Savings and age",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
lines(x = dat[c(1, nrow(dat)), 1], 
      y = dat[c(1, nrow(dat)), 2], col = 2)

top_right <- dat[, 1]>mean(dat[, 1]) & dat[, 2]>mean(dat[, 2])
temp<-dat[top_right, ]; peculiar<-temp[which.max(temp$sr), ]
plot(dat$pop15, dat$sr, main = "Savings and age",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
text(peculiar + 1, labels = "Peculiar observation", col = 2)

plot(dat$pop15, dat$sr, main = "Savings and age",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
grid(nx = 5, ny = 5, lwd = 3)

g7 <- c("United States", "Japan", "Germany", "France",
 "United Kingdom", "Italy", "Canada")
plot(dat$pop15, dat$sr, main = "Savings and age",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
points(dat[rownames(dat) %in% g7, ], col = 2, cex = 3)
legend("topleft", legend = c("G7", "Non-G7"), pch = 1, 
       col = 2:1, pt.cex = c(3, 1))

plot(dat$pop15, dat$sr, main = "Savings and age",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
rect(xleft = 0.9 * min(dat$pop15), ybottom = 5,
     xright = 1.1 * max(dat$pop15), ytop = 15,
     col = adjustcolor("red", alpha.f = 0.3))

plot(dat$pop15, dat$sr, main = "Savings and age", xaxt = "n",
 xlab = "Pop. share under 15 (%)", ylab = "Savings ratio (%)")
axis(side = 1, at = seq(20, 40, 2),
     labels = seq(20, 40, 2), tcl = 0.5)

plot(1:10, 1:10, type = "n", xlab = "", ylab = "")
text(2, 5, expression(x %+-% y), cex = 3)
text(4, 5, expression(sqrt(9)), cex = 3)
text(6, 5, expression(widehat(beta)), cex = 3)
text(8, 5, expression(x[i]^{2}), cex = 3)

#install.packages("lattice")
library(lattice)

xyplot(lat ~ long, data = quakes, pch = ".")

scatter <- xyplot(lat ~ long, data = quakes, pch = ".")
update(scatter, main = "Earthquakes in the South Pacific")

depth_g <- equal.count(quakes$depth, number = 4, overlap = 0)
xyplot(lat ~ long | depth_g, data = quakes, pch = ".")

mag_g <- equal.count(quakes$mag, number = 2, overlap = 0)
xyplot(lat ~ long | depth_g * mag_g, data = quakes, pch = ".",
       xlab = "Longitude", ylab = "Latitude")

#install.packages("ggplot2")
library(ggplot2)
ggplot(data = mpg) + # The + has to be at the end of a line.
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, colour=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy), colour = 4)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2) # additional layer

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl) # additional layer

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(colour = class)) +
  geom_smooth()
