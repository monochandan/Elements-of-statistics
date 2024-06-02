df1 <- data.frame(name = c('Alfred','Bernerd', 'Cicilia', 'David', 'Alvira'),
               height_in_cm = c(185,180,160,190,150),
                weight_in_kg = c(78,90,55,85,55),
                speed_in_min_per_km = c(4.3, 6.3,5.0,5.2,5.0))


df1


library(ggplot2)

#ggplot(data, aes(x=data.height_in_cm, y=data.weight_in_kg)) +
 ## geom_point()


plot(x = df1$height_in_cm, y = df1$weight_in_kg,
      xlab = "height",
      ylab = "weight",
      xlim = c(150,190),
      ylim = c(50, 90),
      main = "Weight vs height"
     )


x1 = df1$height_in_cm
y1 = df1$speed_in_min_per_km

x2 = df1$weight_in_kg
y2 = df1$speed_in_min_per_km

plot(x1, y1, col = 'darkgreen', pch = 19)

points(x2, y2, col = 'red', pch = 19)

legend(50,190,legend = c('var1', 'var2'), pch = c(19,19), col = c('darkgreen', 'red'))




data(mtcars)
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 2),
      main = "Motor Trend Cars", full = FALSE) 

stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 2),
      main = "Motor Trend Cars", draw.segments = TRUE)

data(USJudgeRatings)
stars(USJudgeRatings, labels = abbreviate(case.names(USJudgeRatings)),
      key.loc = c(13, 1.5), main = "Judge not ...", len = 0.8)



stars(df1,labels = abbreviate(case.names(df1)), len = 0.8, key.loc= c(13, 5),
      main = "Height Weight Speed of Individuals", full = TRUE, draw.segments = TRUE)
