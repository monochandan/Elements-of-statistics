some_numbers <- c("3", "2", "6", "1", "8")
if (is.character(some_numbers)) {
  some_numbers <- as.numeric(some_numbers) # silent
}
sum(some_numbers)

if (is.character(some_numbers)) print("Oh, characters...")

if (sum(some_numbers) > 10) {
  print("The sum is reasonably large.")
} else {
    print("The sum is rather small.")
  }

ifelse(some_numbers > 5, ">5", "<=5")

for (i in 1:5) {
  print(i * 3) # 1 * 3, 2 * 3, ...
}

for (i in 1:5) {
  cat(i, "times 3 is", i * 3, ".\n")
}

for (name in c("Hanna", "Julia", "Anna")) {
  cat(name, "loves R!\n")
}

system.time( # uses difference between two calls of proc.time
  {
  cum_sum <- 0 
  for (i in 1:1e4) {
    cum_sum <- cum_sum + i
  }
  }
)
cum_sum

system.time(
  {
  nat_numbers <- 1:1e4
  cum_sum2 <- cumsum(nat_numbers)[length(nat_numbers)]
  }
)
cum_sum2

state_df <- data.frame(rep(state.x77[, "Population"], 1e4),
                       rep(state.x77[, "Area"], 1e4))
pop_area <- numeric(nrow(state_df))
system.time(
  for (i in 1:nrow(state_df)) {
    pop_area[i] <- state_df[i, 1] / state_df[i, 2]
  }
)

system.time(apply(state_df, 1, function(x) x[1] / x[2]))

n <- 5
constant <- 2 * n * pi
some_product <- numeric(n)
for (i in 1:n) {
  some_product[i] <-
    # Bad: 2 * n * pi * (i + 10)
    constant * (i + 10) # Good
}
some_product

fun1 <- function(n) {
  a <- NULL
  for (i in 1:n) {
    a <- c(a, i^2)
  }
}
fun2 <- function(n) {
  a <- numeric(n)
  for (i in 1:n) {
    a[i] <- i^2
  }
}
fun3 <- function(n) a <- (1:n)^2

system.time(fun1(2e5))
system.time(fun2(2e5))
system.time(fun3(2e5))
# Do not run with any of the other functions
system.time(fun3(2e8))

treatment <- levels(OrchardSprays$treatment)
avg_response <- numeric(length(treatment))
names(avg_response) <- treatment
for (i in treatment) {
  members <- which(OrchardSprays$treatment == i)
  avg_response[i] <- mean(OrchardSprays$decrease[members])
}
avg_response

tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)

ranges <- 
tapply(OrchardSprays$decrease, OrchardSprays$treatment, range)
ranges[1:4]

# Built-in data set mtcars
tapply(mtcars$mpg,
       list(mtcars$am, mtcars$cyl),
       mean
       )

for (i in 1:4) {
  if (i == 3) break
  print(i)
}
for (i in 1:4) {
  if (i == 3) next
  print(i)
}

i <- 0 # Repetition counter
e <- exp(1) # Target value
approx_e <- 0 # Initial approximation
while (e - approx_e > 10^(-5)) {
  approx_e <- approx_e + 1 / factorial(i)
  i <- i + 1 # Counter has to be updated "manually"
}
i; approx_e; e

# Do not run: lala <- 3; while (lala > 2) lala <- lala + 1

my.sum <- function(summand_one, summand_two) { # funct. head
  summand_one + summand_two # funct. body
}
my.sum(9, 1)

`*`(3, 3)
`>`(10, 5)
`[`(seq(3, by = 3, length.out = 5), 3)

my.sum

my.sum <- function(summand_one, summand_two = 1) {
  summand_one + summand_two
}
my.sum(20)

my.sum <- function(summand_one, summand_two = 1) {
  result <- summand_one + summand_two
  output <- c(summand_one, summand_two, result)
  names(output) <- c("Summand1", "Summand2", "Sum")
  output
}
my.sum(20)

my.stats <- function(x, y) {
  x_range <- range(x)
  x_mean <- mean(x)
  x_in_y <- any(x %in% y)
  report <- paste("We checked ", length(x), 
                  " units. ", ifelse(x_in_y, "Some ", "No "), 
                  "peculiar values were found.", sep = ""
                  ) # more on paste shortly
  output <- list(Range = x_range, Mean = x_mean,
                 Check = x_in_y, Report = report
                 )
  output
}

fun_out <- my.stats(y = 10:12, x = c(5, 10, 7, 2, 21))
fun_out

students <- c(2850, 2284, 1120, 2830, 1993, 1091)
text_part <- "Faculty"; pasted <- 
paste(text_part, 1:length(students), ":", students, sep = " ")
as.matrix(pasted)

some_names <- c("John", "Paul", "George", "Ringo")
paste(some_names, collapse = " and ")

locate.peculiarities <- function(x, y) {
  x_in_y <- any(x %in% y)
  if (!x_in_y) {
    return("No peculiarities were found.")
  } else {
      which(x %in% y)
    } 
}

locate.peculiarities(x = c(3, 1, 13, 14, 6), y = 10:12)
locate.peculiarities(x = c(3, 1, 10, 12, 6), y = 10:12)

iphone.sales <- function(x, ...) {
      new_number <- round(x, ...)
      paste("Apple sold approx. ", new_number,  
            " bn USD in iPhones.", sep = ""
            )
}

iphone.sales(26.418)
iphone.sales(26.418, digits = 1)

my.mean <- function(x) {
  if (any(x < 10)) {
    warning("Some observations are unusually small.")
  }
  mean(x)
}
my.mean(9:30)

hello <- function(x) {
  if (!is.character(x)) {
    stop(paste0(x, " is not a name!"))  
  }
  paste0("Hello, ", x, "!")
}
hello("Florian") # Note how paste0 works.
hello(11)

# Bad example
m<-matrix(1:100,nrow=10);s<-0;for(i in 1:10){
for(j in 1:10){if(m[i,j]%%2==0){s<-s+m[i,j]}}}; s
# Better example
m <- matrix(1:100, nrow = 10)
s <- 0
for (i in 1:10) {
  for(j in 1:10) {
    if (m[i, j] %% 2 == 0) {
      s <- s + m[i, j]  
    }
  }
}
s

# Even better
nat_numbers <- 1:100
position <- nat_numbers %% 2 == 0
sum(nat_numbers[position])

library(RXKCD); getXKCD(1319)
