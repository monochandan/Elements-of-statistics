a_num_vector <- 1:10
a_chr_vector <- "A single element"
a_matrix <- matrix(1:6, nrow = 2, ncol = 3)

(list1 <- list(Numeric_vector = a_num_vector,
               Character_vector = a_chr_vector,
               Matrix = a_matrix
               ))

list1[1]
is.list(list1[1])
list1[[1]] # Note the double square bracket
is.list(list1[[1]])
list1[["Numeric_vector"]]
list1$Numeric_vector

names(list1)

list1[1][[1]][1:5] # first element of list with one element
list1[[1]][1:5]
list1$Numeric_vector[1:5]
list1$Matrix[2, 2:3, drop = FALSE]
list1[[c(1, 10)]] # recursive indexing, not recommended

parent_list <- list(Some_list = list1,
                    Another_list = list(A = 10:1,
                                        B = 1:10
                                        )
                    )
grandparent_list <- list(First = parent_list,
                         Second = "Useless_comment"
                         )

str(grandparent_list)

grandparent_list$First$Some_list$Matrix[2, 3]
grandparent_list[[1]][[1]][[3]][2, 3]

list_copy <- grandparent_list
list_copy$Third <- array(8:1, dim = c(2, 2, 2))
list_copy$First <- NULL
str(list_copy)

lapply(grandparent_list$First$Another_list, sum)
sapply(grandparent_list$First$Another_list, sum)

sapply(grandparent_list$First$Another_list, range)

list2 <- list(V1 = rep(1, 10),
              V2 = rep(1:2, 5),
              V3 = rep(1:5, 2)
              )
sapply(list2, unique) # unique values in objects

sapply(list2, function(x) x[1] + x[2] - x[5])

list_a <- list(a1 = c(120, 80, 70, 160, 130),
               a2 = c(1000, 300, 400, 800, 200)
               )
list_b <- list(b1 = 2, b2 = 5)
mapply(function(x, y)
       x[y], # 2nd element of a1, 5th element of a2
       list_a, # x is list_a
       list_b, # y is list_b
       SIMPLIFY = TRUE # avoiding output of list
       )

(df1 <- data.frame(
  Income = c(1300, 3500, 900, 6800, 2700),
  Age = c(21, 30, 16, 70, 35),
  Comment = c("None", "None", "None", "Error?", "None"),
  Imputed = c(rep(FALSE, 4), TRUE)
  ))

str(df1)
df1[3, "Age"]
mean(df1$Income)

subset(df1, Imputed == TRUE)

df1$Age_group <- cut(
  df1$Age, 
  breaks = c(0, 25, 50, 100), 
  labels = c("Young", "Middle", "Old"),
  right = TRUE # intervals closed on the right
  )
df1$Age_group

str(df1)

as.numeric(df1$Age_group)
numbers <- c(3, 10, 1, 3, 7, 7, 5, 1)
a_factor <- factor(numbers)
as.numeric(a_factor) # 3 is 2nd level, 10 is 5th level etc.
as.numeric(as.character(a_factor))

some_codes <- c(3, 2, 3, 3, 1, 1, 2)
factor(some_codes, 
       levels = 1:3, 
       labels = c("Low", "Medium", "High")
       )

head(ToothGrowth) # First six cases

tail(ToothGrowth, 3) # Last three cases
length(ToothGrowth)

proper_list <- split(ToothGrowth, ToothGrowth$supp)
str(proper_list) # Notice the list element names

len
attach(ToothGrowth)
head(len)
len[1] <- 50
head(ToothGrowth, 1); detach(ToothGrowth) # separate calls

with(ToothGrowth, c(max(len), max(dose)))

tg_ordered <- 
  ToothGrowth[order(ToothGrowth$len, ToothGrowth$dose), ]
tg_ordered[14:19, ]

ToothGrowth[ToothGrowth$len %in% c(5.8, 14.5, 27.3), ]
