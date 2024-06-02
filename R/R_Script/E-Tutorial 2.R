1# 10 - Lists----

### How many (list) elements does the provided list contain??----

length(a_list)

### What is the result of the following computation? 8th vector element of 14th list element * 6th vector element of 25th list element - 4th vector element of 2nd list element----

a_list[[14]][8] * a_list[[25]][6] - a_list[[2]][4]

### What is the sum of all numbers in the provided list?----

a <- sapply(a_list, FUN = sum)
a
sum(a)


lapply(a_list, FUN = sum)
mapply(a_list, FUN = sum)
### What is the sum of the elements on the main diagonal of the following matrix (where the number before the comma indicates a list element and the number after the comma indicates a vector element), constructed from the provided list, minus the third row sum of said matrix? Hint: Try to use the function mapply.----

a <- c(a_list[[24]][7], a_list[[24]][5], a_list[[24]][9])
b <- c(a_list[[41]][4], a_list[[41]][5], a_list[[41]][7])
c <- c(a_list[[47]][7], a_list[[47]][10], a_list[[47]][9])

m <- matrix(c(a,b,c), nrow=3)
m

sum(diag(m))
sum(m[3,])
sum(diag(m)) - sum(m[3,])


# 11 - Data Frames----

### How many individuals does the provided data frame cover?----

nrow(a_df)


### How many variables does the provided data frame contain?----

length(a_df)

### How many data types does the provided data frame contain?----

str(a_df)




### What is the income of the 32nd individual with a higher education in the data set (original order of the individuals)?----

(Subset_of_df <- subset(a_df, Education == "Average"))
Subset_of_df[54,2]

### Please first classify the individuals in the data set according to their income as follows:  ----
### 0 < X <= 30,000
### 30,000 < X <= 50,000
### 50,000 < X <= 75,000
### 75,000 < X <= 100,000
### X > 100,000
### How many individuals fall into the second category (more than 30,000 and not more than 50,000)?

asdasd <- 


a_df$YeniGrup <- cut(a_df$Income,
                     breaks = c(0,30000,50000,75000,100000),
                     labels = c("First", "Second", "Third", "Fourth"),
                     right = TRUE # intervals closed on the right
                     )
a_df

subset_of_income <- subset(a_df, a_df$YeniGrup =="Fourth")
subset_of_income
nrow(subset_of_income)    

### What is the maximum of the number of individuals aged 35 within a group?----

Subset_of_35 <- subset(a_df, a_df$Age == 35)
Subset_of_35
nrow(Subset_of_35)
table(Subset_of_35$YeniGrup)

### What is the mode of the new variable you created in subproblem e)? Please state your answer without quotation marks.----

a_df$YeniGrup

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))] 
  }

getmode(a_df$YeniGrup)

str(a_df$YeniGrup)

summary(a_df$YeniGrup)

# 12 - MUltiple Choices----

#FALSE You cannot mix data types in a list.
#TRUE	The output of an extraction of a single list element using double square brackets preserves that elements data structure.
#FALSE 	The output of an extraction of a single list element using single square brackets is not a list.
#FALSE 	You can index matrices using the dollar sign.
#FALSE 	For a list called abc, abc[[c(2, 4)]] extracts the second and fourth list element of abc.
#TRUE	A list element can itself be a list element.
#TRUE	List elements can be deleted by assigning the value NULL to them.
#TRUE	The functions lapply and sapply serve the same purpose and only differ in the structure of their output.
#TRUE	You can use pre-defined functions in lapply by providing their name as the second argument.
#TRUE	A data frame is a list of vectors of the same length, but of potentially differing data types.

# 13 - Packages----

### What is the acronym of the main R repository, where you can find many useful packages?----
#CRAN

### Please install the R package wesanderson. What is the hexadecimal code of the fourth colour in the colour palette named GrandBudapest2?----
#Please give the result without quotation marks and use type = "discrete".

install.packages("wesanderson")
library(wesanderson)

wes_palette("GrandBudapest2", type = c("discrete","continuous"))
wes_palettes$GrandBudapest2[4]


### How many colour palettes are included in the package?----

length(wes_palettes)

### What is the name of the function that is used to specify a survey design object in the R package survey? Hint: Check out the website of the Task View Official Statistics & Survey Methodology. Please state the answer without parentheses.----
#scddesign


