##factor
##install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
blood <- c("AB","B","A","B","O","A","O","B") ## categorical variable
blood

blood_factor <- factor(blood)
## showing the categories  by factorization and
## sorting levels alphabetically
blood_factor



## converting character vector into vector of integer values
## each integer represent the categories or levels
## in this case
## for A -> 1
## for AB -> 2
## for B -> 3
## for O -> 4
str(blood_factor)

## order levels differently
blood_factor_2 <- factor(blood,levels = c("O","B","AB","A"))
blood_factor_2
str(blood_factor_2)

## Rename factor levels
blood_1 <- c("AB","B","A","B","O","A","O","B") ##vector declaration
blood_1_factor <- factor(blood_1) ##vector to factor

## renaming the levels
### warning !!!! order should not be changed 
blood_1_factor ## A AB B O
levels(blood_1_factor) <- c("BT_A", "BT_AB", "BT_B", "BT_O")

blood_1_factor ## BT_A BT_AB BT_B BT_O

blood_1
### factorizing the vector (blood_1) with the  new labels
blood_1_factor_3 <- factor(blood_1, labels = c("BT_A", "BT_AB", "BT_B", "BT_O"))
blood_1_factor_3


##### avoiding the order change
### we can add the levels too

blood_1_factor_4 <- factor(blood_1,levels = c("O","B","AB","A"), labels=c("BT_O", "BT_B", "BT_AB", "BT_A"))
blood_1_factor_4

blood_1_factor_4[1] > blood_1_factor_4[2]

class(blood_1) ## "character"
class(blood_1_factor) ## "factor"


t_shirt <- c("S","M","L","S","L","L","M","S")
t_shirt
shirt_factor <- factor(t_shirt,ordered = TRUE, levels =c("S","M","L"))
shirt_factor

shirt_factor[1]
shirt_factor[2]
shirt_factor[3]
shirt_factor[1] < shirt_factor[2]

city <- read.csv(file= 'C:/Users/looka/OneDrive/Documents/EOS/city.csv')
class(city[1])

names(city)
colnames(city)

colnames(city[1])
sapply(city, class)
str(city)


vc <- c("Marry","Anna")
vc
names(vc) = c("First","Last")

vc["First"]
vc
vc[c("First","Last")]

x = 2
class(x)
a = as.integer(10)
class(a)

i  =10L
class(i)
i
y = "5.24"
as.integer(y)

as.character(x)
c1  = "Joe"
c2 = "Dam"
paste(c1,c2)# concate

sprintf("%s has %d boll","Peter",1)
substr("Many is everything", start= 1, stop= 7)

sub("Marry","Jerimy","Marry is handsome")