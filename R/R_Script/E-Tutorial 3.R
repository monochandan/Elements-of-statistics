# 14 Base plots I----

### Please match the figures (on the right) to the function calls (on the left) by drawing the boxes of the former into the boxes of the latter. (5 points per correct match)----

plot(1:10, 1:10, xlab = "", ylab = "")
plot(1:10, 1:10, type = "b", xlab = "", ylab = "")
plot(1:10, 1:10, pch = 5, xlab = "", ylab = "")
plot(1:10, 1:10, cex = 5, xlab = "", ylab = "")




# 15 - Colours----
### What is the name of the 530th of R's built-in colours? Please state your answer without quotation marks.----

colours()
colours()[530]

### What is the intensity of the blue channel of the colour named seashell? Please state an integer.----

col2rgb("seashell")




# 16 - Histograms and box plots----

### Please plot a histogram of the variable Income found in the provided data frame using 20 automatically-constructed bins. ----
##What is the upper limit of the penultimate (from lowest incomes to highest incomes) bin?

a_hist <- hist(a_df[,"Income"], breaks = 20)
a_breaks <- a_hist$breaks
a_density <- a_hist$density

a_breaks[length(a_breaks)-1]


### How many incomes lie within the second bin?----

a_hist$counts

### The areas of the bins in a regular histogram sum to..----

sum((a_breaks[-1] - a_breaks[-length(a_breaks)]) * a_density)

### What does the following function call do? ----

boxplot(Income ~ Education, data = a_df)





# 17 - Base plots II----

###Please match the figures (on the right) to the function calls (on the left) by drawing the boxes of the former into the boxes of the latter. (5 points per correct match)----

plot(1:10, 10:1, xlab = "", ylab = ""); abline(v = 5)
plot(1:10, 10:1, xlab = "", ylab = ""); abline(h = 5)
plot(1:10, 10:1, xlab = "", ylab = ""); points(10, 10)
plot(1:10, 10:1, xlab = "", ylab = ""); lines(c(2, 8), y = c(2, 8))



# 18 - Multiple choice III----



#TRUE	Generic functions submit the function input to specific function variants capable of dealing with the specific data at hand.
#FALSE	Low-level plotting is the first task performed when drawing a plot.
#TRUE		dev.off() closes a graphics device and stops plotting to a file.
#FALSE	The argument bg to the plot function can be used with every plotting character/symbol.
#FALSE	To draw a (non-stacked) barplot including different groups, we can use the argument names.arg.
#FALSE	apply(objectname, 2, sum) and rowSums(objectname) yield the same output.
#FALSE	par(mfrow = c(1, 3)) yields a grid of three vertically arranged plots.
#FALSE	You can "erase" single low-level plotting calls.
#TRUE		Using xyplot(Y ~ X | Z, data = objectname) constructs a faceted graph with as many scatterplots as categories in Z in lattice.
#TRUE		Plotting data and plot characteristics are usually separately defined in ggplot2.




# 19 - Multiple choice IV----



#TRUE	ifelse is a vectorised variant of the combination of if and else.
#FALSE	A for loop repeats a task as long as a condition is met.
#FALSE	You cannot use the loop index itself within a loop.
#FALSE	A for loop will usually be faster than a call of apply.
#FALSE	tapply works like lapply but transposes the output.
#FALSE	Binary operators are no functions.
#TRUE	You can set default values in your own functions.
#FALSE	The preferred data structure for function output is an array.
#FALSE	paste only takes character vectors as arguments to be pasted.
#FALSE	If a warning is triggered, function execution stops.






# 20 - Loops etc. I----

###What is the output of the 9th iteration of the following loop? Please mind the spaces.----

loop_ind <- 1:20

for (i in loop_ind) {
  
  cat(i * 7, "divided by 7 is", i, ".\n")
  
}

###What is the difference between the mean of incomes of non-married individuals with a higher level of education and----
#the mean of incomes of married individuals with a lower level of education in the provided data frame?

subset_a <-  subset(a_df, a_df$Education == "Higher" & a_df$Married == "FALSE" )
subset_b <- subset(a_df,a_df$Education == "Lower" & a_df$Married == "TRUE" )

mean(subset_a$Income) - mean(subset_b$Income)


###We want to approximate the number 26. The initial value of our approximation is 1. In every iteration, we add 1% of the current approximation to it. We stop the approximation when the difference between the target and the current approximation is less than 0.01.----
#What is our final approximation?

Target <- 26
Current <- 1 
Iteration <- 0

while (Current < Target) {
  
  if(Target - Current > 0.01) {
    Current = Current * 1.01
    Iteration = Iteration + 1
    }
     
  else 
        break
  
} 

print(Current)
print(Iteration)



####How many iterations do we need for the approximation in subproblem c), where the initial approximation of 1 does not count as an iteration?----

print(Iteration)

