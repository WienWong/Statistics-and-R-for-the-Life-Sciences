
# Week 4-1 

# HISTOGRAM

library(UsingR)

x <- father.son$fheight
length(x)

round(sample(x, 20), 1)

bins <- seq(floor(min(x)),ceiling(max(x)))
hist(x, breaks=bins, main="Height histogram", xlab="Height in inches" )

# Although not as popular as the histogram for EDA, the empirical cumulative density function (CDF) shows us the same
# information and does not require us to define bins. For any number $x$ the empirical CDF reports the proportion of numbers
# in our list smaller or equal to $x$. R provides a function that has as out the empirical CDF function. Here:

myCDF <- ecdf(x) 

xs <- seq( floor(min(x)), ceiling(max(x)), 0.1 )
plot(xs, myCDF(xs), type="l", xlab="Height in inches", ylab="F(x)")


# QQ-PLOT
mean(x)

sd(x)

mean(x>70)

1 - pnorm(70, mean(x), sd(x))

mean(x<70)

pnorm(70, mean(x), sd(x))

normalqs <- qnorm(ps, mean(x), sd(x))

ps <- seq(0.01, 0.99, 0.01)

qs <- quantile(x, ps)

normalqs <- qnorm(ps, mean(x), sd(x))

plot(normalqs, qs, xlab="Normal percentiles", ylab="Height")
# We have the observed percentiles and the percentiles predicted by the norm.

abline(0, 1)

qqnorm(x)
qqline(x)

# QQ-PLOT Exercise

load("skew.RData")

dim(dat)

# Using QQ-plots, compare the distribution of each column of the matrix to a normal. That is, use qqnorm() on each column. To
# accomplish this quickly, you can use the following line of code to set up a grid for 3x3=9 plots. ("mfrow" means we want a
# multifigure grid filled in row-by-row. Another choice is mfcol.)
colnames(dat)<-c("1","2","3","4","5","6","7","8","9")
par(mfrow = c(3,3))
for (i in 1:9) {
    qqnorm(dat[,i],main=paste("Column",colnames(dat)[i]))
    qqline(dat[,i])
}

# Error in plot.new() : figure margins too large
# Thus, modified to blew chunks of code:

par(mfrow = c(2,2))
for (i in 1:4) {
    qqnorm(dat[,i], main=paste("Column",colnames(dat)[i]))
    qqline(dat[,i])
}

par(mfrow = c(2,2))
for (i in 5:8) {
    qqnorm(dat[,i],main=paste("Column",colnames(dat)[i]))
    qqline(dat[,i])
}

par(mfrow = c(2,2))
for (i in 6:9) {
    qqnorm(dat[,i],main=paste("Column",colnames(dat)[i]))
    qqline(dat[,i])
}

# Which column has positive skew (a long tail to the right)?
par(mfrow = c(1, 2))
hist(dat[, 4])

# Which column has negative skew (a long tail to the left)?
hist(dat[,9])

# BOXPLOT

library(UsingR)

data(exec.pay)

hist(exec.pay)

qqnorm(exec.pay)

qqline(exec.pay)

boxplot(exec.pay, ylab="10000s of dollars", ylim=c(0, 400))

# BOXPLOT exercise

head(InsectSprays)

boxplot(split(values, factor))

x <-split(InsectSprays, f=InsectSprays$spray)
class(x[[1]])
# "data.frame"

# Instead of that you should specify the column you want to split. Because you want to split count column you should wrote
# code below
x <- split(InsectSprays$count, InsectSprays$spray)
boxplot(x)

# because now the every element of a list is a class numeric. Try
class(x[[1]])
# "numeric"

# Which spray seems the most effective (has the lowest median)?
boxplot(InsectSprays$count ~ InsectSprays$spray)

# When is it appropirate to use a barplot

A. To compare percentages that add up to 100% 
B. To display data from different groups: show the mean of each group 
C. To illustrate data that resulted in a p-value by adding an antenna at the top 
D. To summarize the relationship between two variables

Barplots are much better than piecharts for displaying percentages. For the 2nd and 3rd answers we would use either boxplots
or stripcharts while for the 4th we would use a scatter plot.
