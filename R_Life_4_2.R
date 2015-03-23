
# # Week 4-1 

# SCATTERPLOT
library(UsingR)
data(father.son)
x = father.son$fheight
y = father.son$sheight
plot(x, y, xlab="Father's height in inches", ylab="Son's height in inches",
    main=paste("correlation = ", signif(cor(x,y), 2)))
# To compute the correlation, we simply standardize the units and then multiply them together and take the mean of that. It
# gives us 0.5.
boxplot(split(y, round(x)))
print(mean(y[ round(x) == 72]))

x=(x - mean(x))/sd(x)
y=(y - mean(y))/sd(y)

means=tapply(y, round(x*4)/4, mean)
fatherheights=as.numeric(names(means))

plot(fatherheights, means, ylab="average of strata of son heights", ylim=range(fatherheights))

abline(0, cor(x, y))

set.seed(1)

a=rnorm(100); a[1]=25
b=rnorm(100); b[1]=26
plot(a, b, main=paste("correlation=", signif(cor(a, b), 2)))

# SCATTERPLOT exercise

library(UsingR)
data(father.son)

plot(father.son$fheight, father.son$sheight)

# And calculate the correlation between these two vectors:
cor(father.son$fheight, father.son$sheight)

# A useful trick for scatterplots in R is the identify() function. This let's you click on points in the plot, and then once
# you hit the ESC key, the row number of the point(s) you clicked will be printed in the R console and on the figure:

identify(father.son$fheight, father.son$sheight)

# We saw that the correlation is related to the scaled vectors seen in the scatterplot. Let's try this with the father's and
# son's heights. Set the following variables (to save yourself keystrokes):

x = father.son$fheight
y = father.son$sheight
n = nrow(father.son)

# The scale() function subtracts the mean and divides by the standard deviation. Make a scatterplot of scale(x) and scale(y).

plot(scale(x), scale(y))

# Add horizontal and vertical lines with:

abline(h=0, v=0)

# Now consider the number of points in the four quadrants of the figure. The correlation (or "Pearson correlation") is nearly
# the same as multiplying the scaled x values with the scaled y values, and taking the average. If the points fall on a
# diagonal line pointing up and to the right, then the points are mostly in the +/+ quadrant and the -/- quadrant. So both of
# these quadrants contribute positive values to the average. If the points falls on a diagonal line to down and to the right,
# then we have mostly +/- and -/+, contributing negative values to the average.

# Calculate the average of (scaled x values times scaled y values)
mean(scale(x)*scale(y))

# Note that this value above is not exactly the same as:
cor(x,y)

# This is because the standard deviation has in its formula (n-1) while the Pearson correlation instead uses (n). Check for
# yourself:
sum(scale(x) * scale(y)) / (n - 1)


# SYMMETRY OF LOG RATIOS


# EDA Accessment

library(UsingR)
data(nym.2002)

head(nym.2002)

hist(nym.2002$time)

plot(nym.2002$time, nym.2002$age)

plot(nym.2002$time, nym.2002$place)

qqnorm(nym.2002$time)
qqline(nym.2002$time)


tail(sort(table(nym.2002$home)),10)

barplot(nym.2002$time, nym.2002$home=="NY")


boxplot(nym.2002$gender, nym.2002$time)

# Create a vector time of the sorted times:
time = sort(nym.2002$time)

# What is the fastest time divided the median time?
min(time)/median(time)

# What is the slowest time divided the median time?
max(time)/median(time)

# 1) A plot of the ratio of times to the median time, with horizontal lines at twice as fast as the median time, and twice as
# slow as the median time.

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

# 2) A plot of the log2 ratio of times to the median time. The horizontal lines indicate the same as above: twice as fast and
# twice as slow.

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

# Note that the lines are equally spaced in Figure #2.



# Quoting from the R help page for the function pie: "Pie charts are a very bad way of displaying information. The eye is
# good at judging linear measures and bad at judging relative areas. A bar chart or dot chart is a preferable way of
# displaying this type of data."
