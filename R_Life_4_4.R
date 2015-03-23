
# Week 4 -- MEDIAN, MAD, AND SPEARMAN CORRELATION

data(ChickWeight)

plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

# In order to easily compare weights at different time points across the different chicks, we will use the reshape() function
# in R to change the dataset from a "long" shape to a "wide" shape. Long data and wide data are useful for different purposes
# (for example, the plotting library ggplot2 and the manipulation library dplyr want to have data in the long format).

head(ChickWeight)

chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")

# The meaning of this line is: reshape the data from long to wide, where the columns Chick and Diet are the ID's and the
# column Time indicates different observations for each ID.

head(chick)

# The only remaining step is that we want to remove any chicks which have missing observations at any time points (NA for
# "not available") . 
chick = na.omit(chick)

# What is the average weight of the day 4 chicks including the outlier chick divided by the average of the weight of the day
# 4 chicks without the outlier. Hint: use c() to add a number to a vector.
newchick <- c(chick$weight.4, 3000)

mean(newchick)/mean(chick$weight.4)

# Or 

mean(c(chick$weight.4, 3000))/mean(chick$weight.4)

# Compute the same ratio but now using median instead of the mean. 
median(c(chick$weight.4, 3000))/median(chick$weight.4)

# What's the standard devition with the outlier chick divided by the standard deviation without the outlier chick?
sd(c(chick$weight.4, 3000))/sd(chick$weight.4)

# Compare this to the median absolute deviation in R, which is calculated with the mad() function. 

1.4826 * median( c(chick$weight.4, 3000) - median(c(chick$weight.4, 3000)) )

# The Pearson correlation between x and y is given in R by cor(x,y). The Spearman correlation is given by 
# cor(x,y,method="spearman").

plot(chick$weight.4, chick$weight.21)

# Calculate the Pearson correlation of the weights of chicks from day 4 and day 21. Now calculate how much the Pearson
# correlation changes if we add a chick that weighs 3000 on day4 and 3000 on day 21. Again, divide the Pearson correlation
# with the outlier chick over the Pearson correlation computed without the outliers.
pearson1 <- cor(chick$weight.4, chick$weight.21)

pearson2 <- cor(c(chick$weight.4, 3000), c(chick$weight.21, 3000))

pearson2 / pearson1

# Note that the Spearman correlation also changes with the addition of this outlier chick, but much less drastically:
# cor(x,y,method="spearman") compared to cor(c(x,3000),c(y,3000),method="spearman") with x and y the vectors of interest.
pearson3 <- cor(chick$weight.4, chick$weight.21, method="spearman")

pearson4 <- cor(c(chick$weight.4, 3000), c(chick$weight.21, 3000), method="spearman")

pearson4 / pearson3


# Week 4 -- MANN-WHITNEY-WILCOXON TEST

data(ChickWeight)
chick <- reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
chick <- na.omit(chick)

# Make a strip chart with horizontal jitter of the chick weights from day 4 over the different diets:

stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)

# Suppose we want to know if diet 4 has a significant impact on chick weight over diet 1 by day 4. It certainly appears so,
# but we can use statistical tests to quantify the probability of seeing such a difference if the different diets had equal
# effect on chick weight.

# Save the weights of the chicks on day 4 from diet 1 as a vector 'x'. Save the weights of the chicks on day 4 from diet 4 as
# a vector 'y'. Now perform a t test comparing x and y (in R the function t.test(x,y) will perform the test). Now, perform a
# Wilcoxon test of x and y (in R the function wilcox.test(x,y) will perform the test). Note that a warning will appear that
# an exact p-value cannot be calculated with ties (so an approximation is used, which is fine for our purposes).
x <- chick$weight.4[1:16]
x

y <-chick$weight.4[37:45]
y

t.test(x, y)

wilcox.test(x,y)

# Now, perform a t-test of x and y, after adding a single chick of weight 200 grams to 'x' (the diet 1 chicks). What is the
# p-value from this test? The p-value of a test is available with the following code: t.test(x,y)$p.value

t.test(c(x,200), y)$p.value

# Or

x = chick$weight.4[chick$Diet == 1]

y = chick$weight.4[chick$Diet == 4]

t.test(c(x, 200), y)$p.value

# Do the same for the Wilcoxon test. Note that the Wilcoxon test is robust to the outlier (and in addition, has less
# assumptions that the t-test on the distribution of the underlying data).

wilcox.test(c(x, 200), y)$p.value

# We will now investigate a possible downside to the Wilcoxon-Mann-Whitney test statistic. Using the following code to make
# three boxplots, showing the true Diet 1 vs 4 weights, and then two altered versions: one with an additional difference of
# 10 grams and one with an additional difference of 100 grams. (Use the x and y as defined above, NOT the ones with the added
# outlier.)

par(mfrow=c(1,3))

boxplot(x,y)

boxplot(x,y+10)

boxplot(x,y+100)

# What is the difference in t-test statistic (the statistic is obtained by t.test(x,y)$statistic) between adding 10 and
# adding 100 to all the values in the group 'y'? So take the the t-test statistic with x and y+10 and substract away the 
# t-test statistic with x and y+100. (The value should be positive).
t.test(x, y+10)$statistic - t.test(x, y+100)$statistic

# Now examine the Wilcoxon test statistic for x and y+10 and for x and y+100. Because the Wilcoxon works on ranks, after the
# groups have complete separation (all points from group 'y' are above all points from group 'x'), the statistic will not
# change, regardless of how large the difference grows. Likewise, the p-value has a minimum value, regardless of how far
# apart the groups are. This means that the Wilcoxon test can be considered less powerful than the t-test in certain 
# contexts, and with small significance levels (alpha). In fact for small sample sizes, the p-value can't be very small, 
# even when the difference is very large. Compare:

wilcox.test(c(1,2,3),c(4,5,6))

wilcox.test(c(1,2,3),c(400,500,600))
