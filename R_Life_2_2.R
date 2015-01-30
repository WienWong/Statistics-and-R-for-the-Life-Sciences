
# HarvardX R Life Week 2

# First, install the gapminder data.
library(devtools)
install_github("jennybc/gapminder")

# Next, load the gapminder data set.
library(gapminder)
data(gapminder)
head(gapminder)

# Create a vector 'x' of the life expectancies of each country for the year 1952. Plot a histogram of these life expectancies
# to see the spread of the different countries.

x <- gapminder[gapminder$year == 1952, ]

plot(x$lifeExp, x$year)

hist(x$lifeExp)

# In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the
# function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.

# We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are
# less than or equal a, divided by the total number of values in x, in other words the proportion of values less than or equal
# to a.

# The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.

# question 1.1 What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
mean(x$lifeExp <= 40)

# What is the proportion of countries in 1952 that have a life expectancy between 40 & 60 years? Hint: this is the proportion
# that have a life expectancy less than or equal to 60 years, minus the proportion that have a life expectancy less than or # equal to 40 years.
mean(x$lifeExp <= 60) - mean(x$lifeExp <= 40)
