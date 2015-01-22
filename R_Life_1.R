
tab = read.csv("msleep_ggplot2.csv")

class(tab)

head(tab)

dim(tab)

colnames(tab)

tab$sleep_total

# add a final number, 1000, to the sleep totals:
c(tab$sleep_total, 1000)

plot(tab$brainwt, tab$sleep_total)

# with a logarithmic scale x-axis:
plot(tab$brainwt, tab$sleep_total, log="x")

# The summary function gives the summary statistics of a set of values.
summary(tab$sleep_total)

#Subsetting a dataframe to the first two rows:
tab[ c(1,2), ]

# The rows where the total sleep is greater than 18 hours:
tab[ tab$sleep_total > 18, ]

# Subsetting a vector looks very similar, but we just remove the comma (because there are no columns now). The first two elements can be subset like so:
tab$sleep_total[ c(1,2) ]

# What is the average total sleep, using the function mean() and vector subsetting, for the animals with total sleep greater than 18 hours?
tab$sleep_total[ tab$sleep_total > 18 ]

mean( tab$sleep_total[ tab$sleep_total > 18 ] )

# The function which() gives us the numeric index that satisfies a logical question:
which(tab$sleep_total > 18)

# Get the first value where the total sleep was more than 18 hours. 
tab$sleep_total[ which(tab$sleep_total > 18)[1] ]

# What is the row number of the animal which has more than 18 hours of total sleep and less than 3 hours of REM sleep?
which( tab$sleep_total > 18 & tab$sleep_rem < 3 ) 

# sort() simply gives back the list of numeric values after sorting them
sort(tab$sleep_total)

# order() gives back the index, in the original vector, of the smallest value, then the next smallest
order(tab$sleep_total)

# check what happens if you use order() as an index to the original vector
tab$sleep_total[ order(tab$sleep_total) ]

# rank() takes in numeric values, and turns the smallest value into 1, the second smallest value into 2, etc., and returns
# the ranks in the same order as the input vector. Ties are resolved by giving the numbers the average of their ranks.
rank(c(1,2,2,3))

# What's the rank of the animal in the first row of the table in terms of total sleep?
rank( tab$sleep_total )

# The match() function in R is useful to find the index of the first match of a vector in a second vector. We can give match
# () a number of queries at once:
match( c("Cow","Owl monkey","Cheetah"), tab$name )

# This can then be used to rearrange an object. For example, we can rearrange the tab dataframe to an order we specify. Let's
# reorder the tab dataframe to give the rows for Cow, Owl monkey and Cheetah, using the vector returned by match() as a row
# index:
idx = match( c("Cow","Owl monkey","Cheetah"), tab$name )

tab[idx, ]

# What is the row number for "Cotton rat" in the tab dataframe?
match ( "Cotton rat", tab$name )

# Factors in R are a way to turn character vectors with repeating values into a class of object that recognizes the repeated
# values. What R is doing internally is keeping track of character values using integers, where the integer refers to the
# unique values, or "levels" of the factor. But R shows you the character vector when you print the factor. 
# 
# The levels of a factor and their order can be seen using the levels() function. 
vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)

# You would use character vectors to test for a match, for example, with a factor "vec", if you wanted to find matches for
# the level "blue", you would write:
vec == "blue"

# The levels are chosen alphabetically, unless we say otherwise:
fac2 = factor(vec, levels=c("blue","green","yellow","orange","red"))
fac2
levels(fac2)

# Some of the columns of our mammal sleep data are factors, and we can use functions like table() on these columns, in order
# to count the number of repetitions.
# 
# How many rodents (Rodentia) are in the table?
table(tab$order)

# split() is a function which takes a vector and splits it into a list, by grouping the vector according to a factor. Let's
# use our mammal sleep data again to try this out. Split the total sleep column by the mammals Order (here Order means the
# biological taxonomy, above Family and below Class)

s = split(tab$sleep_total, tab$order)
s

# We can pull out a single vector from the list using the name of the Order or the number that it occurs in the list (note: this is where the level occurs in the levels of the factor).
# 
# Lists are indexed with double square brackets [[]], instead of a single square bracket []:
s[[17]]

s[["Rodentia"]]

# What is the mean hours of total sleep of the rodents?
mean(s[["Rodentia"]])

# lapply() and sapply() are useful functions for applying a function repeatedly to a vector or list. lapply() returns a list,
# while sapply() tries to "simplify", returning a vector if possible (if there is only one element returned by the function
# for each element of the input. Let's use lapply() to get the average total sleep for each Order:

lapply(s, mean)

# As you can see, a list is returned. Let's use sapply() instead:
sapply(s, mean)

# A shortcut for using split() and sapply() is to straightaway use tapply(), providing these 3 arguments: the vector of
# values, a factor to group by, and the function to apply:
tapply(tab$sleep_total, tab$order, mean)

# Use any one of lapply(), sapply(), or tapply() to answer the following question:
# What is the standard deviation of total hours of sleep for the Primates Order? (The standard deviation function is sd() in R.)
tapply(tab$sleep_total, tab$order, sd)

lapply(s, sd)

sapply(s, sd) 
