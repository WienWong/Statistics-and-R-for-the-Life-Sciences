

# HarvardX R Life Week 2

dat <- read.csv("femaleMiceWeights.csv")

dat[1:12, 2]

# The observed difference between high fat diet and control was calculated like so:
mean(dat[13:24, 2]) - mean(dat[1:12, 2])

# split() is a function which takes a vector and splits it into a list, by grouping the vector according to a factor.
# Let's make a plot of these two groups: a strip chart of the weights.
s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)

# Add the means to the plot as well:
abline(h=sapply(s, mean), col=1:2)

##########

# How many of the high fat mice weigh less than the mean of the control mice (chow)? You can either see from the plot, or 
sum(dat[13:24,2] < mean(dat[1:12,2]))

# How many of the control mice weigh more than the mean of the high fat mice?
# You can either see from the plot, or 
sum(mean(dat[13:24,2]) < dat[1:12,2])

# Continuing from the code above where we split the weights by diet, s[["hf"]] or equivalently s$hf gives the weights for 
# the high fat diet mice. Save that to a new vector 'highfat':
highfat = s[["hf"]]

# Print the values for highfat:
highfat

# Use the sample() function in R to generate random samples of a population.
sample(highfat, 6)

# Argument of 'replace' which toggles whether or not an observation can be chosen more than once (whether to replace the
# observations back in the population after they are chosen once). The default setting for 'replace' is set to FALSE, so 
# if we don't say anything, the sample() function will not choose an observation more than once.
sample(highfat, 6, replace=FALSE)

# You should notice that some of the time, there are repeated values in the sample. This is because we changed the setting 
# of 'replace' to allow for multiple draws of the same obseration.
sample(highfat, 6, replace=TRUE)

# to use this logical vector directly in functions which usually work on numeric values. 
highfat > 30

# What happens inside of the R function is that the TRUE is turned into a 1, and the FALSE is turned into a 0. This happens
# automatically, so you don't need to convert the vector type yourself:
as.numeric(highfat > 30)

# if we want to know the number of the high fat diet mice that weigh over 30:
sum(highfat > 30)

# The proportion of high fat diet mice over 30 is the sum of high fat diet mice over 30 divided by the number of high fat
# diet mice, in other words, the mean of a vector of 1s and 0s. What is the proportion of high fat diet mice over 30?
sum(highfat > 30) / length(highfat)

mean(highfat > 30)

##########

# Read a new csv file
population <- read.csv("femaleControlsPopulation.csv")

# What would happen if we received 24 new random mice and we split them in half,
# gave them both the control diet, and then see what values we get in the difference and mean.
control <- sample(population[, 1], 12)
mean(control)
# This mean is a random variable if repeat above 2 lines.

n <- 1000
# To use the vector function to create an empty vector
null <- vector("numeric", n)
for(i in 1:n){
    control <- sample(population[, 1], 12) # get a random set of mice.
    treatment <- sample(population[, 1], 12) # get another set of mice.
    null[i] <- mean(treatment) - mean(control)
}
# So we have 10,000 numbers that the differences that happen just by chance.

# The original difference we saw above.
diff <- mean(dat[13:24, 2] - mean(dat[1:12, 2]))

# To count how many of these null values are bigger than this difference.
mean(null > diff) # a convinient trick here


#computed a p-value. This is the probability of seeing a difference as big as the one we saw in our experiment when the 
#null hypothesis is true.

##########

# load the population data for the control mice 
population <- read.csv("femaleControlsPopulation.csv")

# Because this is just one column of data, as you can see with head(), let's just extract the one column as a vector:
population <- population[,1]

# the control population mean
mean(population)

# make a random sample using the sample() function:
sample(population, 12)

# calculate the mean for a sample in the normal way:
mean(sample(population, 12))

# The replicate() function takes two arguments, which are the number of times to replicate, and then an expression: the
# command you want to replicate. First let's try out making a random sample of one group of 12 mice from the population 
# and calculating the mean:
sampleMean = replicate(10000, mean(sample(population, 12)) )
head(sampleMean)

# plot for the mean of 12 random mice:
plot(sampleMean)

# We can also use replicate() to perform the same operation done in the video: calculating the difference between two 
# random samples of 12 from the control mice:

null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)) )

# Take a look at a few of these 10,000 differences, and plot them along the x-axis

head(null)
plot(null)

##########

# INTRODUCTION TO NULL DISTRIBUTIONS
population <- read.csv("femaleControlsPopulation.csv")

n <- 100
library(rafalib)  # I don't have this library so run the next chunk of code after '###'
mypar(1,1)
plot(0,0,xlim=c(-5,5),ylim=c(1,30),type='n')
totals <- vector("numeric", 11)
for(i in 1:n){
    control <- sample(population[,1],12)
    treatment <- sample(population[,1],12)
    nulldiff <- mean(treatment) - mean(control)
    j <- pmax(pmin(round(nulldiff)+6, 11), 1)
    totals[j] <- totals[j] + 1
    text(j-6, totals[j], pch=15, round(nulldiff,1), cex=0.75)
    if(i < 15) scan()
}

###
population <- read.csv("femaleControlsPopulation.csv")

n <- 100
plot(0,0,xlim=c(-5,5),ylim=c(1,30),type="n")
totals <- vector("numeric",11)
for(i in 1:n){
    control <- sample(population[,1],12)
    treatment <- sample(population[,1],12)
    nulldiff <- mean(treatment) - mean(control)
    j <- pmax(pmin(round(nulldiff)+6,11),1)
    totals[j]<-totals[j]+1
    text(j-6,totals[j],pch=15,round(nulldiff,1))
    ##if(i < 15) scan() ##You can add this line to interactively see values appear
}


##########

population <- read.csv("femaleControlsPopulation.csv")

population <- population[,1]

# In the previous assessments, we created a vector of differences between means of random samples from the control 
# population. This gives us a sense of the null distribution of differences if there is no true effect of a high fat 
# diet. Let's recreate that vector:
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)) )

# The simple visualization of stacking the values which are close, in order to see the spread, is incredibly useful. 
hist(null)

# Let's return to the original difference we observed between the mice fed high fat diets and control mice:

diff = mean(dat[13:24,2]) - mean(dat[1:12,2])

# Now what do we see when we add this difference to the histogram:

abline(v=diff, col="red")

# If we look for the number of null distribution values to the right of the red line, we would say "we calculated the
# probability of observing a larger difference from the null distribution". This is sometimes called a "one-tailed" 
# probability, because we only look at one "tail" of the histogram (the left and right sides where the bars become short). 

# We can also add the negative of the difference:
abline(v=-diff, col="red")

# By looking at the tails on both sides of the histogram, we can say "we calculated the probability of observing as extreme
# a difference from the null distribution". This is sometimes called a "two-tailed" probability. And as shown the video, 
# this probability is commonly referred to as a p-value.

# What is the one-tailed probability of seeing as big a difference as we observed, calculated from your null distribution?

1 - sum(null < diff) / length(null)

mean(null > abs(diff)) 

# What is the two-tailed probability of seeing as big a difference as we observed, calculated from your null distribution?
mean(null > abs(diff)) + mean(null < -diff) 

mean(abs(null) > abs(diff))
