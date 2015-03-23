
# Week 4 -- Inference III  MONTE CARLO SIMULATION   Exercise 

babies = read.table("babies.txt", header=TRUE)

# The population of nonsmoker baby weights is:
bwt.nonsmoke = babies$bwt[babies$smoke==0]

# And the population variance is 302.7144:
pop.var = var(bwt.nonsmoke)

# Use replicate() to perform Monte Carlo simulations. So the following line of example code will repeat the R expression,
# "xyz" 1000 times
vars = replicate(1000, xyz)

# Using Monte Carlo simulation, take 1000 samples of size 10 from bwt.nonsmoke and calculate the variance. 
# Look at a histogram of vars, 

set.seed(1)

smokers <- sample(babies$bwt[babies$smoke==1],10)

nonsmokers <- sample(babies$bwt[babies$smoke==0],10)


vargenerator <- function(n){ 
    nonsmokers = sample(babies$bwt[babies$smoke==0], n) 
    return(var(nonsmokers)) 
} 

vars <- replicate(1000, vargenerator(10))

hist(vars) 

abline(v=pop.var)

mean(vars)

qqnorm(vars)

table(vars > pop.var*1.5)[2] / (table(vars > pop.var*1.5)[1] + table(vars > pop.var*1.5)[2])
# 0.176 

# Or come from the solution
vars = replicate(1000, var(sample(bwt.nonsmoke, 10)))

hist(vars, breaks=100)

abline(v=pop.var, col="blue")

mean(vars > pop.var * 1.5)


# Now use a sample size of 50. How often (what proportion) is the sample variance larger than 1.5 times the population
# variance?

vars = replicate(1000, var(sample(bwt.nonsmoke, 50)))

hist(vars, breaks=100)

abline(v=pop.var, col="blue")

mean(vars > pop.var * 1.5)


# Finally, we'll make a plot to see how the sample variance estimates gets better (closer to the population variance) as the
# sample size increases. First, we'll make a vector of sample sizes from 2 to 400:

sample.size = 2:400

# Now, for each sample size, take a sample from the nonsmokers of that size, and calculate the variance:
    
var.estimate = sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))

# Finally, plot these sample variances over their sample sizes, and draw a horizontal line of the population variance:
    
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")

