

# HarvardX R Life Week 3-1

getwd()

setwd("C:/....../Documents/HarvardX_R_Life_1")

dat <- read.csv("femaleMiceWeights.csv")

dat[1:12, 2]

# The observed difference between high fat diet and control was calculated:
mean(dat[13:24, 2]) - mean(dat[1:12, 2])

population <- read.csv("femaleControlsPopulation.csv")

control <- sample(population[, 1], 12)
mean(control)

n <- 10000
# To use the vector function to create an empty vector
null <- vector("numeric", n)

for(i in 1:n){
    control <- sample(population[, 1], 12)   # get a random set of mice.
    treatment <- sample(population[, 1], 12) # get another set of mice.
    null[i] <- mean(treatment) - mean(control)
}

diff <- mean(dat[13:24, 2] - mean(dat[1:12, 2]))

mean(null > diff) 

hist(null)

qqnorm(null)
qqline(null)

####

pops <- read.csv("mice_pheno.csv")

head(pops)

# To use the females, and the third column that has the body weights.
hf <- pops[pops$Diet=="hf" & pops$Sex=="F", 3]

hist(hf)

chow <- pops[pops$Diet=="chow" & pops$Sex=="F", 3]
hist(chow)

# Diff in mean btw the high fat diet pop & the mean control or chow
mean(hf) - mean(chow)
# 2.375517
# It's about 10% of their body weight, so it should be a noticeable difference.

x <- sample(hf, 12)
y <- sample(chow, 12)

mean(x) - mean(y)
# 2.004167
# We can do it over and over again and we're going to get different values. This is a random variable, and what the central
# limit theorem does is that it gives us an approximation of the distribution of this random variable.

####

# sample sizes
Ns <- c(3,5,10,25)
B <- 10000

# for each of the entries of Ns,
res <- sapply(Ns, function(n){
    sapply(1:B, function(j){
        mean(sample(hf, n)) - mean(sample(chow, n))
    })
})

# look at the distribution of these random variables
library(rafalib)
mypar2(2,2)
for(i in seq(along=Ns)){
    title <- paste("Avg=", signif(mean(res[,i]), 3))
    qqnorm(res[, i], main=title)
    qqline(res[, i])
}
