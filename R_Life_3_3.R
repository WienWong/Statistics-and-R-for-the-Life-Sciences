
# HarvardX R Life Week 3-3   CONFIDENCE INTERVALS

dat <- read.csv("mice_pheno.csv")

pop <- dat[ dat$Sex=="F" & dat$Diet=="chow", 3]

mu <- mean(pop)

mu

N <- 30

y <- sample(pop, N)

mean(y)

# We report an interval that we are fairly certain is going to fall on top of the real mean. The mu is the real mean.

se <- sd(y)/sqrt(N)

se

# CONFIDENCE INTERVALS II

# I have the random variable mean of y. And I want to somehow create an interval that has a high probability of falling on
# top of the true population mean, which is mean pop.

# If I pick Qs such that the probability that a random variable fall in between Q and minus Q is 0.95, then I have what I
# wanted.
Q <- qnorm(1 - 0.05/2)

# This random variable is a normal that is approximately normal with mean 0 and standard deviation 1,
( mean(pop) - mean(y) ) / se

# So we know that 95% of the time, a normal random variable falls between 1.96 and negative 1.96.
# -Q < ( mean(pop) - mean(y) ) / se < Q 

# mean(y) - Q*se < mean(pop) < mean(y) + Q*se

# Thus the interval is
interval <- c(mean(y) - Q*se, mean(y) + Q*se)
interval

# Make a plot where I have the center at mu. I'm going to add minus 7 and 7 to both sides. How do I pick that 7? I look at
# the interval and notice the size is about 3, so I'm going to make it extra big by adding 7 on both sides.
plot( mu + c(-7, 7), c(1, 1), type="n", xlab="weights", 
      ylab="intervals", ylim=c(1, 100)) # 100 intervals
abline(v=mean(pop))

# We already defined intervals, and now I'm going to add a line at interval,c(1,1). And let's see if it falls on top of our
# vertical line.
lines(interval, c(1, 1))

# Keep in mind, confidence intervals are random. The beginning is random, the end is random, they are based on the sample
# mean. Thus, every time we take a new sample, the confidence interval is going to change.

for(i in 2:100){
    y <- sample(pop, N)
    se <- sd(y)/sqrt(N)
    interval <- c(mean(y)-Q*se, mean(y)+Q*se)
    # I want the color to be different depending if interval is covering the population mean or not. And we'll use an ifelse
    # to give it either a 1 or a 2
    color <- ifelse(interval[1] <= mean(pop) & interval[2] >= mean(pop), 1,2)
    lines(interval, c(i, i), col=color)    
}

# So we see 100 intervals. Three of them did not fall on the population mean, it didn't cover, and that is what we expect. We
# expect about 5% not to fall on top of the population mean.
 

# CONFIDENCE INTERVALS III
 
# The code shows how we run the t statistic,
N <- 5
Q <- qt(1 - 0.05/2, 5-1)

interval <- c(mean(y) - Q*se, mean(y) + Q*se)
interval

plot( mu + c(-7, 7), c(1, 1), type="n", xlab="weights", ylab="intervals", ylim=c(1, 100)) 
abline(v=mean(pop))

for(i in 1:100){
    y <- sample(pop, N)
    se <- sd(y)/sqrt(N)
    interval <- c(mean(y)-Q*se, mean(y)+Q*se)
    color <- ifelse(interval[1] <= mean(pop) & interval[2] >= mean(pop), 1,2)
    lines(interval, c(i, i), col=color)    
}

#
dat <- read.csv("femaleMiceWeights.csv")

# First 12 were the controls, and the second 12 were the treatment.
t.test( dat[13:24, 2], dat[1:12, 2])

# Notice that for this particular case, we're doing a t statistic. So we're testing to see if there's a difference in the
# mean of these two populations.

# So the null hypothesis is that the difference of the means is 0. If the null hypothesis is true, they should fall on top of
# 0, 95% of the time. And we see that in this case, it does.

data:  dat[13:24, 2] and dat[1:12, 2]
t = 2.0552, df = 20.236, p-value = 0.053
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
    -0.04296563  6.08463229
sample estimates:
    mean of x mean of y 
26.83417  23.81333 

# So that guarantees, if we have a 95% confidence interval that includes 0 for a t statistic that's testing for that null
# hypothesis -- that the mean is 0 -- if the confidence interval includes 0, that means the p-value must be bigger than 0.05.
# If 0 was not included here, then we would get a p-value that's less than 0.05.
