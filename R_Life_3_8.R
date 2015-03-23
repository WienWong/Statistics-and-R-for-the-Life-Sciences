
# Week 4 -- Inference III  MONTE CARLO SIMULATION

# We would take sample size of 10-- 10 from smokers and 10 from nonsmokers, and create a T-statistic.
dat <- read.table("babies.txt",header=TRUE)

# Random numbers that I generate are exactly the same if I redo this code later. This is a useful little command to use when
# you want, for example, to create a reproducible results where you include Monte Carlo simulation.
set.seed(1)

smokers <- sample(dat$bwt[dat$smoke==1],10)

nonsmokers <- sample(dat$bwt[dat$smoke==0],10)

cat("observed difference =", mean(smokers) - mean(nonsmokers))

# The value are all negative, which is something we expect, because as we said earlier, the babies from smokers tend to weigh
# less.
for(i in 1:10) {
    smokers <- sample(dat$bwt[dat$smoke==1],10)
    nonsmokers <- sample(dat$bwt[dat$smoke==0],10)
    cat("observed difference = ",mean(smokers)-mean(nonsmokers),"ounces\n")
}

# Let's take 1,000 random samples under the null and re-computing the t-statistic:

ttestgenerator <- function(n) {
    # note that here we have a false "smokers" group where we actually
    # sample from the nonsmokers. this is because we are modeling the *null*
    smokers = sample(dat$bwt[dat$smoke==0], n)
    nonsmokers = sample(dat$bwt[dat$smoke==0], n)
    # Here is the computation of the T-statistic.
    return((mean(smokers)-mean(nonsmokers))/sqrt(var(smokers)/n + var(nonsmokers)/n))
}

# I generate 1,000 and I can now check if, in fact, this follows a normal distribution.
ttests <- replicate(1000, ttestgenerator(10))

# With 1,000 simulated ocurrences of this random variable we can now get a gimplse of it's distribution

hist(ttests)

qqnorm(ttests)
abline(0,1)

# This looks like a very good approximation. So for this particular population a sample size of 10 was large enough to use the
# CLT approximation. How about 3?

ttests <- replicate(1000, ttestgenerator(3))

# The right tail-- you get bigger values in the observed T-statistics than predicted by the asymptotics.
qqnorm(ttests)
abline(0,1)

# Now we see that the large quantiles (refered to by statisticians as the tails) are large than expected. In the previous
# module we explained that when the sample size is not large enough and the population values follow a normal distribution
# then the t-distribution is a better approximation. Our simulation results seem to confirm this:

qs <- (seq(0,999)+0.5)/1000
qqplot(qt(qs,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)
# Now one reason it's not perfect it because is, for the T-distribution to be a good approximation, the original data needs to
# be normally distributed.


# We can look at the original data. This is the entire population. Making a Q-Q plot against the normal, and you can see that
# the tails are a little bit bigger than the normal distribution predicts.
qqnorm(dat$bwt[dat$smoke==0])
qqline(dat$bwt[dat$smoke==0])


# In the previous section we sampled from the entire population. In many cases we don't have access to data from the entire
# population. In these cases we can simulate the populaton data as well, using what is called a "parametric simulation". This
# means that we take parameters from the real data (here the mean and the standard deviation), and plug these into a model
# (here the normal distribution). This is acually the most common form of Monte Carlo simulation.

# For the case of wieghts we could use:
nonsmokerweights <- rnorm(5000, 
    mean=mean(dat$bwt[dat$smoke==0]), 
    sd=sd(dat$bwt[dat$smoke==0]))

# and repeat the entire excercise.


