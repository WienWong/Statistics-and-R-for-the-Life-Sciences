
# Week 3  POWER CALCULATIONS

set.seed(1)

dat <- read.csv("mice_pheno.csv")
hfpop <- dat[dat$Sex=="F" & dat$Diet=="hf", 3]
chowpop <- dat[dat$Sex=="F" & dat$Diet=="chow", 3]

N <- 5
hf <- sample(chowpop, N)
chow <- sample(chowpop, N)
t.test(hf, chow)

data:  hf and chow
t = 1.774, df = 7.958, p-value = 0.1142
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
    -0.7923933  6.0563933
sample estimates:
    mean of x mean of y 
24.164    21.532

# So using the standard cut off of 0.05, we would not reject the null hypothesis.
# The problem in this particular instance is that we don't have enough power.

# Type One Error (false positive) -- when we reject the null when we should have not rejected the null.

# Type Two Error (false negative) -- when we don't reject the null when we should have.

# Definition of power is the probability of rejecting the null when the null is false.

# Run a simulation to see what the power is:

N <- 12
alpha <- 0.05
B <- 1000

# We are doing it 10,000 times, taking a sample and performing a t-test to see how often we reject.
rejections <- sapply(1:B, function(i) {
    hf <- sample(hfpop, N)
    chow <- sample(chowpop, N)
    t.test(hf, chow)$p.value < alpha
    }
)

head(rejections)
tail(rejections)

# Let's see how often we reject it, 
mean(rejections)
# 0.199 # This is our estimate of power based on this simulation.

# Let's do for several Ns, to see how power improves when we make the sample size bigger.

Ns <- seq(5, 50, 5)
power <- sapply(Ns, function(N){
    rejections <- sapply(1:B, function(i) {
        hf <- sample(hfpop, N)
        chow <- sample(chowpop, N)
        t.test(hf, chow)$p.value < alpha
    })
    return(mean(rejections))
})

plot(Ns, power)
# Just to review again what we're doing. For every one of these Ns, we're sending it through the  function as N value. And
# then we're re-doing what we already did, which is run 10,000 simulations. See how often we reject at that alpha level.

# Not surprise that as N gets bigger, we have more power.

