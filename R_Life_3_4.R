
# Week 3 

babies = read.table("babies.txt", header=TRUE)

# Extract the baby birthweights from smoking and non-smoking mothers with the code:
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

# T-TEST FOR DIFFERENCE BETWEEN MEANS 

# Take a random sample of N=30 measurement from each of the smoking and nonsmoking datasets. Then compute the difference in
# their means and construct a 95% confidence interval for the difference using t.test(). Do this 1,000 times and keep track of
# all the confidence intervals. One way to answer this question is to use the replicate() function in R, which will return a 2
# x 1000 matrix of the lower and upper bound for each of the 1000 replications.
N = 30

CIs = replicate(1000, t.test(sample(bwt.nonsmoke, N), sample(bwt.smoke, N))$conf.int)

mean(CIs[2, ] - CIs[1, ])

# The population-level difference was
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
popdiff
# How often (what proportion of times) did the confidence intervals contain the population-level difference? That is, what
# proportion of times was the lower bound of the confidence interval less than popdiff and the upper bound greater than
# popdiff?

mean(CIs[1, ] < popdiff & CIs[2, ] > popdiff)

# In the previous video Rafa mentioned that we should report confidence intervals whenever possible because they communicate
# both an effect size and the statistical significance of a result. Rafa later mentions that when comparing a difference
# between two groups to zero, we can tell whether the difference has a p-value of less than 0.05 based on whether the
# confidence interval for the difference contains zero. We will explore that statement in more detail in the next two
# questions.

# Recall that when we perform a t-test for the difference between two means, we calculate a t-value like the following.

dat.ns = sample(bwt.nonsmoke, 30)
dat.s = sample(bwt.smoke, 30)
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/30 + sd.s^2/30)
tval = (X.ns - X.s)/sd.diff

# Because our sample sizes are rather large, we can then use the qnorm() function to tell whether tval corresponds to a 
# p-value that is less than 0.05.

qnorm(1-0.05/2)

# This tells us that if the absolute value of tval is greater than 1.96, the p-value is less than 0.05 and the result is
# significant an the 0.05 level.

# We can use the same numbers to construct a confidence interval for the difference in means between the smoking and 
# nonsmoking populations. 

ci.upper = (X.ns-X.s) + sd.diff*1.96
ci.lower = (X.ns-X.s) - sd.diff*1.96

# the difference in means (X.ns - X.s) must have absolute value greater than _____ times sd.diff in order for the result to
# be significant (at alpha=0.05).
1.96

# the difference in means (X.ns - X.s) must be a greater distance than _____ times sd.diff away from 0 in order for the 95%
# confidence interval not to contain 0.
1.96

# Based on these facts, we see that having a p-value less than 0.05 is equivalent to having a 95% confidence interval that
# does not contain 0.

