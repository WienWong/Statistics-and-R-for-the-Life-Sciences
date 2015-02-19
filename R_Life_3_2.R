
# HarvardX R Life Week 3-2

dat <- read.csv("femaleMiceWeights.csv")

# The first 12 are the controls of the data 
control <- dat[1:12, 2]
treatment <- dat[12+1:12, 2] # from 12th to the 24th mice
diff <- mean(treatment)+ -mean(control)
print(diff)
# 3.020833

t.test(treatment, control)

# So to report a p value we have to know what the distribution of this random variable diff is.
# Under the null hypothesis they (treatment, control) have same mean. So if you subtract them, they have mean 0.

# The standard deviation of the sample.
sd(control)
# And we use it as an estimate of the actual standard deviation of the population.

# To get the stand error we have to divide by the square of the sample size
sd(control)/sqrt(length(control))

# The variance of the difference of two random variables (aka, treatment, control) that are independent of each other. That
# means that one doesn't affect the other, because they're two independent samples, we take 12 random mice, and another 12
# random mice, so they shouldn't affect each other. So the variance of the difference of two random variables is the sum of
# their variances.

# The stand error of the diff
se <- sqrt( var(treatment)/length(treatment) + var(control)/length(control) )
se
# 1.469867

# Random variablr divided by its stand error
tstat <- diff / se
# 2.055174

pnorm(tstat)
# It gives us the probability that a normal random variable, mean 0, standard deviation 1, is bigger than 2.05.

1 - pnorm(tstat)

1 - pnorm(tstat) + pnorm(-tstat)
# 0.0398622

# Because of the symmetry of the standard normal distribution, there is a simpler way to calculate the probability that a 
# t-value under the null could have a larger absolute value than tval. 

2*pnorm(-abs(tstat)) 
# 0.0398622
# The area outside of -|tval| and |tval| is equal to 2 times the area to the left of -|tval|. Change tval to tstat.
##

qqnorm(control)
qqline(control)

qqnorm(treatment)
qqline(treatment)

##

# We are interested in testing whether the birth weights of babies born to non-smoking mothers are significantly different
# from the birth weights of babies born to smoking mothers.

babies = read.table("babies.txt", header=TRUE)
head(babies, 6)

bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
head(bwt.smoke)

# Now, we can look for the true population difference in means between smoking and non-smoking birthweights.
mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

# Compute the t-value (t-statistic) for the first 30 weights of non-smoking mothers and the first 30 weights of smoking
# mothers. Confirm that the t-statistic calculated manually and by t.test() is the same. What is the t-value (t-statistic)?

X.ns = mean(bwt.nonsmoke[1:30])
sd.ns = sd(bwt.nonsmoke[1:30])
X.s = mean(bwt.smoke[1:30])
sd.s = sd(bwt.smoke[1:30])
N = 30
sd.diff = sqrt(sd.ns^2/N + sd.s^2/N)
tval = (X.ns - X.s) / sd.diff
t.test(bwt.nonsmoke[1:30], bwt.smoke[1:30])

# Or

dat.ns = head(bwt.nonsmoke, 30)
dat.s = head(bwt.smoke, 30)
t.test(dat.ns, dat.s)$statistic

# 

