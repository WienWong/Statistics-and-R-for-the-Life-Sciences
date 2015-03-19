
# Week 3 POWER AND ALPHA

# There are several aspects of a hypothesis test that affect its power for a particular effect size. Intuitively, setting a
# lower alpha decreases the power of the test for a given effect size because the null hypothesis will be more difficult to
# reject. This means that for an experiment with fixed parameters (i.e., with a predetermined sample size, recording 
# mechanism, etc), the power o the hypothesis test trades off with its Type I error rate, no matter what effect size you 
# target.

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

# Because we have the full population, we know what the true effect size is (about 8.93), and we can compute the power of the
# test for true difference between populations.

# Take a random sample of N=15 measurements from each of the smoking (bwt.smoke) and nonsmoking (bwt.nonsmoke) groups. Then
# perform a t-test and compare the p-value to a significance level alpha. Do this 1,000 times. Decide whether or not to reject
# the null hypothesis based on three significance levels alpha=0.1, alpha = 0.05, alpha=0.01. For each experiment, keep track
# of whether you correctly rejected the null hypothesis at each of these significance levels (thus, each of the 1,000
# experiments should produce 3 numbers to keep track of). For each significance level, in what proportion of the experiments
# did you correctly reject the null hypothesis?

reject <- function(N, alpha=0.05){
    nons <- sample(bwt.nonsmoke,N) 
    s <- sample(bwt.smoke,N)
    pval <- t.test(nons, s)$p.value
    pval < alpha
}

N <- 15
B <- 1000
alphas <- c(0.1,0.05,0.01)

power <- sapply(alphas,function(al){
    rejections <- replicate(B,reject(N,alpha=al))
    mean(rejections)
})

power

plot(log10(alphas), power, xlab="log (base 10) alpha", type="b")


#  Suppose that one of the homework question is graded based on whether the result you reported falls within a exact 99%
# interval around a true value. Now suppose that 2,000 students complete the assignment, and assume that all students execute
# the simulation correctly. What is the expected number of student responses that would be marked wrong simply by chance?
2000*0.01

# (For this reason, we typically use one-in-a-billion-based intervals (6 standard deviations) to judge answers in this class.)


