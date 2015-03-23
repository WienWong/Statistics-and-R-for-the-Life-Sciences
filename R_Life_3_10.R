
# Week 4 -- Inference III  Permutation Tests    Exercise 

# Reason for why you might use a permutation test:

# "Sometimes the summary statistics we form are not as simple as just taking the average or the difference in averages, and
# ... the null distribution is not something we can easily compute, so permutation tests give us an alternative way of doing
# this, of computing the null distribution."

set.seed(0)
N <- 50
smokers <- sample(babies$bwt[babies$smoke==1], N)
nonsmokers <- sample(babies$bwt[babies$smoke==0], N)

# We calculated the observed difference in means:
    
obs <- mean(smokers) - mean(nonsmokers)

# Finally, we used 1000 replicated permutations to generate a null distribution. We joined the two groups, shuffled the
# samples, and took the first 50 in one group, and the second 50 in a second group:
    
avgdiff <- replicate(1000, {
    all <- sample(c(smokers,nonsmokers))
    smokersstar <- all[1:N]
    nonsmokersstar <- all[(N+1):(2*N)]
    return(mean(smokersstar) - mean(nonsmokersstar))
})

# Finally, we calculated a probability of seeing such a large difference between means under the null hypothesis. We use the
# absolute value, because we are interested if the difference was large in the positive or negative direction.

mean(abs(avgdiff) > abs(obs))

##

obs2 <- median(smokers) - median(nonsmokers)

avgdiff2 <- replicate(1000, {
    all <- sample(c(smokers,nonsmokers))
    smokersstar <- all[1:N]
    nonsmokersstar <- all[(N+1):(2*N)]
    return(median(smokersstar) - median(nonsmokersstar))
})

mean(abs(avgdiff2) > abs(obs2))










