
# Week 3 ASSOCIATION TESTS

tab <- matrix(c(180, 40, 20, 20), 2, 2)

tab

rownames(tab) <- c("AA or Aa", "aa")

colnames(tab) <- c("Controls", "Cases")

tab

prop.table(tab)

tab[2, 2] <- 10

tab

prop.table(tab)

prop.table(tab, 1)

ctest <- chisq.test(tab)

ctest

######

# This dataframe reflects the allele status (either AA/Aa or aa) and the case/control status for 72 individuals.
d = read.csv("assoctest.csv")

tab = table(d$allele, d$case)

# Compute the Chi-square test for the association of genotype with case/control status
chisq.test(table(d))

# Compute the Fisher's exact test ( fisher.test() ) for the same table. What is the p-value?
fisher.test(table(d))


