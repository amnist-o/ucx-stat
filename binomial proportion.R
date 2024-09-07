library(tidyverse)

# binomial proportions
# H0: true population proportionA = true population proportionB
# H1: they are not equal

# let nA nB be the sizes of samples from population A and B
# pA pB be the observed sample proportions

# for large sample, CLT saying the samples are normally distibuted
# we can easily use t-test if the sample are binary

# for small sample, we can use chi-squared test for 2x2 table
# or use prop.test()

# if the sample are less than 5, tests wont work well
# prop.test only use for 2 groups
