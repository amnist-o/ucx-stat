library(tidyverse)
df <- read.csv("datasets/lego1.csv")

table(df$sex,df$emotion)

# H0: no correlation between 2 variables = distribution of emotion perceived does not depend on the sex of the respondent
# H1: correlation between 2 variables = distribution of emotion perceived depend on the sec of the respondent.

# Chi-squared test assumes independence of categories -> variables should be nominal

# if the distribution does not depend on sex -> expected response from each sex should be
# ratio of each emotion * sample size of each sex

emo.ratio <- table(df$emotion)
emo.ratio/sum(emo.ratio)

# expected response from female
emo.ratio/sum(emo.ratio) * sum(df$sex=="F")

# expected response from male
emo.ratio/sum(emo.ratio) * sum(df$sex=="M")

# chi-squared = sum(observed - expected)^2/expected
# df = (rows - 1)(cols - 1)
# since we have 2 sex and  6 emotions -> df = (2-1)(6-1) = 5
chisq.test(df$sex,df$emotion)

# order of variables do not matter
chisq.test(df$emotion,df$sex)

# assumption: classical chi test sample size is large enough to ensure that count in each cell are above 5
df1 <- df[1:150,]
table(df1)
cat("We can see that fear and surprise have no observation.\n")
chisq.test(df1$sex,df1$emotion)

# use chi-squared with simulation
chisq.test(df1$sex,df1$emotion,simulate.p.value = T)

# chi-squared can be use with 10 groups and 5 responses or 2 groups with 2 responses
# 2 groups with 2 responses is equivalent to 2 binomial proportions
