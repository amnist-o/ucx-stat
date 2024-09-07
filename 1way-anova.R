# Anova
# t-test has type 1 error (false positive)
# when we want to test multiple groups, false positive probability for teting each pair rises
library(tidyverse)
df <- read.csv("datasets/Eucalypti_ANOVA.csv")

df$stocking <- factor(df$stocking)

# Exploratory Data Analysis (EDA)
df %>%
  reframe(across(c(hgt),
                 list(n = length, mean = mean, sd = sd)),
          .by = stocking)

boxplot(hgt~stocking,data=df)

# Performing ANOVA
m1 <- lm(hgt ~ stocking -1, data = df)
m2 <- update(m1,.~.+1)

anova(m1)

# At least one group mean is different (statistically sig) from others'.
# Using Tukey's post hoc test to figure out which groups are different from each other.
library(multcomp)

# Tukey test
g1 <- glht(m1, linfct = mcp(stocking = "Tukey"))
summary(g1)

# creating a plot
df.anova <- data.frame(stocking=levels(df$stocking),
                       est=m1$coefficients,
                       lo=confint(m1)[,1],
                       hi=confint(m1)[,2],
                       z=rep(25,4), # height for explanatory text in the graph
                       cld=cld(g1)$mcletters$Letters)

df %>%
  ggplot(aes(x=stocking)) + 
  geom_boxplot(aes(y=hgt))+
  geom_errorbar(data=df.anova,aes(ymin=lo,ymax=hi),
                width=.1,linewidth=1.5,col='blue')+
  geom_point(data=df.anova,aes(y=est),col='red',size=2)+
  geom_text(data=df.anova, aes(y=z,label=cld),size=10)+
  ylim(0,30)+
  theme(text=element_text(size=15))+
  ylab('Height (m)')

# lm plot to compare sd between groups
# if red line is level then sd are equal
plot(m1,3)

# qq plot for all group to see if all sample are normally distributed
plot(m1,2)

# the tails are off

# two-ways ANOVA
df2 <- read.csv("datasets/EucalyptiANOVA2way.csv")
df2$stocking <- factor(df2$stocking)

# plots
df2 %>%
  ggplot(aes(x = spp, y = hgt, fill = stocking))+
  geom_boxplot()+
  facet_wrap(~spp,scales = "free")

# decompose variance
m3 <- lm(hgt ~ stocking*spp,data = df2)
anova(m3)

# P-value of the interaction term is small
# It means impossible to interpret the effect of individual factors since each combination is different

# If the P-value of the interaction term is large (>0.05)
# It means we can see some pattern repeated in different groups.
# We can look at the effect of each factor separately

# If you want to evaluate the overall statistical significance of species,
# fit hte model with and without the species factor and compare the two:

anova(m3,update(m3,.~stocking))

# since the p-value < 0.05, effect of factor species is statistically significant
# If there are categorical factor variable involved:
# comparing blood pressure among smokers and non-smokers with age-adjust
# look for ANCOVA (ANalysis of VAriance with COvariates)
# comparing binary responses, we need to use glm rather than lm
# Statistical power is the probability of rejecting the null hypothesis when it is false