ANCOVA
================
2024-09-12

## ANCOVA

ANOVA but there are other variables in the model which we need to
account for.

Basically, it is that we want to compare not only the variables we are
interested in but all the variables that affect the output. Take a look
at weight and height data:

``` r
library(tidyverse)
```

``` r
dat <- read.csv("datasets/howell1.csv",sep=";")
dat %>%
  filter(age >= 18) %>%
  mutate(male = factor(male)) -> dat
m1 <- lm(weight ~ height, data = dat)
m2 <- update(m1, .~height*male)
anova(m1,m2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: weight ~ height
    ## Model 2: weight ~ height + male + height:male
    ##   Res.Df    RSS Df Sum of Sq     F Pr(>F)
    ## 1    350 6297.3                          
    ## 2    348 6292.1  2     5.207 0.144 0.8659

In this case, we do not spot statistically significant differences
between the sex in weight after adjusting for height.
