Post hoc power analysis
================
2024-10-02

## Assessment

Consider the following seed germination experiment. Three groups of 40
seeds each were submitted to three different treatments (A, B, and C).
The resulting germination counts were 20, 21 and 27 respectively.

An ANOVA for binomial response (via GLM) was performed as follows:

``` r
library(tidyverse)
```

``` r
d <- data.frame(group = LETTERS[1:3],
                germinated = c(20,21,27),
                total = rep(40,3))

anova(glm(cbind(germinated, total - germinated)~group,data = d, 
          family = "binomial"(link = "logit")),
      test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: cbind(germinated, total - germinated)
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##       Df Deviance Resid. Df Resid. Dev Pr(>Chi)
    ## NULL                      2     2.9656         
    ## group  2   2.9656         0     0.0000    0.227

Perform the post hoc power analysis to figure out the minimum number of
seeds in each group (assume the same number of seeds in each group)
necessary to detect the observed effects (provided that they actually
exist) with at least 80% probability at 5% significance level.

``` r
pow <- 0
for(i in 1:30){
  beta <- numeric(1000)
  for(j in 1:1000){
    d %>%
      mutate(total = 40+(10*i),
             germinated = rbinom(3,total,prob = c(20/40,21/40,27/40))) -> dsim
  glm(cbind(germinated, total - germinated)~group,data = dsim,
      family = "binomial"(link = "logit")) -> msim
  beta[j] = (anova(msim,test = "Chisq")[2,5] < 0.05)
  }
  pow[i] = mean(beta)
}
d.pow <- data.frame(n = 40+seq(10,300,10),
                    pow = pow)

plot(d.pow$n, d.pow$p, ty="b",pch = 16,
     xlab = "Sample size, n",
     ylab = "Statistical power")
abline(h = .8, col = "violetred")
```

![](posthoc-binom_files/figure-gfm/adding%20samples-1.png)<!-- -->
