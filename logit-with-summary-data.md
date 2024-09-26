Bernoulli vs Binomial
================
2024-09-26

## Logistic regression with summary data

``` r
library(tidyverse)
library(multcomp)
```

``` r
chase <- read_lines(file = "datasets/chase")
df <- read.table(file = chase, header = T, sep = ",", stringsAsFactors = T)
df %>%
  reframe(Wins = sum(Win),
          Total = length(Win),
          .by = Chaser) %>%
  arrange(Chaser) -> df.sum
df.sum
```

    ##   Chaser Wins Total
    ## 1   Anne   56   237
    ## 2  Jenny    4    43
    ## 3   Mark   55   246
    ## 4   Paul   53   210
    ## 5  Shaun   79   230

Let’s fit logistic regression on `df.sum`

``` r
m.sum <- glm(cbind(Wins, Total - Wins) ~ Chaser, data = df.sum,
             family = "binomial"(link = "logit"))
summary(m.sum)
```

    ## 
    ## Call:
    ## glm(formula = cbind(Wins, Total - Wins) ~ Chaser, family = binomial(link = "logit"), 
    ##     data = df.sum)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.17315    0.15291  -7.672 1.69e-14 ***
    ## ChaserJenny -1.10412    0.54683  -2.019   0.0435 *  
    ## ChaserMark  -0.07179    0.21633  -0.332   0.7400    
    ## ChaserPaul   0.08719    0.22050   0.395   0.6925    
    ## ChaserShaun  0.52531    0.20655   2.543   0.0110 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance:  1.7924e+01  on 4  degrees of freedom
    ## Residual deviance: -8.4377e-15  on 0  degrees of freedom
    ## AIC: 35.67
    ## 
    ## Number of Fisher Scoring iterations: 3

compare with original model:

``` r
m <- glm(Win ~ Chaser,data = df,
         family = "binomial"(link = "logit"))
summary(m)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ Chaser, family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.17315    0.15291  -7.672 1.69e-14 ***
    ## ChaserJenny -1.10412    0.54675  -2.019   0.0434 *  
    ## ChaserMark  -0.07179    0.21633  -0.332   0.7400    
    ## ChaserPaul   0.08719    0.22050   0.395   0.6925    
    ## ChaserShaun  0.52531    0.20655   2.543   0.0110 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1098.4  on 965  degrees of freedom
    ## Residual deviance: 1080.4  on 961  degrees of freedom
    ## AIC: 1090.4
    ## 
    ## Number of Fisher Scoring iterations: 4

We can see that the model is the same. This means
`cbind(Wins,Total-Wins)` in `glm` is equal to
$\log\left(\frac{p(win)}{1-p(win)}\right)$. Thus the result is the same
as original model. This helps us to specify the model with different
types of data.
