Logistic ANCOVA
================
2024-09-26

Let’s step up and try to fit real model. Refer to the chase data, we are
going to include team score and the chaser to the model.

``` r
library(tidyverse)
library(multcomp)
library(ggpubr)
```

``` r
chase <- read_lines(file = "datasets/chase")
df <- read.table(file = chase, header = T, sep = ",", stringsAsFactors = T)

m <- glm(Win ~ TeamScore * Chaser, data = df,
         family = "binomial"(link = "logit"))

summary(m)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore * Chaser, family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           -7.70693    1.19539  -6.447 1.14e-10 ***
    ## TeamScore              0.36414    0.06338   5.746 9.15e-09 ***
    ## ChaserJenny           -3.30331    4.52792  -0.730    0.466    
    ## ChaserMark            -1.28731    1.75267  -0.734    0.463    
    ## ChaserPaul            -0.89134    1.80398  -0.494    0.621    
    ## ChaserShaun            1.80010    1.48340   1.213    0.225    
    ## TeamScore:ChaserJenny  0.11885    0.23149   0.513    0.608    
    ## TeamScore:ChaserMark   0.05785    0.09162   0.631    0.528    
    ## TeamScore:ChaserPaul   0.05568    0.09654   0.577    0.564    
    ## TeamScore:ChaserShaun -0.05030    0.08098  -0.621    0.534    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1098.4  on 965  degrees of freedom
    ## Residual deviance:  857.3  on 956  degrees of freedom
    ## AIC: 877.3
    ## 
    ## Number of Fisher Scoring iterations: 6

The output is overwhelming. Let’s ask a question so we know what to look
for.

## Does the outcome depend on the chaser at all?

We will use the ANCOVA (analysis of variance with covariates). We want
to know if the chaser affect the odds of winning. However, we want to
include the effect of team score as well. So, we compare the model that
include the chasers and team score with the model with just team score.

``` r
anova(update(m,.~TeamScore),m,test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Win ~ TeamScore
    ## Model 2: Win ~ TeamScore * Chaser
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1       964     891.61                          
    ## 2       956     857.30  8   34.317 3.559e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We can see that the model is statistically different from each other.
Thus, the chaser identity matters.

## Does the shape of estimated win probability depend on the chaser?

If the model is additive, the curves will just shift left and right (due
to the nature of the function). If the model is multiplicative (with
interactions), the curves might be different. Let’s see what they look
like.

``` r
my.df <- data.frame(expand.grid(Chaser=unique(df$Chaser),TeamScore=1:30))

my.df$P.est <- predict(m,my.df,type='response')

(my.df %>%
  ggplot(aes(x=TeamScore,y=P.est))+
  geom_line(aes(group=Chaser,col=Chaser),linewidth=1.5)+
  ylab("Estimated Probability of Winning")+theme_bw()+
  ggtitle('With interaction') -> g.int)
```

![](logit-ancova_files/interaction%20plot-1.png)<!-- -->

Now, let’s plot the additive one:

``` r
m.add <- update(m,.~TeamScore+Chaser)

my.df$P.est.add <- predict(m.add,my.df,type='response')

(my.df %>%
  ggplot(aes(x=TeamScore,y=P.est.add))+
  geom_line(aes(group=Chaser,col=Chaser),size=1.5)+
  ylab("Estimated Probability of Winning")+theme_bw()+
  ggtitle('Additive') -> g.add)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](logit-ancova_files/additive%20curve-1.png)<!-- -->

``` r
ggarrange(g.int,g.add,nrow=1,
          common.legend = T)
```

![](logit-ancova_files/plot%20together-1.png)<!-- -->

What does the figure tell us? Besides the difference in shape, we are
not sure. We don’t know if the difference is significant. So let’s use
statistical test to answer the question.

``` r
anova(m,m.add,test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Win ~ TeamScore * Chaser
    ## Model 2: Win ~ TeamScore + Chaser
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       956     857.30                     
    ## 2       960     859.88 -4  -2.5819     0.63

Apparently, the effect of teamscore does not depend on chaser.
$p = 0.6300417$

With the result, we can interpret additive model which is easier.

``` r
summary(m.add)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore + Chaser, family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -7.88913    0.59759 -13.201  < 2e-16 ***
    ## TeamScore    0.37390    0.03063  12.208  < 2e-16 ***
    ## ChaserJenny -1.02177    0.59187  -1.726   0.0843 .  
    ## ChaserMark  -0.18498    0.24493  -0.755   0.4501    
    ## ChaserPaul   0.13377    0.24658   0.542   0.5875    
    ## ChaserShaun  0.95031    0.24111   3.941  8.1e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1098.35  on 965  degrees of freedom
    ## Residual deviance:  859.88  on 960  degrees of freedom
    ## AIC: 871.88
    ## 
    ## Number of Fisher Scoring iterations: 5

The effect of additional point in team score increase the odds of
winning by 45.3392%. Odds of winning against Jenny is 2.7731948 smaller
than odds of winning against Anne.

## Default category

Like linear regression with categorical variables, we can change the
reference level by using `relevel`.

``` r
update(m.add,.~TeamScore + relevel(Chaser,"Mark")) %>%
  summary()
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore + relevel(Chaser, "Mark"), family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  -8.07412    0.60980 -13.241  < 2e-16 ***
    ## TeamScore                     0.37390    0.03063  12.208  < 2e-16 ***
    ## relevel(Chaser, "Mark")Anne   0.18498    0.24493   0.755    0.450    
    ## relevel(Chaser, "Mark")Jenny -0.83679    0.59273  -1.412    0.158    
    ## relevel(Chaser, "Mark")Paul   0.31875    0.24965   1.277    0.202    
    ## relevel(Chaser, "Mark")Shaun  1.13529    0.24586   4.618 3.88e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1098.35  on 965  degrees of freedom
    ## Residual deviance:  859.88  on 960  degrees of freedom
    ## AIC: 871.88
    ## 
    ## Number of Fisher Scoring iterations: 5
