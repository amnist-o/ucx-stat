Logistic regression
================
2024-09-25

## Load data

We look at chase data. Chase is a game where two teams (chaser and
runner) compete by answering quizes within time limit. If the chaser can
correctly answer more quizes than the runner team, they win. Chase data
consists of number of questions the runner can correctly answer and
whether they win or not.

``` r
library(tidyverse)
```

``` r
chase <- readr::read_lines(file = "datasets/chase")
df <- read.table(file = chase, header = T, sep = ",")
```

## Create a logistic model

``` r
m <- glm(Win ~ TeamScore, data = df, family = "binomial"(link = "logit"))
summary(m)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore, family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -7.27425    0.54112  -13.44   <2e-16 ***
    ## TeamScore    0.35036    0.02914   12.03   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1098.35  on 965  degrees of freedom
    ## Residual deviance:  891.61  on 964  degrees of freedom
    ## AIC: 895.61
    ## 
    ## Number of Fisher Scoring iterations: 5

Interpretation of the regression is that for 1 unit change in $x$ which
in this case is TeamScore, resulted in 0.3503585 unit change in log odds
which is $\log\left(\frac{p}{1-p}\right)$.

Confident interval can be found using `confint`

``` r
confint(m)
```

    ## Waiting for profiling to be done...

    ##                  2.5 %     97.5 %
    ## (Intercept) -8.3710659 -6.2477653
    ## TeamScore    0.2949191  0.4092537

Remember that the interpretation is log odds. To convert back refer to
log linear interpretation markdown.

``` r
scales::percent(exp(confint(m)[2,])-1,accuracy = 1e-6)
```

    ## Waiting for profiling to be done...

    ##        2.5 %       97.5 % 
    ## "34.301769%" "50.569369%"

## Create plots

Estimated curve can be plotted with following codes:

``` r
plot(df$TeamScore,jitter(df$Win),
     xlab='Team Score',ylab='Probability of Winning',yaxt='n')
abline(h=0,col='gray');abline(h=1,col='gray')
axis(2,at=0:1,c('Lose','Win'))
# add logit curve
curve(1/(1+exp(-(m$coef[1]+m$coef[2]*x))),col='red',lwd=2,add=T)
```

![](logistic-regression_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Prediction curve can be plotted with a tweak in the code:

``` r
plot(df$TeamScore,jitter(df$Win),
     xlab='Team Score',ylab='Probability of Winning',yaxt='n')
abline(h=0,col='gray');abline(h=1,col='gray')
axis(2,at=0:1,c('Lose','Win'))

# predict the response with predict and type of predict is response or predict will return the probability
d.predict <- data.frame(TeamScore=0:30)
d.predict$PWin <- predict(m,newdata=d.predict, type="response")

lines(d.predict$TeamScore, d.predict$PWin, col='red',lwd=2)
```

![](logistic-regression_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Confident interval envelope can be plotted with:

``` r
plot(df$TeamScore,jitter(df$Win),
     xlab='Team Score',ylab='Probability of Winning',yaxt='n')
abline(h=0,col='gray');abline(h=1,col='gray')
axis(2,at=0:1,c('Lose','Win'))

d.predict <- data.frame(TeamScore=0:30)

# mean for the prediction (on logit scale)
d.predict$MEAN <- predict(m,newdata=d.predict, type="link")

# standard error for the prediction (on logit scale)
d.predict$SE <- predict(m,newdata=d.predict, type="link", se.fit=T)$se.fit

d.predict$PWin <- 1/(1+exp(-d.predict$MEAN))

# 95% CI limits:
d.predict$PWin.Lo <- 1/(1+exp(-(d.predict$MEAN-1.96*d.predict$SE)))
d.predict$PWin.Hi <- 1/(1+exp(-(d.predict$MEAN+1.96*d.predict$SE)))



lines(d.predict$TeamScore, d.predict$PWin, col='salmon',lwd=2)
lines(d.predict$TeamScore, d.predict$PWin.Lo, col='red',lwd=2,lty=3)
lines(d.predict$TeamScore, d.predict$PWin.Hi, col='red',lwd=2,lty=3)
```

![](logistic-regression_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
