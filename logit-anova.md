ANOVA on logistic regression
================
2024-09-25

## ANOVA for binary response

Let’s look at the chaser data. Actually, we have 5 different chasers.
Let’s see if the chaser has any effect on the odds of winning.

``` r
library(tidyverse)
library(multcomp)
```

``` r
chase <- read_lines(file = "datasets/chase")
df <- read.table(file = chase, header = T, sep = ",", stringsAsFactors = T)
summary(df)
```

    ##    Chaser      TeamScore      ChaserScore       TimeLeft        Win        
    ##  Anne :237   Min.   : 1.00   Min.   : 3.00          :247   Min.   :0.0000  
    ##  Jenny: 43   1st Qu.:14.00   1st Qu.:14.00   (0:26) : 19   1st Qu.:0.0000  
    ##  Mark :246   Median :17.00   Median :16.00   (0:01) : 18   Median :0.0000  
    ##  Paul :210   Mean   :16.77   Mean   :15.85   (0:15) : 18   Mean   :0.2557  
    ##  Shaun:230   3rd Qu.:19.00   3rd Qu.:18.00   (0:21) : 18   3rd Qu.:1.0000  
    ##              Max.   :28.00   Max.   :27.00   (0:14) : 17   Max.   :1.0000  
    ##                                              (Other):629

``` r
m <- glm(Win ~ Chaser, data = df,
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

From the coefficients, we can see that odds of winning differs by
chaser. But is the difference statistically significant? Let’s use
`anova` to test the hypothesis of

$$
H_0: p_1 = p_2 = \dots = p_n
$$ against

$$
H_1: \text{there is at least one group that is different from the others}
$$

``` r
anova(update(m,.~1),m,test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Win ~ 1
    ## Model 2: Win ~ Chaser
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
    ## 1       965     1098.3                        
    ## 2       961     1080.4  4   17.924 0.001277 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The anova test confirms that there is at least one group that is
different from the others. So the chaser affect the odds of winning.
Let’s dig deeper to determine how.

``` r
g <- glht(m,linfct = mcp(Chaser = "Tukey"))
summary(g)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: glm(formula = Win ~ Chaser, family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Linear Hypotheses:
    ##                    Estimate Std. Error z value Pr(>|z|)  
    ## Jenny - Anne == 0  -1.10412    0.54675  -2.019   0.2366  
    ## Mark - Anne == 0   -0.07179    0.21633  -0.332   0.9971  
    ## Paul - Anne == 0    0.08719    0.22050   0.395   0.9943  
    ## Shaun - Anne == 0   0.52531    0.20655   2.543   0.0723 .
    ## Mark - Jenny == 0   1.03233    0.54678   1.888   0.3017  
    ## Paul - Jenny == 0   1.19131    0.54844   2.172   0.1736  
    ## Shaun - Jenny == 0  1.62944    0.54298   3.001   0.0197 *
    ## Paul - Mark == 0    0.15899    0.22058   0.721   0.9472  
    ## Shaun - Mark == 0   0.59711    0.20664   2.890   0.0275 *
    ## Shaun - Paul == 0   0.43812    0.21099   2.076   0.2118  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

Let’s assign the letter so we can easily interpret the result.

``` r
g.letter <- cld(g)$mcletters$monospacedLetters
g.letter
```

    ##  Anne Jenny  Mark  Paul Shaun 
    ##  "ab"  " b"  " b"  "ab"  "a "

We can also show all chaser name by remove the intercept:

``` r
m0 <- update(m,.~.-1)
summary(m0)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ Chaser - 1, family = binomial(link = "logit"), 
    ##     data = df)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## ChaserAnne   -1.1731     0.1529  -7.672 1.69e-14 ***
    ## ChaserJenny  -2.2773     0.5249  -4.338 1.44e-05 ***
    ## ChaserMark   -1.2449     0.1530  -8.135 4.11e-16 ***
    ## ChaserPaul   -1.0860     0.1589  -6.836 8.15e-12 ***
    ## ChaserShaun  -0.6478     0.1389  -4.666 3.08e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1339.2  on 966  degrees of freedom
    ## Residual deviance: 1080.4  on 961  degrees of freedom
    ## AIC: 1090.4
    ## 
    ## Number of Fisher Scoring iterations: 4

Let’s plot the probability of winning against each chaser. Remember the
probability is obtained by using inverse function of logit.

``` r
expit <- function(x){1/(1+exp(-x))}
# confidence intervals
m.ci <- confint(m0)
my.df <- data.frame(Chaser = substr(rownames(m.ci),7,11),
                  p.est = expit(m0$coef),
                  p.lo = expit(m.ci[,1]),
                  p.hi = expit(m.ci[,2]))


plot(1:5, my.df$p.est, pch=16, ylim=c(0,1),xlim=c(0,6),
     xaxt='n',xlab='',ylab='Probability of Winning')
axis(1,1:5,my.df$Chaser)
arrows(1:5,my.df$p.lo,1:5,my.df$p.hi,angle=90,length=.01,lwd=2,code=3)
text(1:5,.8,g.letter)
```

![](logistic%20anova/probability-1.png)<!-- -->
