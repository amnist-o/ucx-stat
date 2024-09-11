Googness of Fit
================
2024-09-11

## Variance decomposition and $R^2$

In ANOVA, we decompose sum of squared errors into the within group and
between group elements.

In linear models, we can decompose the observed variation into the part
explained by the model and the part unexplained by the model:

$$
\sum_i(y_i-\bar{y})^2=\sum_i(\hat{y}_i-\bar{y})^2+\sum_i(y_i-\hat{y}_i)^2
$$

where $\hat{y}_i$ is the model prediction.

If the model is good then the part that is explained by the model should
be close to the observed variation.

$$
R^2 = \frac{\sum_i(\hat{y}_i-\bar{y})^2}{\sum_i(y_i-\bar{y})^2}
$$

So, higher $R^2$, the better the model is.

Note: $R^2$ says nothing about the complexity of the model like number
of parameters. Therefore, it is not particularly good for comparing
models with different parameters.

## Further variance decomposition

Total variance explained is equals to variance explained by variable 1 +
variance explained further by variable 2 and so on. So adding a variable
might resulted in small addition in variance explained due to numeric
correlation.

When we have multicollinearity, variance further explained by variable 2
is 0. Thus, variables may appear significant on their own but not
together.

Let’s simulate some data

``` r
set.seed(20202020)
x1 <- runif(20)
x2 <- x1+ rnorm(20,0,.1)
y <- x1+.01*x2+rnorm(20,0,.05)
m <- lm(y~x1+x2)
summary(m)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x1 + x2)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.095577 -0.022939 -0.004005  0.031042  0.109932 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.04123    0.02547   1.619    0.124    
    ## x1           0.81406    0.12405   6.562 4.84e-06 ***
    ## x2           0.15657    0.11716   1.336    0.199    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05009 on 17 degrees of freedom
    ## Multiple R-squared:  0.9667, Adjusted R-squared:  0.9628 
    ## F-statistic: 246.5 on 2 and 17 DF,  p-value: 2.778e-13

``` r
anova(m)
```

    ## Analysis of Variance Table
    ## 
    ## Response: y
    ##           Df  Sum Sq Mean Sq  F value    Pr(>F)    
    ## x1         1 1.23256 1.23256 491.3097 5.542e-14 ***
    ## x2         1 0.00448 0.00448   1.7858     0.199    
    ## Residuals 17 0.04265 0.00251                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The `Sum Sq` shows the sum of squares decomposition. We can see that
$X_1$ explains $\frac{1.2326}{1.2326+0.00448+0.04265}\approx 96\%$ of
the total variation.

What if we change the order?

``` r
anova(update(m,.~x2+x1))
```

    ## Analysis of Variance Table
    ## 
    ## Response: y
    ##           Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## x2         1 1.12901 1.12901 450.034 1.140e-13 ***
    ## x1         1 0.10803 0.10803  43.062 4.838e-06 ***
    ## Residuals 17 0.04265 0.00251                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Now, contribution of $X_2$ is fairly large at
$\frac{1.1290}{1.1290+0.10883+0.04265}\approx 88\%$. The contribution of
$X_1$ is smaller because once $X_2$ is added there is less variation to
explain. However, the residuals sum of squared does not change.

## Nested models

Nested models can be compared using F-test. Same as for ANOVA. Nested
model is model that contains all the variables of the others.

$$
\begin{aligned}
\text{nested} \\
Y&=\beta_0+\beta_1X_1+\beta_2X_2 \\
Y&=\beta_0+\beta_1X_1 \\
\text{not nested} \\
Y&=\beta_0+\beta_1X_1+\beta_2X_2 \\
Y&=\beta_0+\beta_1X_1X_2
\end{aligned}
$$

## An F-test

Let’s compare 2 models with ANOVA:

``` r
m1 <- lm(y~x1+x2)
m2 <- lm(y~x1)
anova(m1,m2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: y ~ x1 + x2
    ## Model 2: y ~ x1
    ##   Res.Df      RSS Df  Sum of Sq      F Pr(>F)
    ## 1     17 0.042648                            
    ## 2     18 0.047128 -1 -0.0044802 1.7858  0.199

`Pr(>F)` more than 0.05 means that the two models are not statistically
significantly different. We should use the model with lesser variables.

F-test is useful when two models are different in more than one term

``` r
anova(m1,update(m2,.~x1*x2))
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: y ~ x1 + x2
    ## Model 2: y ~ x1 + x2 + x1:x2
    ##   Res.Df      RSS Df  Sum of Sq     F Pr(>F)
    ## 1     17 0.042648                           
    ## 2     16 0.042647  1 7.2802e-07 3e-04  0.987

If you compare non-nested model. R will produce the result but you will
not see the p value. R will not prevent any misuse of the command since
the responsibility lies with us.

``` r
anova(m2,update(m2,.~x2))
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: y ~ x1
    ## Model 2: y ~ x2
    ##   Res.Df      RSS Df Sum of Sq F Pr(>F)
    ## 1     18 0.047128                      
    ## 2     18 0.150678  0  -0.10355
