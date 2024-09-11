Non continuous input in linear regression
================
Amnist.O
2024-09-11

## Simple linear regression with binary input

Let’s look at Nancy Howell’s anthropological data of height and sex for
subjects aged at least 18

``` r
library(tidyverse)
```

``` r
df <- read.csv("datasets/howell1.csv",sep = ";")
df %>%
  filter(age >= 18) -> df
```

``` r
plot(df$male,df$height,
     xlab = "is male",
     ylab = "Height (cm)")
```

![](binary-and-categorical-regression_files/figure-gfm/plot-1.png)<!-- -->

We can see that sex is a binary variable which is 1 when the subject is
male and 0 otherwise.

Recall the regression model: $$\mathbb{E}(Y|X) = \beta_0+\beta_1X$$
Interpretation of $\beta_1$ coefficient is the expected change in $Y$
associated with 1 unit change in $X$.

But if we fit $Y$ as height and $X$ as sex, what is the meaning of 1
unit change in sex?

Since $X$ is a binary variable. The expected value of $Y$ given $X$ or
$\mathbb{E}(Y|X)$ is a binary variable as well: $$
\begin{aligned}
\mathbb{E}(Y|X=0)&=0 \\
\mathbb{E}(Y|X=1)&=\beta_0+\beta_1
\end{aligned}
$$ Therefore, the interpretation of $\beta_1$ is a difference in
expected heights for men and women.

Let’s try to fit the model and check assumptions.

``` r
m1 <- lm(height ~ male, data = df)
par(mfrow=c(2,2))
plot(m1)
```

![](binary-and-categorical-regression_files/figure-gfm/model-1.png)<!-- -->

There seems to be no explicit problem from the plots. Next, let’s take a
look at summary.

``` r
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = height ~ male, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -20.6585  -3.4635   0.2965   3.4715  18.7115 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 149.5135     0.4049  369.25   <2e-16 ***
    ## male         10.8450     0.5914   18.34   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.537 on 350 degrees of freedom
    ## Multiple R-squared:   0.49,  Adjusted R-squared:  0.4885 
    ## F-statistic: 336.3 on 1 and 350 DF,  p-value: < 2.2e-16

We can interpret $\beta_1$ as men are on average taller than women by
10.8449576 cm with p value \< 0.0001

We have learned that there is a statistical test for testing difference
in mean, the t-test. Let’s try using t-test to find whether men are
taller than women on average.

``` r
t.test(height~male,data = df, var.equal = T)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  height by male
    ## t = -18.337, df = 350, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -12.008123  -9.681792
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##        149.5135        160.3585

We can see that t-statistic from t.test -18.3374503 is equal to male
t-statistic from regression 18.3374503 but with a different sign due to
order (female - male and male - female).

Simple regression with binary independent variable is identical to
t-test with equal variance assumption.

## Simple linear regression with categorical input

What if the variable is categorical with more than 2 possible values
such as jobs, income class, species. Let’s take a look at Eucalyptus
data with 3 species.

``` r
df.eu <- read.csv("datasets/EucalyptiANOVA1.csv")
```

``` r
boxplot(hgt ~ spp,data=df.eu,
        ylab = "Height (m)",
        xlab = "Species")
```

![](binary-and-categorical-regression_files/figure-gfm/plot%20euca-1.png)<!-- -->

We cannot just code each species as 1, 2, and 3 since it will imply
particular order ( $1<2<3$ ) and particular relationship such as $1+2=3$

We create new variables: dummy variables $D_1$ and $D_2$ as binary
variables for each species: $$
\begin{aligned}
D_1 &=1 \space \text{if species = dun, 0 otherwise} \\
D_2 &=1 \space \text{if species = pil, 0 otherwise}
\end{aligned}
$$

Table for each species are:

| Species | $D_1$ | $D_2$ |
|---------|-------|-------|
| clo     | 0     | 0     |
| dun     | 1     | 0     |
| pil     | 0     | 1     |

Now our linear regression looks like this:
$$\mathbb{E}(Y|D_1,D_2)=\beta_0+\beta_1D_1+\beta_2D_2$$

R automatically treat `chr` or `fct` variable as categorical and create
dummy variables for you.

Let’s fit the model

``` r
m.e <- lm(hgt ~ spp, data = df.eu)
summary(m.e)
```

    ## 
    ## Call:
    ## lm(formula = hgt ~ spp, data = df.eu)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8900 -0.8900 -0.0517  0.6446  8.1900 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  14.9100     0.2230  66.874   <2e-16 ***
    ## sppdun       -0.4583     0.3153  -1.454    0.148    
    ## spppil        0.2800     0.3153   0.888    0.376    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.727 on 177 degrees of freedom
    ## Multiple R-squared:  0.03061,    Adjusted R-squared:  0.01966 
    ## F-statistic: 2.795 on 2 and 177 DF,  p-value: 0.06382

Since we did not define factor levels for species, `R` arranges it
alphabetically.

This is the same as doing ANOVA as binary input regression is the same
as t-test.
