Perform ANOVA for binary response
================
2024-09-26

## Assessment

Seed vaults are places where seeds are kept in case of losses of genetic
and bio-diversity resulting from diseases, wars and other catastrophic
events. The seeds within may be stored for years or even decades. It is
good to know that the seeds will still be viable after all this time.
And it is important to understand what storage regime works best for
each seed variety.

Below is the result of a germination experiment. Where 4 different
pre-storage treatments were applied to seeds.

| Treatment | Germinated | Total |
|-----------|------------|-------|
| A         | 3          | 40    |
| B         | 25         | 40    |
| C         | 39         | 40    |
| D         | 19         | 40    |

Test whether the germination rate differs between treatments. If on the
verified track you will be able to check your results by answering the
questions that follow.

``` r
library(tidyverse)
library(multcomp)
```

What is the best treatment?

``` r
df <- data.frame(treatment = LETTERS[1:4],
                 germinated = c(3,25,39,19),
                 total = rep(40,4))
# make it to factor to use in multcomp
df$treatment <- factor(df$treatment)

# create a model with cbind since we do not have each attempt. exclude the intercept so we can interpret the result easily.
m <- glm(cbind(germinated,total-germinated) ~ treatment, data = df,
         family = "binomial"(link = "logit"))

summary(m)
```

    ## 
    ## Call:
    ## glm(formula = cbind(germinated, total - germinated) ~ treatment, 
    ##     family = binomial(link = "logit"), data = df)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -2.5123     0.6003  -4.185 2.85e-05 ***
    ## treatmentB    3.0231     0.6834   4.424 9.70e-06 ***
    ## treatmentC    6.1759     1.1773   5.246 1.56e-07 ***
    ## treatmentD    2.4122     0.6787   3.554 0.000379 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance:  8.1966e+01  on 3  degrees of freedom
    ## Residual deviance: -3.5527e-15  on 0  degrees of freedom
    ## AIC: 21.129
    ## 
    ## Number of Fisher Scoring iterations: 4

- We can see that the best treatment is C.

Is it statistically better than any others?

``` r
anova(m, test = "Chisq")
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
    ##           Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                          3     81.966              
    ## treatment  3   81.966         0      0.000 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
glht(m,linfct = mcp(treatment = "Tukey")) %>%
  summary()
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: glm(formula = cbind(germinated, total - germinated) ~ treatment, 
    ##     family = binomial(link = "logit"), data = df)
    ## 
    ## Linear Hypotheses:
    ##            Estimate Std. Error z value Pr(>|z|)    
    ## B - A == 0   3.0231     0.6834   4.424  < 0.001 ***
    ## C - A == 0   6.1759     1.1773   5.246  < 0.001 ***
    ## D - A == 0   2.4122     0.6787   3.554  0.00199 ** 
    ## C - B == 0   3.1527     1.0641   2.963  0.01362 *  
    ## D - B == 0  -0.6109     0.4549  -1.343  0.51520    
    ## D - C == 0  -3.7636     1.0611  -3.547  0.00185 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

``` r
glht(m,linfct = mcp(treatment = "Tukey")) %>%
  cld()
```

    ##   A   B   C   D 
    ## "a" "b" "c" "b"

- We can see that the best treatment C is clearly different from others.

Is treatment B statistically better than treatment D? - From the
pairwise comparison of mean, we can see that treatment B and treatment D
is in the same group. Thus treatment B is not statistically better than
treatment D.

What is the p-value for the hypothesis: Treatment B is different from
treatment D? - From the multiple comparisons of means, at D - B == 0 has
p value around 0.51517.
