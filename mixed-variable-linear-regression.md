Linear regression with mixed type of variables
================

## Linear regression with one continuous and one binary input

Let’s try to combine what we have done by inspecting the association
between weight and height with sex.

``` r
library(tidyverse)
```

``` r
df <- read.csv("datasets/howell1.csv",sep=";")
df %>%
  filter(age >=18) -> df
```

### Additive model

Additive model is the simplest model:
$$\mathbb{E}(weight|height) = \beta_0+\beta_1height +\beta_2sex$$
$\beta_1$ is the average change in weight associated with a 1 cm
increase in height and $\beta_2$ is the difference in weight between a
man and a woman of the same height in every height.

For female the model looks like:
$$\mathbb{E}(weight|height)=\beta_0+\beta_1height$$

For male, the model looks like:
$$\mathbb{E}(weight|height)=\beta_0+\beta_2+\beta_1height$$

In graphical representation, the line for male and female will be
$\beta_2$ apart regardless of height level.

Let’s fit the model

``` r
addm <- lm(weight ~ height+male,data=df)
summary(addm)
```

    ## 
    ## Call:
    ## lm(formula = weight ~ height + male, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.8412  -3.0677  -0.2466   2.7912  14.8527 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -52.95434    6.13866  -8.626 2.26e-16 ***
    ## height        0.63385    0.04100  15.458  < 2e-16 ***
    ## male         -0.09793    0.63529  -0.154    0.878    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.248 on 349 degrees of freedom
    ## Multiple R-squared:  0.5697, Adjusted R-squared:  0.5672 
    ## F-statistic:   231 on 2 and 349 DF,  p-value: < 2.2e-16

Let’s plot the predicted lines and the 95% CI envelopes.

``` r
visreg::visreg(addm,
               xvar = "height",
               by="male",
               overlay=T)
```

![](mixed-variable-linear-regression_files/figure-gfm/fitted%20plot-1.png)<!-- -->

or calculate by hands:

``` r
#preparing a grid for predicted values:
# a range of heights for both sexes:
d.pred <- expand.grid(height = seq(130, 180, 1), 
                      male = c(0, 1))

# obtaining predictions based on the m1
d.pred$weight <- predict(addm, newdata = d.pred)

# obtaining the standard errors for those predictions:
d.pred$weight.se <- predict(addm, newdata = d.pred, 
                            se.fit = T)$se

library(ggplot2)
ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(aes(group = male, col = factor(male))) +
  geom_ribbon(
    data = d.pred,
    aes(
      x = height,
      ymin = weight - 1.96 * weight.se,
      ymax = weight + 1.96 * weight.se,
      group = male,
      fill = factor(male)
    ),
    alpha = .3
  ) +
  geom_line(data = d.pred,
            aes(
              x = height,
              y = weight,
              group = male,
              col = factor(male)
            ),
            linewidth = 1.2)
```

![](mixed-variable-linear-regression_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
summary(addm)
```

    ## 
    ## Call:
    ## lm(formula = weight ~ height + male, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.8412  -3.0677  -0.2466   2.7912  14.8527 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -52.95434    6.13866  -8.626 2.26e-16 ***
    ## height        0.63385    0.04100  15.458  < 2e-16 ***
    ## male         -0.09793    0.63529  -0.154    0.878    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.248 on 349 degrees of freedom
    ## Multiple R-squared:  0.5697, Adjusted R-squared:  0.5672 
    ## F-statistic:   231 on 2 and 349 DF,  p-value: < 2.2e-16

From the result, we can see that the fitted line for male is below
female in every height indicated that male has less weight than female
of the same height.

## Linear regression with interaction

Additive model assumed that the association between height and weight is
the same for men and women. However, the association of height and
weight change according to sex.

$$\mathbb{E}(weight|height)=\beta_0+\beta_1height+\beta_2sex+\beta_3 \space height\times sex$$

The term $\beta_3\space height \times sex$ is called interaction term.
When $sex=0$, $\beta_3$ has no effect to the weight.

``` r
intm <- lm(weight ~ height+male+height:male,data=df)
visreg::visreg(intm,xvar="height",by="male",overlay=T)
```

![](mixed-variable-linear-regression_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

calculation by hand the plot become:

``` r
# obtaining predictions based on the m2
d.pred$weight2 <- predict(intm, newdata = d.pred)

# obtaining the standard errors for those predictions:
d.pred$weight.se2 <- predict(intm, newdata = d.pred, se.fit = T)$se

ggplot(data = df, aes(x = height, y = weight)) +
  geom_point(aes(group = male, col = factor(male))) +
  geom_ribbon(
    data = d.pred,
    aes(
      x = height,
      ymin = weight2 - 1.96 * weight.se2,
      ymax = weight2 + 1.96 * weight.se2,
      group = male,
      fill = factor(male)
    ),
    alpha = .3
  ) +
  geom_line(data = d.pred,
            aes(
              x = height,
              y = weight2,
              group = male,
              col = factor(male)
            ),
            linewidth = 1.2)
```

![](mixed-variable-linear-regression_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
summary(intm)
```

    ## 
    ## Call:
    ## lm(formula = weight ~ height + male + height:male, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.9237  -3.0772  -0.2825   2.7808  14.4599 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -56.45562    9.17335  -6.154 2.08e-09 ***
    ## height        0.65726    0.06132  10.719  < 2e-16 ***
    ## male          6.45255   12.75816   0.506    0.613    
    ## height:male  -0.04243    0.08254  -0.514    0.608    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.252 on 348 degrees of freedom
    ## Multiple R-squared:   0.57,  Adjusted R-squared:  0.5663 
    ## F-statistic: 153.8 on 3 and 348 DF,  p-value: < 2.2e-16

We can see that the slopes for males is a bit smaller than that of
female. The difference between slope is $\beta_3 = -0.04243$. However,
the difference is not significant (p value \> 0.05)

## Common model vs Individual models

First, regression model assumes that the measurement error
$\varepsilon_i$ are normally distributed with common variance. With
individual models, we allow this variance to be different for men and
women.

Secondly, separating the dataset does not allow comparison betwenn slope
coefficients. Whereas fitting common model allows us to say that the
effect of height on weight was on average 42g/cm smaller for men than
for women (p=0.608)

Remember to check the assumptions for the analysis.
