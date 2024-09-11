Linear regression
================
Amnist.O
2024-09-10

## Assessment: Interpret linear regression

A statistician has analysed correlation between height (cm) and weight
(kg) and found the following linear model: Expected Weight = -25 +
0.5\*Height or
$$\mathbb{E}(\text{Weight}) = -25 + 0.5 \cdot \text{Height}$$

It means  
1. each cm of height increased is associated (not cause) with an average
(since it is $\mathbb{E}(Y|X)$ ) 500 g increased in weight.

2.  expected weight of a 2 meters tall person is $-25 + 0.5 \times 200$
    which is 75 kg.

3.  relationship between height and weight is approximately linear
    (according to this analysis)

## Assessment: Linearisation

Consider the following model: $$Y=ae^{bX}$$, where $X$ and $Y$ are
observed variables and $a$ and $b$ are coefficients to be estimated.

What is the linearisation of the model?

$$
\begin{aligned}
Y &= ae^{bX} \\
log(Y) &= log(ae^{bX}) \\
&= log(a)+log(e^{bX}) \\
&= log(a)+bX
\end{aligned}
$$

## Assessment: Checking assumptions

What can Residuals vs Fitted plot tell you?

| Pattern                                                           | What the pattern may indicate |
|-------------------------------------------------------------------|-------------------------------|
| Fanning or uneven spreading of residuals across fitted values     | Nonconstant variance          |
| Curvilinear                                                       | A missing higher-order term   |
| A point that is far away from zero                                | An outlier                    |
| A point that is far away from the other points in the x-direction | An influential point          |

[source](https://support.minitab.com/en-us/minitab/help-and-how-to/statistical-modeling/regression/how-to/fitted-line-plot/interpret-the-results/all-statistics-and-graphs/residual-plots/#residuals-versus-fits)

## Assessment: Applying Linear Regression

1.  Load the dataset howell1.csv.
2.  Select only the people above 18 years of age.
3.  After a brief exploratory analysis, fit the regression model of
    weight vs. height for men and women separately.
4.  Check the assumptions.
5.  Produce plots of the fitted models.
6.  Interpret the coefficients.

``` r
library(tidyverse)
```

Exploratory analysis

``` r
df <- read.csv("datasets/howell1.csv",sep=";")
df %>%
  filter(age >= 18) -> df
```

Regression based on sex

``` r
m1 <- lm(weight ~ height, data = subset(df, male == 1))
m2 <- update(m1, data = subset(df, male == 0))

modelsummary::msummary(list(male = m1,female = m2))
```

|             | male     | female   |
|-------------|----------|----------|
| (Intercept) | -50.003  | -56.456  |
|             | (8.901)  | (9.142)  |
| height      | 0.615    | 0.657    |
|             | (0.055)  | (0.061)  |
| Num.Obs.    | 165      | 187      |
| R2          | 0.430    | 0.385    |
| R2 Adj.     | 0.426    | 0.381    |
| AIC         | 951.2    | 1074.7   |
| BIC         | 960.5    | 1084.4   |
| Log.Lik.    | -472.578 | -534.367 |
| F           | 122.865  | 115.677  |
| RMSE        | 4.24     | 4.21     |

Assumptions check

``` r
par(mfrow = c(2,4))
plot(m1)
plot(m2)
```

![](linear-regression_files/figure-gfm/resid%20plot-1.png)<!-- -->

Plot the fitted model

``` r
plot(df$height,df$weight,
     xlab = "Height (cm)",
     ylab = "Weight (kg)",
     pch =16)
curve(m1$coefficients[1]+m1$coefficients[2]*x,
      add = T,
      col = "navyblue",lwd = 2)
curve(m2$coefficients[1]+m2$coefficients[2]*x,
      add=T, col = "plum", lwd = 2,lty=2)
```

![](linear-regression_files/figure-gfm/plot-1.png)<!-- -->

Interpret the coefficients

1 cm additional height is associated with average 615g increase in
weight for male while the same additional height is associated with
average 657g increase in weight for female.
