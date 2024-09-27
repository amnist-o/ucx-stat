Probit link function
================
2024-09-28

## Assessment

There is another link function: probit. Probit is an inverse of the
standard cumulative normal density function:

$$
\Phi(p) = \int_0^p \frac{1}{\sqrt{2\pi}}e^{-\frac{1}{2}x^2}dx
$$ 

We can plot using `curve(qnorm(x),from = 0, to = 1, lwd = 2)`

``` r
curve(qnorm(x),from = 0, to = 1, lwd = 2)
```

![](probit_files/figure-gfm/probit%20plot-1.png)<!-- -->

while logit looks like:

``` r
curve(log(x/(1-x)), from = 0, to = 1, lwd = 2, ylab = "logit")
```

![](probit_files/figure-gfm/plot%20logit-1.png)<!-- -->

plotting both together makes:

``` r
curve(qnorm(x), from = 0, to = 1, lwd = 2, col = "salmon",ylab = "y")
curve(log(x/(1-x)), from = 0, to = 1, lwd = 2, col = "blue", add = T)
legend(x = 0.02, y = 2, legend = c("probit","logit"), col = c("salmon","blue"), lwd = 2)
title("probit vs logit curves")
```

![](probit_files/figure-gfm/plot%20together-1.png)<!-- -->

## Repeating the ANCOVA

Repeat the ANCOVA analysis to test whether the probability of winning
depends on the Chaser after adjusting for the team score. Use probit
instead of logit as a link function.

``` r
library(tidyverse)
chase <- read_lines(file = "datasets/chase")
df <- read.table(file = chase, header = T, sep = ",", stringsAsFactors = T)
```

``` r
p <- glm(Win ~ TeamScore * Chaser, data = df,
         family = "binomial"(link = "probit"))
m <- update(p, family = "binomial"(link = "logit"))
modelsummary::msummary(list(logit = m,probit = p), stars = T)
```

<table style="width:69%;">
<colgroup>
<col style="width: 36%" />
<col style="width: 16%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>logit</th>
<th>probit</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>(Intercept)</td>
<td>-7.707***</td>
<td>-4.553***</td>
</tr>
<tr class="even">
<td></td>
<td>(1.195)</td>
<td>(0.653)</td>
</tr>
<tr class="odd">
<td>TeamScore</td>
<td>0.364***</td>
<td>0.215***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.063)</td>
<td>(0.035)</td>
</tr>
<tr class="odd">
<td>ChaserJenny</td>
<td>-3.303</td>
<td>-1.871</td>
</tr>
<tr class="even">
<td></td>
<td>(4.528)</td>
<td>(2.532)</td>
</tr>
<tr class="odd">
<td>ChaserMark</td>
<td>-1.287</td>
<td>-0.709</td>
</tr>
<tr class="even">
<td></td>
<td>(1.753)</td>
<td>(0.949)</td>
</tr>
<tr class="odd">
<td>ChaserPaul</td>
<td>-0.891</td>
<td>-0.400</td>
</tr>
<tr class="even">
<td></td>
<td>(1.804)</td>
<td>(0.976)</td>
</tr>
<tr class="odd">
<td>ChaserShaun</td>
<td>1.800</td>
<td>0.948</td>
</tr>
<tr class="even">
<td></td>
<td>(1.483)</td>
<td>(0.815)</td>
</tr>
<tr class="odd">
<td>TeamScore × ChaserJenny</td>
<td>0.119</td>
<td>0.067</td>
</tr>
<tr class="even">
<td></td>
<td>(0.231)</td>
<td>(0.132)</td>
</tr>
<tr class="odd">
<td>TeamScore × ChaserMark</td>
<td>0.058</td>
<td>0.032</td>
</tr>
<tr class="even">
<td></td>
<td>(0.092)</td>
<td>(0.050)</td>
</tr>
<tr class="odd">
<td>TeamScore × ChaserPaul</td>
<td>0.056</td>
<td>0.027</td>
</tr>
<tr class="even">
<td></td>
<td>(0.097)</td>
<td>(0.053)</td>
</tr>
<tr class="odd">
<td>TeamScore × ChaserShaun</td>
<td>-0.050</td>
<td>-0.023</td>
</tr>
<tr class="even">
<td></td>
<td>(0.081)</td>
<td>(0.045)</td>
</tr>
<tr class="odd">
<td>Num.Obs.</td>
<td>966</td>
<td>966</td>
</tr>
<tr class="even">
<td>AIC</td>
<td>877.3</td>
<td>873.1</td>
</tr>
<tr class="odd">
<td>BIC</td>
<td>926.0</td>
<td>921.8</td>
</tr>
<tr class="even">
<td>Log.Lik.</td>
<td>-428.648</td>
<td>-426.550</td>
</tr>
<tr class="odd">
<td>F</td>
<td>17.359</td>
<td>19.901</td>
</tr>
<tr class="even">
<td>RMSE</td>
<td>0.38</td>
<td>0.38</td>
</tr>
</tbody><tfoot>
<tr class="odd">
<td colspan="3"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
# AER logit
cat("AER logit\n")
```

    ## AER logit

``` r
table(df$Win,predict(m,type = "response")>.5,dnn = list("Actual","Predicted"))
```

    ##       Predicted
    ## Actual FALSE TRUE
    ##      0   661   58
    ##      1   162   85

``` r
#AER probit
cat("AER probit\n")
```

    ## AER probit

``` r
table(df$Win,predict(p,type = "response")>.5,dnn = list("Actual","Predicted"))
```

    ##       Predicted
    ## Actual FALSE TRUE
    ##      0   661   58
    ##      1   162   85

Once you have completed your analysis, please answer the following
questions: Are the ANCOVA results qualitatively different?

``` r
anova(m,update(m,.~TeamScore),test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Win ~ TeamScore * Chaser
    ## Model 2: Win ~ TeamScore
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1       956     857.30                          
    ## 2       964     891.61 -8  -34.317 3.559e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(p,update(p,.~TeamScore),test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Win ~ TeamScore * Chaser
    ## Model 2: Win ~ TeamScore
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1       956     853.10                          
    ## 2       964     888.38 -8   -35.28 2.377e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

- We can see from the ancova that the probability of winning does not
  depend on chaser after adjusting team score in both logit model and
  probit model.

What method can be used to compare the two models with different link
functions
 - Since the response is the same, we can use AIC to compare
between models. Also, AER can also be used to compare since it measures
the correctness of the models.

Which model is statistically better?
 - From the AIC (the lesser, the better), we can state that probit model is better
($873.1000532<877.295919$).

What is the AIC for the GLM with the probit link function? - AIC of the
probit link function is 873.1000532 for additive model and 873.1000532
for interaction model (question refers to interaction model).

Plot the curves for chaser Anne based on both models

``` r
curve(1/(1+exp(-(coef(m)[1]+coef(m)[2]*x))),from = 0, to = 30,lwd = 2, col = "salmon",
      ylab = "Winning probability")
lines(x = seq(0,30,0.5),
      y = predict(p,list(TeamScore = seq(0,30,0.5),Chaser = rep("Anne",61)),type = "response"),
      col = "blue", lwd = 2, lty = 2)
title("logit vs probit winning probability against chaser Anne")
legend(x = 0.02, y= 0.975,
       legend = c("logit","probit"),
       col = c("blue","salmon"),
       lwd = 2,
       lty = c(2,1))
```

![](probit_files/figure-gfm/curve%20plot-1.png)<!-- -->

- They look almost the same.
