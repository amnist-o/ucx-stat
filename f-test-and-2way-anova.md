F-test and 2-way ANOVA
================
2024-09-11

## Assessment:

Consider the data-set CuCC.csv. These data have been collected as part
of the experiment to study the effect of copper (Cu) concentration on
the growth of certain toxins.

The data have been kindly provided by Dr. Francine Harland and is
further described in Harland FM, Wood SA, Moltchanova E, Williamson WM,
Gaw S. Phormidium autumnale growth and anatoxin-a production under iron
and copper stress. Toxins. 2013 Dec;5(12):2504-21..

The three variables in the data set are the copper concentration (Cu),
day of the experiment, and the bacterial cell count (cc). The three
levels of copper concentrations were 2.5 ppb, 25 ppb and 250 ppb
respectively.

``` r
library(tidyverse)
```

``` r
dat <- read.csv("datasets/CuCC.csv")

dat %>%
  ggplot(aes(x=Day,y=CC))+
  geom_point(aes(col = Cu))
```

![](f-test-and-2way-anova_files/figure-gfm/load%20data-1.png)<!-- -->

In this analysis we will treat `Day` as a categorical variable, rather
than a continuous one. Basically, we are saying that there is no
parametric function suitable to describing the growth trajectory.

``` r
dat %>%
  mutate(d = factor(Day)) -> dat
```

## Research Question:

The main question is: does copper concentration (statistically
significantly) affect growth trajectory?

The secondary question is: Does the highest copper concentration (Cu100
in the data file) provide statistically significantly smaller counts
than either of the other two concentrations on day 49?

### main question

We should ran regression with output `CC` and input `Day` and `Cu` to
see if copper concentration `Cu` affect the growth of bacteria `CC`.

1.  growth of bateria on days
2.  growth of bateria on days with shift in trajectory based on Cu
3.  growth of bateria on days with trajectory (slope) different for each
    Cu

Since we are interested in the growth of bateria, we can use
log-transform on `CC` for scaling and easier interpretation.

``` r
m1 <- lm(log(CC) ~ d, data = dat)
m2 <- update(m1,.~.+Cu)
m3 <- update(m1,.~d*Cu)
modelsummary::msummary(list(day=m1,additive = m2, interaction = m3),stars = T)
```

<table style="width:75%;">
<colgroup>
<col style="width: 22%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>day</th>
<th>additive</th>
<th>interaction</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>(Intercept)</td>
<td>13.896***</td>
<td>14.081***</td>
<td>14.308***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.257)</td>
<td>(0.237)</td>
<td>(0.277)</td>
</tr>
<tr class="odd">
<td>d3</td>
<td>0.301</td>
<td>0.301</td>
<td>0.403</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d6</td>
<td>0.983**</td>
<td>0.983**</td>
<td>0.356</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d9</td>
<td>1.330***</td>
<td>1.330***</td>
<td>0.355</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d12</td>
<td>1.458***</td>
<td>1.458***</td>
<td>1.149**</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d17</td>
<td>2.198***</td>
<td>2.198***</td>
<td>1.663***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d22</td>
<td>2.097***</td>
<td>2.097***</td>
<td>1.374***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d32</td>
<td>2.095***</td>
<td>2.095***</td>
<td>2.041***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d41</td>
<td>2.543***</td>
<td>2.543***</td>
<td>2.989***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d49</td>
<td>2.625***</td>
<td>2.625***</td>
<td>3.028***</td>
</tr>
<tr class="even">
<td></td>
<td>(0.363)</td>
<td>(0.307)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>CuCu10</td>
<td></td>
<td>0.186</td>
<td>-0.438</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td>(0.168)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>CuCu100</td>
<td></td>
<td>-0.741***</td>
<td>-0.798*</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td>(0.168)</td>
<td>(0.391)</td>
</tr>
<tr class="odd">
<td>d3 × CuCu10</td>
<td></td>
<td></td>
<td>-0.307</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d6 × CuCu10</td>
<td></td>
<td></td>
<td>1.505**</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d9 × CuCu10</td>
<td></td>
<td></td>
<td>1.576**</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d12 × CuCu10</td>
<td></td>
<td></td>
<td>0.295</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d17 × CuCu10</td>
<td></td>
<td></td>
<td>0.668</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d22 × CuCu10</td>
<td></td>
<td></td>
<td>1.404*</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d32 × CuCu10</td>
<td></td>
<td></td>
<td>1.167*</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d41 × CuCu10</td>
<td></td>
<td></td>
<td>0.004</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d49 × CuCu10</td>
<td></td>
<td></td>
<td>-0.066</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d3 × CuCu100</td>
<td></td>
<td></td>
<td>0.002</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d6 × CuCu100</td>
<td></td>
<td></td>
<td>0.376</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d9 × CuCu100</td>
<td></td>
<td></td>
<td>1.347*</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d12 × CuCu100</td>
<td></td>
<td></td>
<td>0.631</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d17 × CuCu100</td>
<td></td>
<td></td>
<td>0.937+</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d22 × CuCu100</td>
<td></td>
<td></td>
<td>0.765</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d32 × CuCu100</td>
<td></td>
<td></td>
<td>-1.007+</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d41 × CuCu100</td>
<td></td>
<td></td>
<td>-1.344*</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>d49 × CuCu100</td>
<td></td>
<td></td>
<td>-1.143*</td>
</tr>
<tr class="even">
<td></td>
<td></td>
<td></td>
<td>(0.553)</td>
</tr>
<tr class="odd">
<td>Num.Obs.</td>
<td>90</td>
<td>90</td>
<td>90</td>
</tr>
<tr class="even">
<td>R2</td>
<td>0.587</td>
<td>0.713</td>
<td>0.880</td>
</tr>
<tr class="odd">
<td>R2 Adj.</td>
<td>0.541</td>
<td>0.672</td>
<td>0.822</td>
</tr>
<tr class="even">
<td>AIC</td>
<td>3002.4</td>
<td>2973.7</td>
<td>2931.2</td>
</tr>
<tr class="odd">
<td>BIC</td>
<td>3029.9</td>
<td>3006.2</td>
<td>3008.7</td>
</tr>
<tr class="even">
<td>Log.Lik.</td>
<td>-98.893</td>
<td>-82.543</td>
<td>-43.253</td>
</tr>
<tr class="odd">
<td>F</td>
<td>12.639</td>
<td>17.606</td>
<td>15.185</td>
</tr>
<tr class="even">
<td>RMSE</td>
<td>0.73</td>
<td>0.61</td>
<td>0.39</td>
</tr>
</tbody><tfoot>
<tr class="odd">
<td colspan="4"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

Let’s plot the regression to diagnose any problem

``` r
par(mfrow=c(3,4))
plot(m1)
plot(m2)
plot(m3)
```

![](f-test-and-2way-anova_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Now let’s use F-test to test the addition variance explain of each
model.

``` r
anova(m1,m2,m3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: log(CC) ~ d
    ## Model 2: log(CC) ~ d + Cu
    ## Model 3: log(CC) ~ d + Cu + d:Cu
    ##   Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
    ## 1     80 47.444                                   
    ## 2     78 32.991  2    14.454 31.4698 4.506e-10 ***
    ## 3     60 13.779 18    19.212  4.6479 3.318e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The result shows that the interaction term model is best describe the
growth of bateria since the sum of sq is the highest.

## secondary question

We set reference level at day 49 by using `relevel`

``` r
summary(update(m3,.~relevel(d,"49")*Cu))
```

    ## 
    ## Call:
    ## lm(formula = log(CC) ~ relevel(d, "49") + Cu + relevel(d, "49"):Cu, 
    ##     data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3838 -0.2594  0.0248  0.2741  1.0550 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                17.33634    0.27667  62.660  < 2e-16 ***
    ## relevel(d, "49")0          -3.02804    0.39127  -7.739 1.37e-10 ***
    ## relevel(d, "49")3          -2.62510    0.39127  -6.709 7.82e-09 ***
    ## relevel(d, "49")6          -2.67228    0.39127  -6.830 4.88e-09 ***
    ## relevel(d, "49")9          -2.67289    0.39127  -6.831 4.85e-09 ***
    ## relevel(d, "49")12         -1.87858    0.39127  -4.801 1.09e-05 ***
    ## relevel(d, "49")17         -1.36460    0.39127  -3.488 0.000919 ***
    ## relevel(d, "49")22         -1.65400    0.39127  -4.227 8.19e-05 ***
    ## relevel(d, "49")32         -0.98709    0.39127  -2.523 0.014312 *  
    ## relevel(d, "49")41         -0.03891    0.39127  -0.099 0.921120    
    ## CuCu10                     -0.50459    0.39127  -1.290 0.202136    
    ## CuCu100                    -1.94071    0.39127  -4.960 6.12e-06 ***
    ## relevel(d, "49")0:CuCu10    0.06627    0.55334   0.120 0.905067    
    ## relevel(d, "49")3:CuCu10   -0.24051    0.55334  -0.435 0.665382    
    ## relevel(d, "49")6:CuCu10    1.57111    0.55334   2.839 0.006163 ** 
    ## relevel(d, "49")9:CuCu10    1.64244    0.55334   2.968 0.004299 ** 
    ## relevel(d, "49")12:CuCu10   0.36173    0.55334   0.654 0.515793    
    ## relevel(d, "49")17:CuCu10   0.73422    0.55334   1.327 0.189579    
    ## relevel(d, "49")22:CuCu10   1.47061    0.55334   2.658 0.010069 *  
    ## relevel(d, "49")32:CuCu10   1.23375    0.55334   2.230 0.029527 *  
    ## relevel(d, "49")41:CuCu10   0.07025    0.55334   0.127 0.899399    
    ## relevel(d, "49")0:CuCu100   1.14280    0.55334   2.065 0.043226 *  
    ## relevel(d, "49")3:CuCu100   1.14518    0.55334   2.070 0.042806 *  
    ## relevel(d, "49")6:CuCu100   1.51870    0.55334   2.745 0.007981 ** 
    ## relevel(d, "49")9:CuCu100   2.48988    0.55334   4.500 3.18e-05 ***
    ## relevel(d, "49")12:CuCu100  1.77364    0.55334   3.205 0.002163 ** 
    ## relevel(d, "49")17:CuCu100  2.07953    0.55334   3.758 0.000390 ***
    ## relevel(d, "49")22:CuCu100  1.90778    0.55334   3.448 0.001039 ** 
    ## relevel(d, "49")32:CuCu100  0.13623    0.55334   0.246 0.806373    
    ## relevel(d, "49")41:CuCu100 -0.20106    0.55334  -0.363 0.717622    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4792 on 60 degrees of freedom
    ## Multiple R-squared:  0.8801, Adjusted R-squared:  0.8221 
    ## F-statistic: 15.18 on 29 and 60 DF,  p-value: < 2.2e-16

The coefficient of `Cu100` indicated that highest copper concentration
indeed provide statistically smaller bateria count than other two.

## Graph for estimated growth trajectory

``` r
visreg::visreg(m3,xvar = "d",by="Cu",overlay=T)
```

![](f-test-and-2way-anova_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
