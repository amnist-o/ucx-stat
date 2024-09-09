1-way ANOVA
================
Amnist.O
2024-09-09

# Assessment:

The file EucalyptiANOVA1.csv contains information on heights recorded
for the trees of three species of Eucalyptus: E. cloeziana, E. pilularis
and E. dunnii.

Perform ANOVA to test whether the average population height for at least
one of the species is different from the other two.

``` r
df <- read.csv("datasets/EucalyptiANOVA1.csv")
df$spp <- factor(df$spp)
sample_n(df,5)
```

    ##   spp  hgt
    ## 1 dun 16.0
    ## 2 dun 14.7
    ## 3 pil 15.8
    ## 4 clo 14.2
    ## 5 dun 13.4

- Which sample has the highest mean height?

``` r
df %>%
  reframe(across(hgt,list(n=length,mean=mean,sd=sd)),
          .by=spp)
```

    ##   spp hgt_n hgt_mean   hgt_sd
    ## 1 clo    60 14.91000 2.351898
    ## 2 dun    60 14.45167 1.154137
    ## 3 pil    60 15.19000 1.443712

Sample with highest mean is pil with height mean of 15.19 meters.

- Do the residuals appear normally distributed?

``` r
m1 <- lm(hgt ~ spp - 1,data = df)
plot(m1,which = 2)
```

![](1way-anova_files/figure-gfm/residual-1.png)<!-- --> From the plot,
the residuals is not normally distributed at the tails.

- What is the p-value (4 decimals) of your F-test (number entry)?

``` r
summary(update(m1,.~.+1))
```

    ## 
    ## Call:
    ## lm(formula = hgt ~ spp, data = df)
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

``` r
anova(update(m1,.~.+1))
```

    ## Analysis of Variance Table
    ## 
    ## Response: hgt
    ##            Df Sum Sq Mean Sq F value  Pr(>F)  
    ## spp         2  16.67  8.3361  2.7949 0.06382 .
    ## Residuals 177 527.92  2.9826                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Number entry means include coefficient to the linear regression. From
`anova`, we get p-value of the F test at 4 decimals equals to 0.0638204
