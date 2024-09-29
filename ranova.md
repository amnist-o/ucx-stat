Repeated Measures ANOVA
================
2024-09-29

## Assessment

- Load the eucalyptus.csv data and repeat the analysis of the previous
  section with the diameter at breast height `dbh` rather than height
  `hgt` as the response variable.

``` r
library(tidyverse)
library(lme4)
library(lmerTest)
# load data
df <- read.csv("datasets/eucalyptus.csv")
df$stocking <- factor(df$stocking)
```

``` r
m <- lmer(dbh ~ spp*stocking + (1|plot), data = df)
m1 <- lmer(dbh ~ stocking + (1|plot), data = df)
anova(m,m1)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df
    ## Models:
    ## m1: dbh ~ stocking + (1 | plot)
    ## m: dbh ~ spp * stocking + (1 | plot)
    ##    npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## m1    6 3093.4 3120.8 -1540.7   3081.4                         
    ## m    14 3067.7 3131.8 -1519.9   3039.7 41.623  8  1.594e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The anova test shows that the effect of stocking is statistically
significant on dbh after adjusting for species.

``` r
s <- lmer(dbh ~ spp+(1|plot),data = df)
anova(m,s)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df
    ## Models:
    ## s: dbh ~ spp + (1 | plot)
    ## m: dbh ~ spp * stocking + (1 | plot)
    ##   npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
    ## s    5 3102.7 3125.6 -1546.3   3092.7                         
    ## m   14 3067.7 3131.8 -1519.9   3039.7 52.925  9   3.02e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The anova test shows that the effect of species is statistically
significant on dbh after adjusting for stocking.

``` r
m.add <- update(m,.~spp+stocking+(1|plot))
anova(m,m.add)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df
    ## Models:
    ## m.add: dbh ~ spp + stocking + (1 | plot)
    ## m: dbh ~ spp * stocking + (1 | plot)
    ##       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
    ## m.add    8 3075.0 3111.7 -1529.5   3059.0                        
    ## m       14 3067.7 3131.8 -1519.9   3039.7 19.285  6   0.003709 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The anova test shows that the effect of stocking differed by species.
