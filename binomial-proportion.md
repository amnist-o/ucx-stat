Binomial Proportions
================
Amnist.O
2024-09-09

# Assessment:

Clinical trials have several stages. The new treatment is initially
tested on a very small group of subjects. If the results are promising,
it is then tested on a larger group.

In the initial stage, 10 people were given the old treatment, and
another 10 people were given the new treatment. 6 got better in the old
group and 8 in the new.

In the second stage, 100 people were given the old treatment, and
another 100 people were given the new treatment. 60 got better in the
old group and 80 in the new.

In the final stage, 1000 people were given the old treatment, and
another 1000 people were given the new treatment. 600 got better in the
old group and 800 in the new.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
matrix(
  c(.6, .8, .40, .20),
  nrow = 2,
  byrow = F,
  dimnames = list(c("old", "new"), c("T", "F"))
) -> df
df
```

    ##       T   F
    ## old 0.6 0.4
    ## new 0.8 0.2

## Comparing between statistical tests

For prop.test

``` r
for(i in c(10, 100, 1000)) {
  cat("sample size = ",i,"\n")
  print(prop.test(df * i))
}
```

    ## sample size =  10

    ## Warning in prop.test(df * i): Chi-squared approximation may be incorrect

    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 0.2381, df = 1, p-value = 0.6256
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.6919928  0.2919928
    ## sample estimates:
    ## prop 1 prop 2 
    ##    0.6    0.8 
    ## 
    ## sample size =  100 
    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 8.5952, df = 1, p-value = 0.00337
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.33395901 -0.06604099
    ## sample estimates:
    ## prop 1 prop 2 
    ##    0.6    0.8 
    ## 
    ## sample size =  1000 
    ## 
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 94.288, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.2401993 -0.1598007
    ## sample estimates:
    ## prop 1 prop 2 
    ##    0.6    0.8

For chi-squared test

``` r
for(i in c(10, 100, 1000)) {
  cat("sample size = ",i,"\n")
  print(chisq.test(df * i))
}
```

    ## sample size =  10

    ## Warning in chisq.test(df * i): Chi-squared approximation may be incorrect

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 0.2381, df = 1, p-value = 0.6256
    ## 
    ## sample size =  100 
    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 8.5952, df = 1, p-value = 0.00337
    ## 
    ## sample size =  1000 
    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 94.288, df = 1, p-value < 2.2e-16

For t-test

``` r
for(i in c(10, 100, 1000)) {
  data.frame(group = rep(c("old","new"),each=i),
             x = rep(c(0,1,0,1),c(0.4,0.6,0.2,0.8)*i))->dft
  cat("sample size = ",i,"\n")
  print(t.test(x~group,data=dft))
}
```

    ## sample size =  10 
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  x by group
    ## t = 0.94868, df = 17.308, p-value = 0.3558
    ## alternative hypothesis: true difference in means between group new and group old is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2441866  0.6441866
    ## sample estimates:
    ## mean in group new mean in group old 
    ##               0.8               0.6 
    ## 
    ## sample size =  100 
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  x by group
    ## t = 3.1464, df = 190.38, p-value = 0.001918
    ## alternative hypothesis: true difference in means between group new and group old is not equal to 0
    ## 95 percent confidence interval:
    ##  0.0746195 0.3253805
    ## sample estimates:
    ## mean in group new mean in group old 
    ##               0.8               0.6 
    ## 
    ## sample size =  1000 
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  x by group
    ## t = 9.995, df = 1921.2, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group new and group old is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1607564 0.2392436
    ## sample estimates:
    ## mean in group new mean in group old 
    ##               0.8               0.6

Find the lowest sample size with mean of success rate = 0.6 for old drug
and 0.8 for new drug that return significant result.

``` r
for(i in 10:100){
  prop.test(df*i)$p.value -> x
  if(x<0.05){
    cat("sample size = ",i)
    print(prop.test(df*i))
    break
  }
}
```

    ## sample size =  50
    ##  2-sample test for equality of proportions with continuity correction
    ## 
    ## data:  df * i
    ## X-squared = 3.8571, df = 1, p-value = 0.04953
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.395304508 -0.004695492
    ## sample estimates:
    ## prop 1 prop 2 
    ##    0.6    0.8
