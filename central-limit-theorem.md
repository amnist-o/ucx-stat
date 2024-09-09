Central Limit Theorem
================
Amnist.O
2024-09-09

# Assessment:

The dataset EucalyptusTtest.csv contains information on heights (m) of
trees of two species of Eucalyptus trees: E. cloeziana and E. dunnii.

Do some exploratory analysis:

- Evaluate sample statistics for each species.
- Produce a histogram/density plot for each species. Use the Central
  Limit Theorem to produce
- the estimate
- the standard error
- the 95% confidence interval for the population mean height.

<!-- -->

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
df %>%
  mutate(spp = factor(spp)) -> df

summary(df)
```

    ##   spp          hgt       
    ##  clo:60   Min.   :10.50  
    ##  dun:60   1st Qu.:13.70  
    ##           Median :14.70  
    ##           Mean   :14.68  
    ##           3rd Qu.:15.40  
    ##           Max.   :23.10

``` r
df %>%
  reframe(across(hgt,list(mean=mean,sd=sd,n=length)),.by = spp) %>%
            mutate(se = hgt_sd/sqrt(hgt_n),
                   lo95 = hgt_mean-(1.96*se),
                   hi95 = hgt_mean+(1.96*se)) %>%
  column_to_rownames("spp") %>%
  t() %>%
  data.frame()
```

    ##                 clo        dun
    ## hgt_mean 14.9100000 14.4516667
    ## hgt_sd    2.3518979  1.1541365
    ## hgt_n    60.0000000 60.0000000
    ## se        0.3036287  0.1489984
    ## lo95     14.3148877 14.1596298
    ## hi95     15.5051123 14.7437035

Consider a seed germination experiment, in which 15 out of 20 seeds have
germinated.

Use CLT to produce:

- The estimate for the population proportion
- The associated standard error
- The 95% confidence interval

``` r
seed <- c(rep(1,15),rep(0,5))

cat("sample size =",length(seed),"\n")
```

    ## sample size = 20

``` r
cat("sd = ",sd(seed),"\n")
```

    ## sd =  0.4442617

``` r
cat("mean = ",mean(seed),"\n")
```

    ## mean =  0.75

``` r
cat("se = ",sd(seed)/sqrt(length(seed)),"\n")
```

    ## se =  0.09933993

``` r
cat("lo95 = ",mean(seed)-(1.96*sd(seed)/sqrt(length(seed))),"\n")
```

    ## lo95 =  0.5552937

``` r
cat("hi95 = ",mean(seed)+(1.96*sd(seed)/sqrt(length(seed))),"\n")
```

    ## hi95 =  0.9447063
