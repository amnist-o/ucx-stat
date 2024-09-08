Non-Parametric Bootstrap
================
Amnist.O
2024-09-08

## Assessment:

Consider again the dataset EucalyptusTtest.csv, which contains
information on heights (m) of trees of two species of Eucalyptus trees:
E. cloeziana and E. dunnii.

In Statistical Inference: Central Limit Theorem (Unit 2, Part 4) we used
the Central Limit Theorem to produce the estimate, the standard error
and the 95% confidence interval for the population mean height.

Now use non-parametric bootstrap to do the same, and compare the
results.

You may also want to see how the number of bootstrapped samples affects
your results.

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

The statistical mean, se, and 95% CI from CLT are as followed:

``` r
df <- read.csv("datasets/EucalyptusTtest.csv")
df %>%
  mutate(spp = factor(spp)) -> df

df %>%
  reframe(across(hgt,list(mean=mean,sd=sd,n=length)),.by = spp) %>%
            mutate(se = hgt_sd/sqrt(hgt_n),
                   lo95 = hgt_mean-(1.96*se),
                   hi95 = hgt_mean+(1.96*se)) %>%
  column_to_rownames("spp") %>%
  t() %>%
  data.frame() -> clt
clt
```

    ##                 clo        dun
    ## hgt_mean 14.9100000 14.4516667
    ## hgt_sd    2.3518979  1.1541365
    ## hgt_n    60.0000000 60.0000000
    ## se        0.3036287  0.1489984
    ## lo95     14.3148877 14.1596298
    ## hi95     15.5051123 14.7437035

Now let’s try using bootstrap to find the mean, se, and 95% CI. First,
let’s try a small number of bootstrap samples: 10

``` r
bst <- function(n) {
  bs <- data.frame()
  
  set.seed(123456)
  
  for (i in 1:1000) {
    df %>%
      reframe(hgt = sample(hgt, n,replace = T), .by = spp) -> s1
    bs %>%
      bind_rows(s1 %>%
                  reframe(hgt = mean(hgt), .by = spp)) -> bs
  }
  
  bs %>%
    reframe(across(hgt, list(
      mean = mean, sd = sd, n = length
    )), .by = spp) %>%
    mutate(
      se = hgt_sd / sqrt(hgt_n),
      lo95 = hgt_mean - (1.96 * se),
      hi95 = hgt_mean + (1.96 * se)
    ) %>%
    column_to_rownames("spp") %>%
    t() %>%
    data.frame() %>%
    rename(bs.clo = clo, bs.dun = dun) %>%
    bind_cols(clt) %>%
    mutate(across(everything(),  ~ formatC(
      .x, digits = 6, format = "f"
    )))
}
bst(10)
```

    ##               bs.clo      bs.dun       clo       dun
    ## hgt_mean   14.893820   14.434350 14.910000 14.451667
    ## hgt_sd      0.730639    0.366013  2.351898  1.154137
    ## hgt_n    1000.000000 1000.000000 60.000000 60.000000
    ## se          0.023105    0.011574  0.303629  0.148998
    ## lo95       14.848535   14.411664 14.314888 14.159630
    ## hi95       14.939105   14.457036 15.505112 14.743703

Now let’s try a sample same as original $n=60$

``` r
bst(60)
```

    ##               bs.clo      bs.dun       clo       dun
    ## hgt_mean   14.904860   14.452892 14.910000 14.451667
    ## hgt_sd      0.285904    0.149769  2.351898  1.154137
    ## hgt_n    1000.000000 1000.000000 60.000000 60.000000
    ## se          0.009041    0.004736  0.303629  0.148998
    ## lo95       14.887139   14.443609 14.314888 14.159630
    ## hi95       14.922581   14.462174 15.505112 14.743703

Finally, a very large sample of $n=1000$

``` r
bst(1000)
```

    ##               bs.clo      bs.dun       clo       dun
    ## hgt_mean   14.907592   14.449844 14.910000 14.451667
    ## hgt_sd      0.071477    0.037034  2.351898  1.154137
    ## hgt_n    1000.000000 1000.000000 60.000000 60.000000
    ## se          0.002260    0.001171  0.303629  0.148998
    ## lo95       14.903161   14.447548 14.314888 14.159630
    ## hi95       14.912022   14.452139 15.505112 14.743703

Higher sample size leads to more accurate mean and variance
