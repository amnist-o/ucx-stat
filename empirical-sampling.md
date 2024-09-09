Empirical Sampling
================
Amnist.O
2024-09-09

# Assessment

Sample the height of eucalyptus size of 5 and 20, 1000 times. Analyze
the mean, median, standard deviation, and quantiles of each sampling.

``` r
df <- read.csv("datasets/Eucalyptus1_.csv")
sample.df.5 <- list()
sample.df.20 <- list()
set.seed(1)
for (i in 1:1000) {
  sample.df.5[[i]] <- sample(df$hgt, size = 5, replace = F)
  sample.df.20[[i]] <- sample(df$hgt, size = 20, replace = F)
}
```

After we have sampled for 1000 times of size 5 and 20, we find the mean,
median, sd, and quantiles of the samples.

``` r
rbind(sapply(sample.df.5,function(x) c(mean(x),median(x),sd(x),sum(x>15)/10,quantile(x,c(.025,.975)))),
      sapply(sample.df.20,function(x) c(mean(x),median(x),sd(x),sum(x>15)/10,quantile(x,c(.025,.975))))) -> sample.df
dimnames(sample.df) <- list(paste0(c("mean","median","sd","15m+","2.5%","97.5%"),rep(c(".5",".20"),each=6)))
as.data.frame(t(sample.df)) -> sample.df
```

Now, we plot the statistic of each sampling

``` r
meanplot1 <- hist(
  sample.df$mean.5,
  main = "",
  xlab = "mean height, m.",
  col = "dodgerblue"
)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-1.png)<!-- -->

``` r
meanplot2 <- hist(sample.df$mean.20, col = "orange2")
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-2.png)<!-- -->

``` r
plot(
  meanplot1,
  main = "sample mean",
  sub = "sample size 5 is blue, sample size 20 is orange",
  xlab = "mean height, m.",
  col = "dodgerblue"
)
plot(meanplot2, add = T, col = "orange2")
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-3.png)<!-- -->

``` r
medianplot1 <- hist(sample.df$median.5)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-4.png)<!-- -->

``` r
medianplot2 <- hist(sample.df$median.20)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-5.png)<!-- -->

``` r
plot(
  medianplot1,
  main = "sample median",
  sub = "sample size 5 is blue, sample size 20 is orange",
  xlab = "height, m.",
  col = "dodgerblue"
)
plot(medianplot2, col = "orange2", add = T)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-6.png)<!-- -->

``` r
sdplot1 <- hist(sample.df$sd.5)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-7.png)<!-- -->

``` r
sdplot2 <- hist(sample.df$sd.20)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-8.png)<!-- -->

``` r
plot(
  sdplot1,
  main = "sample sd",
  sub = "sample size 5 is blue, sample size 20 is orange",
  xlab = "height, m.",
  col = "dodgerblue"
)
plot(sdplot2, col = "orange2", add = T)
```

![](empirical-sampling_files/figure-gfm/sampling%20plot-9.png)<!-- -->
