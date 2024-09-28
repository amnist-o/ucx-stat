Poisson assessment
================
2024-09-29

## Assessment

Let’s continue analysing LLGA data. Let’s include body mass index (BMI)
into our modeling. It is only available for a small subset of people
though, so let’s make our dataset smaller:

``` r
library(tidyverse)
df <- read.csv("datasets/LLGA.csv",stringsAsFactors = T)
df %>%
  mutate(# only return
         return = Freq -1,
         # change duration to 1 year and less
         duration = as.numeric(dmy("1/4/2016")-dmy(StartDate)),
         duration = pmin(duration, 365)) %>%
  filter(return >0,
         duration >0,
         !is.na(BMI),
         Gender %in% c('F','M')) -> d

summary(d)
```

    ##   memMemberNo    Gender        Age             StartDate         BMI       
    ##  Min.   :   11    :   0   Min.   :16.00   5/01/2016 :  21   Min.   :12.02  
    ##  1st Qu.:10985   F:1432   1st Qu.:27.00   25/01/2016:  19   1st Qu.:22.48  
    ##  Median :12794   M: 984   Median :35.00   27/07/2015:  19   Median :25.44  
    ##  Mean   :12162   U:   0   Mean   :38.44   9/06/2015 :  19   Mean   :26.56  
    ##  3rd Qu.:14640            3rd Qu.:48.00   20/07/2015:  18   3rd Qu.:29.55  
    ##  Max.   :19058            Max.   :87.00   21/06/2015:  18   Max.   :57.36  
    ##                                           (Other)   :2302                  
    ##       Freq            return          duration    
    ##  Min.   :  2.00   Min.   :  1.00   Min.   :  2.0  
    ##  1st Qu.:  2.00   1st Qu.:  1.00   1st Qu.:136.8  
    ##  Median :  4.00   Median :  3.00   Median :233.0  
    ##  Mean   :  7.73   Mean   :  6.73   Mean   :215.8  
    ##  3rd Qu.:  8.00   3rd Qu.:  7.00   3rd Qu.:300.0  
    ##  Max.   :124.00   Max.   :123.00   Max.   :365.0  
    ## 

Check whether there is an affect of BMI on attendance after adjusting
for age and gender. Do not forget to include the duration as an offset,
and start with the most complex model:
`m2 <- glm(I(Freq - 1) ~ BMI*Age*Gender+offset(log(duration)), data = d, family = "poisson"(link = "log))`

``` r
m <- glm(return ~ BMI*Age*Gender+offset(log(duration)),
         data = d,
         family = "poisson"(link = "log"))
summary(m)
```

    ## 
    ## Call:
    ## glm(formula = return ~ BMI * Age * Gender + offset(log(duration)), 
    ##     family = poisson(link = "log"), data = d)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -4.1376865  0.1457188 -28.395  < 2e-16 ***
    ## BMI             -0.0141824  0.0054043  -2.624 0.008683 ** 
    ## Age              0.0097137  0.0034525   2.814 0.004900 ** 
    ## GenderM         -0.1965468  0.2185972  -0.899 0.368585    
    ## BMI:Age          0.0004083  0.0001254   3.257 0.001126 ** 
    ## BMI:GenderM      0.0289237  0.0082480   3.507 0.000454 ***
    ## Age:GenderM      0.0052535  0.0051028   1.030 0.303230    
    ## BMI:Age:GenderM -0.0002967  0.0001884  -1.575 0.115200    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 24974  on 2415  degrees of freedom
    ## Residual deviance: 22387  on 2408  degrees of freedom
    ## AIC: 29989
    ## 
    ## Number of Fisher Scoring iterations: 6

Use this model to produce and plot predictions for the return visit
frequency for  
- 45 years old male with various values of BMI  
- 75 years old female with various values of BMI

``` r
data.frame(BMI = rep(15:35,each = 2),
           Gender = rep(c("F","M"),21),
           duration = 365) %>%
  mutate(Age = ifelse(Gender == "F",75,45),.before = 1) %>%
  bind_cols(predict(m,newdata = .,
                    type = "response",
                    se.fit = T)) %>%
  select(-residual.scale) %>%
  mutate(lo = fit - (1.96*se.fit),
         hi = fit + (1.96*se.fit)) -> d.pred

d.pred %>%
  ggplot(aes(x = BMI, y = fit))+
  geom_line(aes(group = Gender, col = Gender),linewidth = 1)+
  geom_ribbon(aes(ymin = lo, ymax = hi,
                  group = Gender, fill = Gender),
              alpha = .4)
```

![](poisson_files/fig-assessment/prediction%20and%20plot-1.png)<!-- -->

The results may be explained by that the time 45 years old male have to
spend is less than 75 years old female. On the other hand, 75 years old
female might not be able to visit the place that often and the result is
due to high visits in younger age.
