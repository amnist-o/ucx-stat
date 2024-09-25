Logistic Regression
================
2024-09-25

## Assessment

There are five different chasers in “The Chase” show.

Fit individual binary logistic regression models showing how the chance
of winning depends on the Team Score when the Chaser is Shaun, and when
the Chaser is Paul. - Interpret the results. - Plot the estimated
probability curves. - Evaluate the Apparent Error Rates (AERs) - Who do
you think is the better Chaser?

``` r
library(tidyverse)
```

``` r
chase <- read_lines(file = "datasets/chase")
df <- read.table(file = chase, header = T, sep = ",",stringsAsFactors = T)
summary(df)
```

    ##    Chaser      TeamScore      ChaserScore       TimeLeft        Win        
    ##  Anne :237   Min.   : 1.00   Min.   : 3.00          :247   Min.   :0.0000  
    ##  Jenny: 43   1st Qu.:14.00   1st Qu.:14.00   (0:26) : 19   1st Qu.:0.0000  
    ##  Mark :246   Median :17.00   Median :16.00   (0:01) : 18   Median :0.0000  
    ##  Paul :210   Mean   :16.77   Mean   :15.85   (0:15) : 18   Mean   :0.2557  
    ##  Shaun:230   3rd Qu.:19.00   3rd Qu.:18.00   (0:21) : 18   3rd Qu.:1.0000  
    ##              Max.   :28.00   Max.   :27.00   (0:14) : 17   Max.   :1.0000  
    ##                                              (Other):629

Fit *individual* models based on chaser: Paul and Shaun.

``` r
mp <- glm(Win ~ TeamScore,
          data = df,subset = Chaser=="Paul",
          family = "binomial"(link = "logit"))
ms <- update(mp,subset= Chaser=="Shaun")
```

Let’s see how the TeamScore affect the chance of winning from each
model.  
When Paul is the chaser:

``` r
summary(mp)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore, family = binomial(link = "logit"), 
    ##     data = df, subset = Chaser == "Paul")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -8.59827    1.35106  -6.364 1.96e-10 ***
    ## TeamScore    0.41982    0.07282   5.765 8.16e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 237.27  on 209  degrees of freedom
    ## Residual deviance: 189.61  on 208  degrees of freedom
    ## AIC: 193.61
    ## 
    ## Number of Fisher Scoring iterations: 5

When Shaun is the chaser:

``` r
summary(ms)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore, family = binomial(link = "logit"), 
    ##     data = df, subset = Chaser == "Shaun")
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.90683    0.87833  -6.725 1.75e-11 ***
    ## TeamScore    0.31384    0.05041   6.226 4.77e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 295.93  on 229  degrees of freedom
    ## Residual deviance: 240.82  on 228  degrees of freedom
    ## AIC: 244.82
    ## 
    ## Number of Fisher Scoring iterations: 4

From the results, we can see that team score has more positive impact to
the odds of winning when the chaser is Paul. 1 unit increased in team
score increase the log odds of winning by 0.4198248 or increase in odds
ratio of 1.5216949 when the chaser is Paul. While the same score
increased only increase the log odds of winning by 0.3138413 and
increase the odds ratio of 1.3686725 when the chaser is Shaun.

Next, we plot the estimated probability curves.

``` r
plot(df$TeamScore[df$Chaser%in%c("Paul","Shaun")],
     jitter(df$Win[df$Chaser%in%c("Paul","Shaun")]),
     xlab='Team Score',ylab='Probability of Winning',yaxt='n',
     xlim = c(0,28))
abline(h=0,col='gray');abline(h=1,col='gray')
axis(2,at=0:1,c('Lose','Win'))
# add logit curve
curve(1/(1+exp(-(mp$coef[1]+mp$coef[2]*x))),col='peru',lwd=2,add=T)
curve(1/(1+exp(-(ms$coef[1]+ms$coef[2]*x))),col='salmon',lwd=2,add=T)
# add legend
legend(x=1,y=1,
       legend=c("Paul","Shaun"),
       col = c("peru","salmon"),
       lty=1,lwd=2)
```

![](logistic-regression_files_exercise/figure-gfm/estimated%20plot-1.png)<!-- -->

Let’s check the performance of our model with AERs.

``` r
aer <- function(model){
  stopifnot("glm"%in%class(model))
  cbind(model$model,estimates = round(model$fitted.values)) -> x
  table(x[,1],x$estimate,dnn = list("Actual","Predicted")) -> tab
  return(list("AER table" = tab,
              "Correct ratio" = sum(diag(tab))/sum(tab)))
}
cat("Chaser: Paul\n")
```

    ## Chaser: Paul

``` r
aer(mp)
```

    ## $`AER table`
    ##       Predicted
    ## Actual   0   1
    ##      0 149   8
    ##      1  33  20
    ## 
    ## $`Correct ratio`
    ## [1] 0.8047619

``` r
cat("Chaser: Shaun\n")
```

    ## Chaser: Shaun

``` r
aer(ms)
```

    ## $`AER table`
    ##       Predicted
    ## Actual   0   1
    ##      0 123  28
    ##      1  43  36
    ## 
    ## $`Correct ratio`
    ## [1] 0.6913043

Who is the better chaser? Based on the probability of winning, Paul is
the better chaser. However let’s check the model with chaser as a
categorical variable:

``` r
m <- update(mp,.~.+Chaser,subset = Chaser %in% c("Paul","Shaun"))
summary(m)
```

    ## 
    ## Call:
    ## glm(formula = Win ~ TeamScore + Chaser, family = binomial(link = "logit"), 
    ##     data = df, subset = Chaser %in% c("Paul", "Shaun"))
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -7.35374    0.78645  -9.351  < 2e-16 ***
    ## TeamScore    0.35195    0.04194   8.393  < 2e-16 ***
    ## ChaserShaun  0.79257    0.24249   3.269  0.00108 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 537.56  on 439  degrees of freedom
    ## Residual deviance: 431.90  on 437  degrees of freedom
    ## AIC: 437.9
    ## 
    ## Number of Fisher Scoring iterations: 5

Paul is the better chaser since chaser Shaun increase the log odds of
winning by 0.7925748.
