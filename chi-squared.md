Chi-Squared test
================
Amnist.O
2024-09-09

# Assessment:

Use the data provided in the file LegoTable.csv to test whether the
strength of emotion is associated with the emotion. (I.e., for example,
do people, on average, perceive fear as more intense than sadness?)

List of emotions in the datasets

``` r
table(df$emotion)
```

    ## 
    ##     Anger   Disgust      Fear Happiness   Sadness  Surprise 
    ##      1212       632       322      2742       293       437

``` r
table(df$strength,df$emotion)
```

    ##    
    ##     Anger Disgust Fear Happiness Sadness Surprise
    ##   1   181      88   33       463      56       56
    ##   2   271     191   66       771      77      123
    ##   3   264     173   67       664      67      114
    ##   4   251     124   67       504      63       82
    ##   5   245      56   89       340      30       62

Perform the chi-squared test to test whether strength of emotioni
sassociated with the type of emotion

``` r
chisq.test(df$strength,df$emotion)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df$strength and df$emotion
    ## X-squared = 135.95, df = 20, p-value < 2.2e-16
