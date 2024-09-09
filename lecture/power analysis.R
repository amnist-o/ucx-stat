# Simulation for power analysis
# power analysis before the experiment:
## determine the minimum sample size n necessary to obtain staitstical power of at least 1-beta at a predetermined statistical significance level alpha
# power analysis after the experiment:
## determining the power of the experiment already performed,
## then determining the minimum sample size n necessary to obtain statistical power of at least 1-beta at a predetermined statistical significance lvel alpha

# for statistical tests, power is the probability of detecting the effect when it is there -> probability of true positive
# comparing average weights of two varieties of grapes
# A has mean weight of 250 g variance of 100
# B has mean weight of 245 g variacne of 144
# assume the weight population of 2 grapes is normally distributed

# How likely are we to detect this population difference if we collect n = 10 bunces from eahc variety and perform a t-test at 5% significance level

# step 1: simulate 2 grapes weights
set.seed(20200915)
n <- 10
x1 <- rnorm(10,250,10)
x2 <- rnorm(10,245,12)

t.test(x1,x2)

# step 2: evaluating the statistical power of the test
set.seed(20200915)
is.positive <- numeric(1000)
n <- 10
for(i in 1:1000){
  x1 <- rnorm(n,250,10)
  x2 <- rnorm(n,245,12)
  
  is.positive[i] <- (t.test(x1,x2)$p.value < 0.05)
}
mean(is.positive)

# this simulation shows that for 15% of the time t-test shows that the mean of the grape weights are different
# because the sample size is small (n = 10)

# Now try with larger sample size
set.seed(20200915)
is.positive <- numeric(1000)
n <- 20
for (i in 1:1000){
  x1 <- rnorm(n,250,10)
  x2 <- rnorm(n,245,12)
  
  is.positive[i] <- (t.test(x1,x2)$p.value < 0.05)
}
mean(is.positive)

# Now the mean shows that the test is accurate 25% of the time
# This means that the power of statistical test (in this case t-test) increases with the sample size

# Now write the function of power testing
mypower <- function(n, alpha = 0.05){
  is.positive <- numeric(1000)
  for(i in 1:1000){
    x1 <- rnorm(n,250,10)
    x2 <- rnorm(n,245,12)
    
    is.positive[i] <- (t.test(x1,x2)$p.value < alpha)
  }
  return(mean(is.positive))
}

mypower(100)

# what we did up to now is 1 statistical power test
# Now we will make a full power analysis of a test
n.vals <- seq(5,200,5) # create a sequence of number starting from 5 to 200 with increment of 5.

# preparing the vector keeping evaluated power
power99 <- power95 <- numeric(length(n.vals))

set.seed(20202020)

# loop the test
for(i in 1:length(n.vals)){
  power99[i] <- mypower(n.vals[i],0.01)
  power95[i] <- mypower(n.vals[i],0.05)
}

dfp <- data.frame(
  n = rep(n.vals,2),
  alpha = factor(rep(c(.01,.05),each = length(n.vals))),
  power = c(power99,power95)
)

dfp %>%
  ggplot(aes(x = n, y = power, group = alpha))+
  geom_point(aes(col = alpha))+
  geom_line(aes(col = alpha))+
  xlab("Sample size (n)")+
  geom_hline(yintercept = c(.8,.9,.95),col = c("red","goldenrod","springgreen3"))

# smaller alpha -> larger sample size required
# higher power -> larger sample size required
