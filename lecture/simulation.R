# CLT says that if an i.i.d. sample x1,...,xn of size n comes from a distribution with mean mu and variance sigma then
# mean(x) is normally distributed
# generate x of poisson distribution with mean = 5 and variance = 5
x <- rpois(200,5)
mean(x)

# repeat for 1000 times
x.means <- numeric(1000)

# generate 1000 samples and record their means
for(i in 1:1000){
  x <- rpois(200,5)
  x.means[i] <- mean(x)
}

# mean should be close to 5. variance close to 0.025
mean(x.means)
var(x.means)

ggplot(data.frame(x = x.means),
       aes(x))+
  geom_density(fill = "turquoise3")+
  stat_function(fun = dnorm,
                args = list(mean=5,sd = sqrt(5/200)),
                xlim = c(3.5,6.5),
                size = 1.5,
                col = "black")

# commands for various distributions
runif(5) # uniform
rnorm(5) # normal
rpois(5,5) # poisson
rbinom(5,1,.5) # binomial obs, amount of trials (result returns amount of success), probability of success

# setting seed to ensure the starting value for randoming is the same
set.seed(1)

# timing
system.time({
  x.means <- numeric(1000)
  for(i in 1:1000){
    x <- rbinom(2000,10,.5)
    x.means[i] <- mean(x)
  }
})

# CI simulation
# If we repeat the experiment (i.e. draw the sample of size n from the distribution) many times
# after each experiment, construct the 95% CI in the way described above
# then about 95% of those CIs will contain the true value of the parameter.

mysample <- rbinom(100,1,.79)

# This assumes that the population is infinite
# simulate the population first
set.seed(1)
n.car <- .79*5e6
n.o <- (1-.79)*5e6
mypopulation <- rep(c(0,1),c(n.o,n.car))
mysample <- sample(mypopulation,size = 100, replace = F)

xbar <- mean(mysample)
xsd <- sd(mysample)
lo <- xbar - (1.96*xsd/sqrt(100))
hi <- xbar + (1.96*xsd/sqrt(100))
(lo < .79)&(hi > .79)

# try with a lot of samples
set.seed(20200209)
nsample <- 10^3
mysamples <- array(rbinom(nsample*100,1,.79),dim=c(nsample,100))
xbars <- apply(mysamples, 1, mean)
xsds <- apply(mysamples, 1, sd)

los <- xbars - (1.96*xsds/sqrt(100))
his <- xbars + (1.96*xsds/sqrt(100))

is.in <- (los < .79)&(his > .79)
mean(is.in)

# normal distribution
set.seed(20200902)
nval <- seq(5,120,5)
nsample <- 10^4
coverage <- numeric(length(nval))

for (j in 1:length(nval)){
  n <- nval[j]
mysamples <- array(rnorm(nsample*n,175,10),dim=c(nsample,n))
xbars <- apply(mysamples, 1, mean)
xsds <- apply(mysamples, 1, sd)

los <- xbars - (1.96*xsds/sqrt(n))
his <- xbars + (1.96*xsds/sqrt(n))

is.in <- (los < 175)&(his > 175)
coverage[j] <- mean(is.in)
}

plot(nval,coverage,
     xlab = "sample size n",
     pch = 16,
     ty = "b",
     ylim = c(.5,1))
abline(h=.95,col = "red")

# CLT stated that sample will have normal distribution with the same mean as the population and the variance equals to population variance/n
# only if we sample large enough
# but we don't know what is "large" quantitatively
# recommendation is 30 but that's still not clear

n <- 10
x <- runif(10,0,1)
mean(x)

# variance of uniform [0,1] = 1/12
# var = E(x^2) - E(x)^2
# E(x) = 1/(b-a) \int_([a,b]) x dx

nsim <- 10000
x <- array(runif(nsim*n),dim = c(nsim,n))
mean.x <- apply(x, 1, mean)

# plot distribution
hist(mean.x,
     main = "",
     xlab = expression(mean(x)),col="grey",
     freq=F,
     xlim = c(0,1),
     ylim = c(0,5))
lines(density(mean.x),col = "goldenrod",lwd = 2)
curve(dnorm(x,1/2,sd=sqrt(1/12/10)),add = T,
      col = "navyblue",
      lwd = 2,lty=2)
legend(.02,4,col = c("goldenrod","navyblue"),
       lty = c(1,2),c("observed","CLT"))

# qqplot
qqnorm(mean.x)
qqline(mean.x,col = "red",lwd = 2)

# try small sample size
clt <- function(n, nsim) {
  x <- array(runif(nsim * n), dim = c(nsim, n))
  mean.x <- apply(x, 1, mean)
  
  # plot distribution
  hist(
    mean.x,
    main = "",
    xlab = expression(mean(x)),
    col = "grey",
    freq = F,
    xlim = c(0, 1),
    ylim = c(0, 5)
  )
  lines(density(mean.x), col = "goldenrod", lwd = 2)
  curve(
    dnorm(x, 1 / 2, sd = sqrt(1 / 12 / n)),
    add = T,
    col = "navyblue",
    lwd = 2,
    lty = 2
  )
  legend(
    .02,
    4,
    col = c("goldenrod", "navyblue"),
    lty = c(1, 2),
    c("observed", "CLT")
  )
  
  # qqplot
  qqnorm(mean.x)
  qqline(mean.x, col = "red", lwd = 2)
}
