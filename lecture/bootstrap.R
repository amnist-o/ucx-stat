# Non parametric method
# CLT cannot be used if interested variable is not about mean and variance
# We may not know if a sample size is large enough when we do not know the distribution of the population
# We can use bootstrap instead
# bootstrap assumes that the variant of sample responses we have is the population
# meaning that we can replicate the sample infinitely to obtain the population
# then we sample the generated population many many times to obtain the information

# more than 1 car survey: 100 samples, 98 more than 1 car, 2 no more than 1 car.
x <- rep(c(0,1),c(2,98))

x.bar <- mean(x)
x.sd <- sd(x)
x.n <- length(x)

x.lo <- x.bar - (1.96*x.sd/sqrt(x.n))
x.hi <- x.bar + (1.96*x.sd/sqrt(x.n))

# bootstrap
set.seed(20202020)
mymean <- numeric(1000)
for (i in 1:1000) {
  s1 <- sample(x, size = x.n, replace = T)
  mymean[i] <- mean(s1)
}

mean(mymean)
quantile(mymean,c(0.025,.975))
hist(mymean)

# another example

x <- c(1.3,7.2,5.7,4.8,2.9) # original sample
b <- 1000 # number of bootstrap samples


x.boot <- array(sample(x, replace = T, size = b * length(x)), 
                dim = c(b, length(x)))

x.means <- apply(x.boot,1,mean)

mean(x.means)
sd(x.means)
quantile(x.means,c(.025,.975))