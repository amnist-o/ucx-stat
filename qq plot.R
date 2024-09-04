set.seed(1)
x.norm <- rnorm(100)

x.poisson <- rpois(100,20)

x.binom <- rbinom(100,10,.7)

x.gamma <- rgamma(100,100,1)

hist(x.gamma)
plot(hist(x.norm),col="dodgerblue")
plot(hist(x.poisson),col="orange2",add=T)
plot(hist(x.binom),col="springgreen3",add=T)
plot(hist(x.gamma),col="turquoise3",add=T)
plot(hist(c(x.norm,100)),col="royalblue4")

qqnorm(x.norm)
qqline(x.norm,col="red")

qqnorm(x.poisson)
qqline(x.poisson,col="red")

qqnorm(x.binom)
qqline(x.binom,col="red")

qqnorm(x.gamma)
qqline(x.gamma,col="red")

qqnorm(c(x.norm,100))
qqline(c(x.norm,100),col="red")
