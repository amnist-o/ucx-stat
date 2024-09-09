# Permutation
# Permutated the group infinite times then if the mean is different t test will show positive
df <- read.csv("datasets/EucalyptusTtest.csv")
summary(df)

boxplot(hgt ~ spp, data= df,
        col = "grey",
        xlab = "",
        ylab = "Height (m)")

t.test(hgt ~ spp, data = df)
# test shows that there is no difference in mean heights

otest <- t.test(hgt ~ spp, data = df)
otest$statistic

# Start permutation
pnum <- 1000

myt <- numeric(pnum)

for(p in 1:pnum){
  myt[p] <- t.test(df$hgt ~ sample(df$spp))$statistic
}

hist(myt, col = "turquoise",
     main = "",
     xlab = "t-statistic")
abline(v = otest$statistic, col = "goldenrod",lwd = 3)

myp <- (mean(abs(myt)>abs(otest$statistic)))
myp
otest$p.value

# If H0 is one-tailed hypothesis
p.val2 <- (mean(myt > otest$statistic))
p.val2


# ANOVA permutation
x <- read.csv("datasets/EucalyptiANOVA1.csv")
summary(x)
boxplot(hgt~spp,data = x,
        col = "grey",
        xlab = "",
        ylab = "Height (m)")
a1 <- anova(lm(hgt ~ spp, data = x))
summary(a1)

# Permutate the species
myf <- numeric(pnum)

for(p in 1:pnum){
  myf[p] <- anova(lm(x$hgt ~ sample(x$spp)))[1,4]
}

a1[1,4]
hist(myf,
     col = "turquoise",
     main = "",
     xlab = "F-statistic",
     seq(0,10,.5))
abline(v=a1$`F value`[1],col = "goldenrod",lwd=3)

# get p value
p.val <- mean(myf > a1[1,4])
p.val