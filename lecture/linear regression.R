# Regression
df <- read.csv("datasets/howell1.csv",sep =";")
head(df)
summary(df)
plot(df)

# subset data to age >= 18
df1 <- df[df$age>=18,]

plot(df1$height,df1$weight)

m1 <- lm(weight ~ height , data = df1)

summary(m1)

# specify model instead of height we use difference from average height
m2 <- lm(weight ~ I(height - mean(height)),data = df1)

summary(m2)

# plot data of the model
plot(df1$height, df1$weight,
     xlab = "Height (cm)",
     ylab = "Weight (kg)",
     pch = 16)

# use object to refer to coef
curve(m1$coefficients[1]+m1$coefficients[2]*x,
      add = T,
      col = "turquoise3",
      lwd = 2)

# linearisation can be used to change non linear real life behavior to linear
# assumption for linear regression are
# 1 parameters are linear and
# 2 expected residuals = 0
# 3 residuals are i.i.d. ~ N(0,sigma^2)
# check assumptions for m1

# linear in parameter: red line has no pattern
plot(m1,1)

# normal distributed residuals
plot(m1,2)

# constant variance (homoscedasticity)
# does the variance change response to Y?
plot(m1,3)

# Outlier check
plot(m1,4)

# Interpretation of coefficients
summary(m1)

# intercept coefficient has no meaningful interpretation in this case
# the height coefficient however can be interpreted as
# 1 cm additional height is associated with average 630g increase in weight
# We can get confident interval with confint
confint(m1)

# p value is reported to be 2.2e-16 which to make it easy, can be written as 0.0001

# log-linear model
# looking at m1, we can see that for some height, model predicted negative weight which is impossible
# we can say that it doesn't matter because the model works in a definitive domain
# or we can use log transform in order to avoid awkward negative prediction
m3 <- update(m1,log(weight)~.)
summary(m3)

# we can see that m3 plot and m1 plot do not drastically changed
plot(m3)

# let's look at the fit
plot(df1$height,df1$weight,
     xlab = "Height (cm)",
     ylab = "Weight (kg)",,
     pch = 16)
curve(m1$coefficients[1]+m1$coefficients[2]*x,
      add = T,
      col = "navyblue",
      lwd = 2)
curve(exp(m3$coefficients[1]+m3$coefficients[2]*x),
      add = T,
      col = "goldenrod",
      lwd = 2, lty = 2)

# they are close to each other yet not the same
# Interpreting log-linear model
summary(m3)

# this is a percentage change rather than flat addition
# so it means:
# each cm additional height associated with average of e^0.0140923 = 1.42% increase in weight
# confident interval can be obtained with:
exp(confint(m3)[2,])*100-100

# what if the coefficient is large
exp(3)

# which we do not interpret in percentage but in folds or times
# so 1908% increase is equivalent to increase by 19 folds
# if the coefficient is small <.1
exp(.1)

# we can see that e^x is approximately x