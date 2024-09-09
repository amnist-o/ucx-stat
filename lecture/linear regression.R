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

