# read data file
## The data file of the height of eucalyptus and spp (factor)
df <- read.csv("datasets/Eucalyptus1_.csv")

# data structure
str(df)

# some first data
head(df)

# summary stat of data
summary(df)

# some function of data
## mean of the height
mean(df$hgt)
## standard deviation of the height
sd(df$hgt)
## lmedian of the height
median(df$hgt)

# sampling
sample(1:5) # draw from 1:5

sample(1:5, size = 3) # draw 3 times from 1:5

sample(1:5, size = 10, replace = F) # never draw same number twice

# try sampling
sample(df$hgt,size = 10, replace = F) -> sample1
## find the mean of sampling
mean(sample1)

# bootstrap the mean
my.mean <- numeric(1000)
for(i in 1:1000){
  sample(df$hgt,size = 10, replace =F) -> sample1
  my.mean[i]<- mean(sample1)
}

## plot the histogram
hist(my.mean)

## view the named color
colors()
## configure the histogram
hist(my.mean, main = "Histogram of mean of Eucalyptus height",xlab="Height, m.",col="plum")

## view mean
mean(my.mean)
## find quantile
quantile(my.mean,c(.025,.975))
