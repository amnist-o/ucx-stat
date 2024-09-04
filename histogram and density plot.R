library(tidyverse)
nest <- read_lines(file = "datasets/nest")
df <- read_lines(file = nest)

enframe(df[-1],name=NULL,value=df[1]) %>%
  mutate(x = as.numeric(x)) -> df

df %>%
  ggplot(aes(x=x))+
  geom_histogram(bins = 50)

df %>%
  ggplot(aes(x=x))+
  geom_density(fill="darkgreen",adjust=.3)
