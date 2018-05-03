# read data
wine.df <- read.csv("wine.csv")
hist(wine.df$quality)
##alternate graph using ggplot
library(ggplot2)
ggplot(data = wine.df) + 
  geom_histogram(aes(x = quality),binwidth = 1)

set.seed(555)  # set seed for reproducing the partition
# use nrow to get the total number of observations
nobs <- nrow(wine.df)
train.index <- sample(1:nobs, 0.7 * nobs)  
train.df <- wine.df[train.index, ]
valid.df <- wine.df[-train.index, ]

wine.lm <- lm(quality ~ ., data = train.df)
options(scipen = 999)
summary(wine.lm)
library(forecast)
accuracy(wine.lm$fitted.values, train.df$quality)
wine.lm.pred <- predict(wine.lm, valid.df)
accuracy(wine.lm.pred, valid.df$quality)