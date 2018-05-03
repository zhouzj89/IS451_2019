library(rpart)
library(rpart.plot)

bank.df <- read.csv("wine.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# Partition
set.seed(1)  
train.index <- sample(1:nrow(bank.df), nrow(bank.df)*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]


# Default classification tree, optimized by pruning
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
## plot tree
prp(default.ct, type = 1, extra = 1, varlen = -10)



# Full Tree 
## cp: complexity parameter
## minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.
full.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = -1, minsplit = 1)
## Don't attempt to plot the full tree. R may give you an error since it's too huge.

## number of leafs in the full tree
length(full.ct$frame$var[full.ct$frame$var == "<leaf>"])




# Classification accuracy measure of the default tree
library(caret)
# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
# Otherwise, a probablity of belonging to each class
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred, factor(valid.df$Personal.Loan))






