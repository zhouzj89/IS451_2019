housing.df <- read.csv("Housing.csv")
set.seed(5)
train.index <- sample(1:nrow(housing.df), 0.6*nrow(housing.df))
train.df <- housing.df[train.index, ]
valid.df <- housing.df[-train.index,]

# initialize normalized training, validation data, complete data frames to originals

train.norm.df <- train.df
valid.norm.df <- valid.df
housing.norm.df <- housing.df

#load package
library(caret)
norm.values <- preProcess(train.df[, 1:12], method=c("center", "scale"))

#normalize data
train.norm.df[, 1:12] <- predict(norm.values, train.df[, 1:12])
valid.norm.df[, 1:12] <- predict(norm.values, valid.df[, 1:12])
# initialize a data frame with two columns: k, and accuracy to store the accuracy for each k.
accuracy.df <- data.frame(k = 1:5, accuracy = rep(0, 5))

# compute knn for different k on validation.
library(FNN)
for(i in 1:5) {
  # use ?knn to find out more information
  # It worth noting that the input argument cl must be a factor!
  knn.pred <- knn(train.norm.df[, 1:12], valid.norm.df[, 1:12], 
                  cl = train.norm.df[, 14], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 14])$overall[1] 
}

#print out the accuracy matrix
accuracy.df
# Predict for the new household using the best k (=2)
new.df <- data.frame(CRIM = 0.2, ZN = 0, INDUS = 7, CHAS = 0, NOX = 0.538, RM = 6, AGE = 62, DIS = 4.7, RAD = 4, TAX = 307, PTRATIO = 21, LSTAT = 10)
new.norm.df <- predict(norm.values, new.df)
knn.pred.new <- knn(train.norm.df[, 1:12], new.norm.df, 
                    cl = train.norm.df[, 14], k = 2)
# See the result
knn.pred.new[1]
knn.pred.train <- knn(train.norm.df[, 1:12], train.norm.df[, 1:12], 
                      cl = train.norm.df[, 14], k = 2)
#Confusion Matrix
confusionMatrix(knn.pred.train, train.norm.df[, 14])$table

#Accuracy
confusionMatrix(knn.pred.train, train.norm.df[, 14])$overall[1] 

