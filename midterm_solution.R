#midterm solution

cancer.df <- read.csv("cancer.csv")
set.seed(10)
train.index <- sample(1:nrow(cancer.df), 0.7*nrow(cancer.df))
train.df <- cancer.df[train.index, ]
valid.df <- cancer.df[-train.index,]

# initialize normalized training, validation data, complete data frames to originals

train.norm.df <- train.df
valid.norm.df <- valid.df


#load package
library(caret)
norm.values <- preProcess(train.df[, 1:4], method=c("center", "scale"))

#normalize data
train.norm.df[, 1:4] <- predict(norm.values, train.df[, 1:4])
valid.norm.df[, 1:4] <- predict(norm.values, valid.df[, 1:4])

library(FNN)

#get prediction on training set
knn.pred.train <- knn(train.norm.df[, 1:4], train.norm.df[, 1:4], 
                cl = train.norm.df[, 5], k = 1)

confusionMatrix(knn.pred.train, train.norm.df[,5])$table

#accuracy
confusionMatrix(knn.pred.train, train.norm.df[,5])$overall[1]

#predict the new record
new.df <- data.frame(Clump.Thickness = 5, Uniformity.of.Cell.Size = 1, Bare.Nuclei = 10, Bland.Chromatin = 5)

#normalize new variable
new.norm.df <- predict(norm.values, new.df)

#predict new using k =1
knn.pred.new <- knn(train.norm.df[, 1:4], new.norm.df, 
                    cl = train.norm.df[,5], k = 1)

#result: malignant


