library(dplyr)

wine.red<- read_delim("winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)
wine.white<- read_delim("winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
wine.red$color = "Red"
wine.white$color = "White"
wine <- rbind(wine.red, wine.white)
write.csv(wine, file = 'wine.csv',row.names=F)

wine.df <- read.csv("wine.csv")
selected.var <- c(4,6,8,13)
train.index <- sample(1:nrow(wine.df), nrow(wine.df) * 0.8)

train.df <- wine.df[train.index, selected.var]
valid.df <- wine.df[-train.index,selected.var]

library(caret)
norm.values <- preProcess(wine.df[, 1:3], method=c("center", "scale"))

train.norm.df <- train.df
valid.norm.df <- valid.df
wine.norm.df <- wine.df

train.norm.df[, 1:3] <- predict(norm.values, train.df[, 1:3])
valid.norm.df[, 1:3] <- predict(norm.values, valid.df[, 1:3])
wine.norm.df[, 1:3] <- predict(norm.values, wine.df[, 1:3])

library(FNN)
knn.pred <- knn(train.norm.df[, 1:3], valid.norm.df[, 1:3], 
                cl = train.norm.df[, 4], k = 5)
confusionMatrix(knn.pred, valid.norm.df[, 4])$overall[1]

new.df <- data.frame(residual.sugar = 1.60, free.sulfur.dioxide = 20, density = 0.99230)

new.df$residual.sugar
train.df$density
new.norm.df <- new.df
new.norm.df <- predict(norm.values, new.df)
