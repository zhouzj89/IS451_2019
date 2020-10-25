# q3
library(rpart)
library(rpart.plot)

donation.df <- read.csv("donation_binary.csv")

set.seed(1234)  
train.index <- sample(1:nrow(donation.df), nrow(donation.df)*0.6)  
train.df <- donation.df[train.index, ]
valid.df <- donation.df[-train.index, ]

# Default classification tree, optimized by pruning
default.ct <- rpart(Donation ~ ., data = train.df, method = "class")
## plot tree
prp(default.ct, type = 1, extra = 1, varlen = -10)

## q4

phar.df <- read.csv("Pharmaceuticals.csv")
phar.df.numeric <- phar.df[,2:10]
set.seed(123)
km <- kmeans(phar.df.numeric, 3)
km$centers
library(dplyr)
data.frame(cluster = km$cluster, recommendation = phar.df$Median_Recommendation, location = phar.df$Location, 
           exchange = phar.df$Exchange) %>% arrange(cluster)

# q5
phone.df <- read.csv("phone_sale.csv")
phone.df$Phone_sale <- as.numeric(phone.df$Phone_sale  == "Yes")
selected.vars <- c("Phone_sale","Bonus_trans", "Any_cc_miles_12mo")
set.seed(111)
train.index <- sample(1:nrow(phone.df), nrow(phone.df)*0.6)  
train.df <- phone.df[train.index, selected.vars]
valid.df <- phone.df[-train.index, selected.vars]
logit.reg <- glm(Phone_sale ~ ., data = train.df, family = "binomial")
summary(logit.reg)

new.df <- train.df[1,]
new.df$Bonus_trans <-50
new.df$Any_cc_miles_12mo <- 1
logit.reg.pred <- predict(logit.reg, new.df, type = "response")
logit.reg.pred

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
pred <- ifelse(logit.reg.pred > 0.2, 1, 0)
library(caret)
confusionMatrix(factor(pred), factor(valid.df$Phone_sale))
