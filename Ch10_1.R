#### Table 10.2

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(2)
train.index <- sample(1:nrow(bank.df), nrow(bank.df)*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)


#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])



# Choose cutoff value and evaluate classification performance
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)
confusionMatrix(factor(pred), factor(valid.df[,8]))



#### Figure 10.4
delays.df <- read.csv("FlightDelays.csv")

# transform variables and create bins
delays.df$DAY_WEEK <- factor(delays.df$DAY_WEEK, levels = c(1:7), 
                             labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

# create reference categories
delays.df$ORIGIN <- relevel(delays.df$ORIGIN, ref = "IAD")
delays.df$DEST <- relevel(delays.df$DEST, ref = "LGA")
delays.df$CARRIER <- relevel(delays.df$CARRIER, ref = "US")
delays.df$DAY_WEEK <- relevel(delays.df$DAY_WEEK, ref = "Wed")
delays.df$isDelay <- 1 * (delays.df$Flight.Status == "delayed")

# create training and validation sets
selected.var <- c(10, 1, 8, 4, 2, 9, 14)
train.index <- sample(1:nrow(delays.df), nrow(delays.df)*0.6)  
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

# run logistic model, and show coefficients 
lm.fit <- glm(isDelay ~ ., data = train.df, family = "binomial")
summary(lm.fit)


# set cutoff to be 0.5 and evaluate the model
pred <- predict(lm.fit, valid.df, type = "response" )
confusionMatrix(factor(ifelse(pred > 0.5, 1, 0)), factor(valid.df$isDelay))

# find out training accuracy
lm.fit$fitted.values
confusionMatrix(factor(ifelse(pred > 0.5, 1, 0)), factor(train.df$isDelay))


# change cutoff value, compare accuracy and recall for rare cases (delayed in this case), on validation set?
# compute recall from confusion matrix, no need to actually define "Recall", just explain it as the percentage of
# delayed flights that are identified




