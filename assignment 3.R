# assignment 3 solution

cancer.df <- read.csv("breastcancer.csv")

# normalize data
library(caret)
# compute mean and standard deviation of each column
norm.values <- preProcess(cancer.df[,-10], method=c("center", "scale"))
# we perform the transformation/normalization
cancer.df.norm <- predict(norm.values, cancer.df[,-10])

# set seed for reproducibility
set.seed(111)
km <- kmeans(cancer.df.norm, 3)
#show the means
km$centers

# get the clusteres
km$cluster
#compare the clusters with true Diagnosis
data.frame(Diagnosis = cancer.df$Diagnosis,Cluster = km$cluster)

# recode Diagnosis 
cancer.df$isMalignant <- as.numeric(cancer.df$Diagnosis == "malignant")
exlcuded_vars <- 10
# partition data
train.index <- sample(1:nrow(cancer.df), nrow(cancer.df)*0.7)
train.df <- cancer.df[train.index, -exlcuded_vars]
valid.df <- cancer.df[-train.index, -exlcuded_vars]

# logistic regression
logit.reg <- glm(isMalignant ~ ., data = train.df, family = "binomial")
summary(logit.reg)

# prediction on validation set
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)

# calculate the accuracy on validation set
confusionMatrix(factor(pred), factor(valid.df$isMalignant))
