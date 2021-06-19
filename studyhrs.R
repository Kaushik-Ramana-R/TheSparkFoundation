# clear all
rm(list = ls())
cat("\f")

# importing the data set and problem question
df <- read.csv("./studyhrs.csv", header = TRUE)
GivenQuestion <- data.frame(Hours=9.25)

# checking for Outlier and missing values
summary(df)
# There are no outliers or missing values in the set.

# visualizing the dataset
plot(df$Hours,df$Scores)
# We can notice that there is a linear relationship between scores and price.

# test train split
set.seed(0)
split <- rbinom(nrow(df), 1, 0.8)
df_train = df[split == 1, ]
df_test = df[split == 0, ]

# Visualizing test and train set
plot(df_train$Hours,df_train$Scores)
plot(df_test$Hours,df_test$Scores)

# forming a linear simple regression model
df_model = lm(Scores~Hours, data=df_train)

# checking for statistic significance
summary(df_model)
# the r-square value indicates a good model fit

# visualizing the model for train data
plot(df_train$Hours,df_train$Scores)
abline(df_model)

# predicting results for test data
df_test_predicted = predict(df_model, df_test)

df_compare <- df_test
df_compare$scores_predict = df_test_predicted
df_compare

# visualizing the model fit for test data
plot(df_test$Hours,df_test$Scores)
abline(df_model)

# mean absolute error
mae = sum((df_test_predicted - df_test[,-1])/nrow(df_test))
mae

# Answering problem question
FinalSolution = predict(df_model, GivenQuestion)
FinalSolution

              
