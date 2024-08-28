# load in packages
library(tidyverse) # Collection of R packages designed for data science
library(rpart)     # Library for building decision trees


# read the data and store data in a tibble
iowa_data <- read_csv("input/train.csv") 

# make sure Condition1 is a factor & not a char
iowa_data$Condition1 <- as.factor(iowa_data$Condition1)


# Structure of the dataframe
str(iowa_data)

# Summary statistics
summary(iowa_data)

# print a list of the column names
names(iowa_data)

# Glimpse the first few rows
head(iowa_data)

# Split your data into training and test sets
set.seed(42) # Set a seed to reproduce results
sample_index <- sample(1:nrow(iowa_data), 0.7 * nrow(iowa_data))
train_set <- iowa_data[sample_index, ]
test_set <- iowa_data[-sample_index, ]

# Fit a model to your training set
model <- rpart(SalePrice ~ ., data = train_set, method = "anova") 
# Analysis of variance (ANOVA)

# Predict on the test set
predictions <- predict(model, test_set)

# Evaluate the model: Calculate Mean Squared Error
mse <- mean((predictions - test_set$SalePrice)^2)   # Predictions are made on the test set.
print(paste("Mean Squared Error:", mse))