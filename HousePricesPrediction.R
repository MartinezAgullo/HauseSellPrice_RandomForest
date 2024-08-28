# Load in packages
library(tidyverse) # Collection of R packages designed for data science
library(rpart)     # Library for building decision trees

# Read the data and store data in a tibble
iowa_data <- read_csv("input/train.csv") 

# Make sure all necessary columns are treated as factors
factor_cols <- c("Condition1", "ExterCond", "HeatingQC", "Electrical", "PoolQC", "MiscFeature") # Add all relevant factor columns
iowa_data[factor_cols] <- lapply(iowa_data[factor_cols], as.factor)

# Structure of the dataframe
str(iowa_data)

# Summary statistics
summary(iowa_data)

# Print a list of the column names
names(iowa_data)

# Glimpse the first few rows
head(iowa_data)

# Split your data into training and test sets
set.seed(42) # Set a seed to reproduce results
sample_index <- sample(1:nrow(iowa_data), 0.7 * nrow(iowa_data))
train_set <- iowa_data[sample_index, ]
test_set <- iowa_data[-sample_index, ]

# Ensure factor levels in test set match those in the training set for all factors
for(col in factor_cols) {
  test_set[[col]] <- factor(test_set[[col]], levels = levels(train_set[[col]]))
}

# Fit a model to your training set
model <- rpart(SalePrice ~ ., data = train_set, method = "anova") 

# Predict on the test set
predictions <- predict(model, test_set)

# Evaluate the model: Calculate Mean Squared Error
mse <- mean((predictions - test_set$SalePrice)^2)
print(paste("Mean Squared Error:", mse))