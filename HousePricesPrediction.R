# Load in packages
library(tidyverse) # Collection of R packages designed for data science
library(rpart)     # Library for building decision trees
library(randomForest)
library(mice)

# Read the data and store data in a tibble
iowa_data <- read_csv("input/train.csv") 

# Define the target variable
target <- "SalePrice"


# Make sure all necessary columns are treated as factors
factor_cols <- c("Condition1", "Condition2" , "ExterCond", "HeatingQC", "Electrical", "PoolQC", "MiscFeature", "Utilities", "RoofMatl", "Exterior1st", "Exterior2nd", "RoofStyle", "Heating", "Functional") 
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

# Define predictors
predictors <- names(train_set)[names(train_set) != target]

# Impute missing values in the training set
imputed_data_train <- mice(train_set[, c(target, predictors)], m = 1, method = 'pmm', seed = 42)
train_set_complete <- complete(imputed_data_train)

# Impute missing values in the test set using the same method
imputed_data_test <- mice(test_set[, c(target, predictors)], m = 1, method = 'pmm', seed = 42)
test_set_complete <- complete(imputed_data_test)

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
#"Mean Squared Error: 1683466251.99935"

#####
# mae() ::  Function to calculate Mean Absolute Error
#####
mae <- function(model, test_data) {
  predictions <- predict(model, test_data)
  actuals <- test_data$SalePrice
  mean(abs(predictions - actuals))
}

#####
# get_mae() :: a function to get the maximum average error for a given max depth
####
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
    # Create the formula directly using the target and predictors
    formula <- as.formula(paste(target, "~ ."))
    
    # Build the model with the given maxdepth
    model <- rpart(formula, data = training_data[, c(target, predictors)], 
                   control = rpart.control(maxdepth = maxdepth))
    
    # Calculate MAE
    mae <- mae(model, testing_data[, c(target, predictors)])
    return(mae)
}



# Iterate over a range of maxdepth values to find the one with the lowest MAE
maxdepth_values <- 1:10
mae_values <- sapply(maxdepth_values, function(depth) {
  get_mae(maxdepth = depth, target = target, predictors = predictors, 
          training_data = train_set, testing_data = test_set)
})

# Find the best maxdepth
best_maxdepth <- maxdepth_values[which.min(mae_values)]
print(paste("Best maxdepth:", best_maxdepth))
print(paste("Lowest MAE:", min(mae_values)))
# [1] "Best maxdepth: 4"
# [1] "Lowest MAE: 29148.7997552873"



####
## USING RANDOM FOREST
######

# Train the random forest model on the imputed training data
rf_model <- randomForest(as.formula(paste(target, "~ .")), 
                         data = train_set_complete, 
                         ntree = 500, 
                         importance = TRUE)

# Predict on the test set
rf_predictions <- predict(rf_model, test_set_complete)

# Calculate Mean Absolute Error (MAE)
rf_mae <- mean(abs(rf_predictions - test_set_complete$SalePrice))
print(paste("Random Forest MAE:", rf_mae))

# Check out feature importance
importance(rf_model)
varImpPlot(rf_model)