# Load in packages
library(tidyverse) # Collection of R packages designed for data science
library(rpart)     # Library for building decision trees

# Read the data and store data in a tibble
iowa_data <- read_csv("input/train.csv") 

# Make sure Condition1 is a factor & not a char
iowa_data$Condition1 <- as.factor(iowa_data$Condition1)

# Ensure ExterCond and HeatingQC include all possible levels
iowa_data$ExterCond <- factor(iowa_data$ExterCond, levels = c("Ex", "Gd", "TA", "Fa", "Po"))
iowa_data$HeatingQC <- factor(iowa_data$HeatingQC, levels = c("Ex", "Gd", "TA", "Fa", "Po"))

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

# Ensure factor levels in test set match those in the training set
test_set$ExterCond <- factor(test_set$ExterCond, levels = levels(train_set$ExterCond))
test_set$HeatingQC <- factor(test_set$HeatingQC, levels = levels(train_set$HeatingQC))
test_set$Electrical <- factor(test_set$Electrical, levels = levels(train_set$Electrical))

# Handle any new levels in the test set by setting them to NA or an existing level
test_set$Electrical <- factor(test_set$Electrical, levels = levels(train_set$Electrical))

# Fit a model to your training set
model <- rpart(SalePrice ~ ., data = train_set, method = "anova") 
# Analysis of variance ANOVA

# Predict on the test set
predictions <- predict(model, test_set)

# Evaluate the model: Calculate Mean Squared Error
mse <- mean((predictions - test_set$SalePrice)^2)
print(paste("Mean Squared Error:", mse))