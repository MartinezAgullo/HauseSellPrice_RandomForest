# load in the tidyverse package
library(tidyverse) #  Collection of R packages designed for data science

# read the data and store data in a tibble
iowa_data <- read_csv("input/train.csv") 

# make sure Condition1 is a factor & not a char
iowa_data$Condition1 <- as.factor(iowa_data$Condition1)


# Structure of the dataframe
str(iowa_data)

# Summary statistics
summary(iowa_data)

# Glimpse the first few rows
head(iowa_data)