# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(caret)
library(caTools)
library(ROCR)

# Read the dataset
data <- read.csv("customer_purchase_data.csv", stringsAsFactors = TRUE)
#data <- read.csv("customer_purchase_data.csv")

# Structure of the data
str(data)

# 1. Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)

# 2. Check for duplicates
duplicates <- data[duplicated(data), ]
num_duplicates <- nrow(duplicates)
print(paste("Number of duplicate rows:", num_duplicates))

#print("Duplicate rows:")
#print(duplicates)

#numerical columns
numerical_cols <- c("Age", "AnnualIncome", "NumberOfPurchases", "TimeSpentOnWebsite", "DiscountsAvailed")

# Categorical columns
categorical_cols <- c("Gender", "ProductCategory", "LoyaltyProgram")
for (col in categorical_cols) {
  if (any(is.na(data[[col]]))) {
    mode <- as.character(stats::mode(data[[col]]))
    data[[col]][is.na(data[[col]])] <- mode
  }
}

# Step 2: Encoding Categorical Variables
#Convert categorical variables to factors
#data$Gender <- factor(data$Gender, levels = c(0, 1), labels = c("Male", "Female"))
#data$ProductCategory <- factor(data$ProductCategory, levels = 0:4, labels = c("Electronics", "Clothing", "Home Goods", "Beauty", "Sports"))
#data$LoyaltyProgram <- factor(data$LoyaltyProgram, levels = c(0, 1), labels = c("No", "Yes"))
#data$PurchaseStatus <- factor(data$PurchaseStatus, levels = c(0, 1), labels = c("No", "Yes"))

# Step 3: Feature Scaling
# Scale numerical features
data[numerical_cols] <- scale(data[numerical_cols])

# Step 4: Splitting the Dataset
set.seed(123) # for reproducibility
split = sample.split(data$PurchaseStatus, SplitRatio = .8)
data_train <- filter(data, split == TRUE) 
data_test <- filter(data, split == FALSE)

# Check the dimensions of the train and test sets
dim(data_train)
dim(data_test)



