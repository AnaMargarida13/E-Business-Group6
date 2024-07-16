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

# Remove duplicate rows
data <- data %>% distinct()

# Verify if duplicates are removed
duplicates_after_removal <- data[duplicated(data), ]
num_duplicates_after_removal <- nrow(duplicates_after_removal)
print(paste("Number of duplicate rows after removal:", num_duplicates_after_removal))


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
data$PurchaseStatus <- factor(data$PurchaseStatus, levels = c(0, 1), labels = c("No", "Yes"))

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

# Separate features and target variable for training and test sets
x_train <- data_train[, -ncol(data_train)]
y_train <- data_train$PurchaseStatus
x_test <- data_test[, -ncol(data_test)]
y_test <- data_test$PurchaseStatus

# Train and evaluate KNN
set.seed(123)
knn_model <- train(PurchaseStatus ~ ., data = data_train, method = "knn", trControl = trainControl(method = "cv", number = 10))
knn_probs <- predict(knn_model, newdata = data_test, type = "prob")
knn_predictions <- predict(knn_model, newdata = data_test)
knn_conf_matrix <- confusionMatrix(knn_predictions, y_test)
print("KNN Confusion Matrix:")
print(knn_conf_matrix)

# Train and evaluate Decision Tree
set.seed(123)
tree_model <- train(PurchaseStatus ~ ., data = data_train, method = "rpart", trControl = trainControl(method = "cv", number = 10))
tree_probs <- predict(tree_model, newdata = data_test, type = "prob")
tree_predictions <- predict(tree_model, newdata = data_test)
tree_conf_matrix <- confusionMatrix(tree_predictions, y_test)
print("Decision Tree Confusion Matrix:")
print(tree_conf_matrix)

# Train and evaluate Logistic Regression
set.seed(123)
log_reg_model <- train(PurchaseStatus ~ ., data = data_train, method = "glm", family = binomial, trControl = trainControl(method = "cv", number = 10))
log_reg_probs <- predict(log_reg_model, newdata = data_test, type = "prob")
log_reg_predictions <- predict(log_reg_model, newdata = data_test)
log_reg_conf_matrix <- confusionMatrix(log_reg_predictions, y_test)
print("Logistic Regression Confusion Matrix:")
print(log_reg_conf_matrix)

##Train and Evaluate random forest
set.seed(123)
rf_model <- train(PurchaseStatus ~ ., data = data_train, method = "rf", trControl = trainControl(method = "cv", number = 10))
rf_probs <- predict(rf_model, newdata = data_test, type = "prob")
rf_predictions <- predict(rf_model, newdata = data_test)
rf_conf_matrix <- confusionMatrix(rf_predictions, data_test$PurchaseStatus)
print("Random Forest Confusion Matrix:")
print(rf_conf_matrix)

# Print the models
print(knn_model)
print(tree_model)
print(log_reg_model)
print(rf_model)

# Prepare data for ROC curve
knn_pred <- prediction(knn_probs[,2], data_test$PurchaseStatus)
tree_pred <- prediction(tree_probs[,2], data_test$PurchaseStatus)
log_reg_pred <- prediction(log_reg_probs[,2], data_test$PurchaseStatus)
rf_pred <- prediction(rf_probs[,2], data_test$PurchaseStatus)

# Calculate performance
knn_perf <- performance(knn_pred, "tpr", "fpr")
tree_perf <- performance(tree_pred, "tpr", "fpr")
log_reg_perf <- performance(log_reg_pred, "tpr", "fpr")
rf_perf <- performance(rf_pred, "tpr", "fpr")

# Plot ROC curves
plot(knn_perf, col = "blue", main = "ROC Curves for KNN, Decision Tree, Logistic Regression, and Random Forest")
plot(tree_perf, add = TRUE, col = "red")
plot(log_reg_perf, add = TRUE, col = "green")
plot(rf_perf, add = TRUE, col = "purple")
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = c("KNN", "Decision Tree", "Logistic Regression", "Random Forest"), col = c("blue", "red", "green", "purple"), lty = 1)