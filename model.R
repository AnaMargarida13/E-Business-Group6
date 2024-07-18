# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(caret)
library(caTools)
library(ROCR)

#### Data Preparation #### 

#Read the dataset
data <- read.csv("E-Business-Group6/customer_purchase_data.csv", stringsAsFactors = TRUE)
#data <- read.csv("customer_purchase_data.csv")

#Structure of the data
str(data)

#Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)

#Check for duplicates
duplicates <- data[duplicated(data), ]
num_duplicates <- nrow(duplicates)
print(paste("Number of duplicate rows:", num_duplicates))

#Remove duplicate rows
data <- data %>% distinct()

#Verify that duplicates are removed
duplicates_after_removal <- data[duplicated(data), ]
num_duplicates_after_removal <- nrow(duplicates_after_removal)
print(paste("Number of duplicate rows after removal:", num_duplicates_after_removal))

#Numerical Columns
numerical_cols <- c("Age", "AnnualIncome", "NumberOfPurchases", "TimeSpentOnWebsite", "DiscountsAvailed")

#Categorical columns
categorical_cols <- c("Gender", "ProductCategory", "LoyaltyProgram")
for (col in categorical_cols) {
  if (any(is.na(data[[col]]))) {
    mode <- as.character(stats::mode(data[[col]]))
    data[[col]][is.na(data[[col]])] <- mode
  }
}

#Encoding Categorical Variables
data$PurchaseStatus <- factor(data$PurchaseStatus, levels = c(0, 1), labels = c("No", "Yes"))


#Scale numerical features
data[numerical_cols] <- scale(data[numerical_cols])

#Splitting Dataset
set.seed(123) # for reproducibility
split = sample.split(data$PurchaseStatus, SplitRatio = .8)
data_train <- filter(data, split == TRUE) 
data_test <- filter(data, split == FALSE)

#Check dimensions of train and test set
dim(data_train)
dim(data_test)

#Drop the Age and ProductCategory columns as we tested for their importance
data_train <- data_train %>% select(-Gender, -ProductCategory)
data_test <- data_test %>% select(-Gender, -ProductCategory)

#Separate features and target variable for training and test sets
x_train <- data_train[, -ncol(data_train)]
y_train <- data_train$PurchaseStatus
x_test <- data_test[, -ncol(data_test)]
y_test <- data_test$PurchaseStatus

#Function to evaluate and compare model performance on train and test datasets
evaluate_model <- function(model, train_data, test_data, target_colname) {
  x_train <- train_data[, -which(names(train_data) == target_colname)]
  y_train <- train_data[[target_colname]]
  x_test <- test_data[, -which(names(test_data) == target_colname)]
  y_test <- test_data[[target_colname]]
  
  #Predict and evaluate on training data
  train_preds <- predict(model, newdata = x_train, type = "raw")
  train_probs <- predict(model, newdata = x_train, type = "prob")
  train_conf_matrix <- confusionMatrix(train_preds, y_train)
  train_pred <- prediction(train_probs[, 2], y_train)
  train_auc <- performance(train_pred, measure = "auc")@y.values[[1]]
  
  #Predict and evaluate on test data
  test_preds <- predict(model, newdata = x_test, type = "raw")
  test_probs <- predict(model, newdata = x_test, type = "prob")
  test_conf_matrix <- confusionMatrix(test_preds, y_test)
  test_pred <- prediction(test_probs[, 2], y_test)
  test_auc <- performance(test_pred, measure = "auc")@y.values[[1]]
  
  return(list(train_conf_matrix = train_conf_matrix,
              test_conf_matrix = test_conf_matrix,
              train_auc = train_auc,
              test_auc = test_auc))
}

#### Model Training ####

#Train and evaluate Logistic Regression
set.seed(123)
log_reg_model <- train(PurchaseStatus ~ ., data = data_train, method = "glm", family = binomial, trControl = trainControl(method = "cv", number = 10))
log_reg_probs <- predict(log_reg_model, newdata = data_test, type = "prob")
log_reg_predictions <- predict(log_reg_model, newdata = data_test)
log_reg_conf_matrix <- confusionMatrix(log_reg_predictions, y_test)
print("Logistic Regression Confusion Matrix:")
print(log_reg_conf_matrix)

summary(log_reg_model)

# Extract and plot feature importance for Logistic Regression -- Not needed after Feature Dropping
#log_reg_importance <- varImp(log_reg_model, scale = TRUE)
#log_reg_importance_df <- as.data.frame(log_reg_importance$importance)
#log_reg_importance_df$Feature <- rownames(log_reg_importance_df)
#log_reg_importance_df <- log_reg_importance_df %>% arrange(desc(Overall))

#p1 <- ggplot(log_reg_importance_df, aes(x = reorder(Feature, Overall), y = Overall)) +
#  geom_bar(stat = "identity", fill = "steelblue") +
#  geom_text(aes(label = round(Overall, 2)), hjust = -0.2, size = 3) +
#  coord_flip() +
#  labs(title = "Feature Importance from Logistic Regression",
#       x = "Features",
#       y = "Importance") +
#  theme_minimal() +
#  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

#Train and evaluate KNN
set.seed(123)
knn_model <- train(PurchaseStatus ~ ., data = data_train, method = "knn", trControl = trainControl(method = "cv", number = 10))
knn_probs <- predict(knn_model, newdata = data_test, type = "prob")
knn_predictions <- predict(knn_model, newdata = data_test)
knn_conf_matrix <- confusionMatrix(knn_predictions, y_test)
print("KNN Confusion Matrix:")
print(knn_conf_matrix)

summary(knn_model)


#Train and evaluate Decision Tree
set.seed(123)
tree_model <- train(PurchaseStatus ~ ., data = data_train, method = "rpart", trControl = trainControl(method = "cv", number = 10))
tree_probs <- predict(tree_model, newdata = data_test, type = "prob")
tree_predictions <- predict(tree_model, newdata = data_test)
tree_conf_matrix <- confusionMatrix(tree_predictions, y_test)
print("Decision Tree Confusion Matrix:")
print(tree_conf_matrix)

summary(tree_model)

#Train and evaluate Decision Tree incorporating Hyperparameter Tuning
set.seed(123)
tree_model_tune <- train(PurchaseStatus ~ ., data = data_train, method = "rpart",
                    trControl = trainControl(method = "cv", number = 10, search = "grid"),
                    tuneLength = 15)
tree_probs_tune <- predict(tree_model_tune, newdata = data_test, type = "prob")
tree_predictions_tune <- predict(tree_model_tune, newdata = data_test)
tree_conf_matrix_tune <- confusionMatrix(tree_predictions_tune, y_test)
print("Decision Tree after Hyperparameter Tuning Confusion Matrix:")
print(tree_conf_matrix_tune)
summary(tree_model_tune)

#Visualize Decision Tree
rpart.plot(tree_model$finalModel, main = "Decision Tree")

#Visualize Decision Tree after Tuning
rpart.plot(tree_model_tune$finalModel, main = "Decision Tree after Hyperparameter Tuning")

# Extract and plot feature importance for Decision Tree
# tree_importance <- varImp(tree_model, scale = TRUE)
# tree_importance_df <- as.data.frame(tree_importance$importance)
# tree_importance_df$Feature <- rownames(tree_importance_df)
# tree_importance_df <- tree_importance_df %>% arrange(desc(Overall))
# 
# p2 <- ggplot(tree_importance_df, aes(x = reorder(Feature, Overall), y = Overall)) +
#   geom_bar(stat = "identity", fill = "mediumseagreen") +
#   geom_text(aes(label = round(Overall, 2)), hjust = -0.2, size = 3) +
#   coord_flip() +
#   labs(title = "Feature Importance from Decision Tree",
#        x = "Features",
#        y = "Importance") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))


##Train and Evaluate random forest
set.seed(123)
rf_model <- train(PurchaseStatus ~ ., data = data_train, method = "rf", trControl = trainControl(method = "cv", number = 10))
rf_probs <- predict(rf_model, newdata = data_test, type = "prob")
rf_predictions <- predict(rf_model, newdata = data_test)
rf_conf_matrix <- confusionMatrix(rf_predictions, data_test$PurchaseStatus)
print("Random Forest Confusion Matrix:")
print(rf_conf_matrix)

summary(rf_model)

# # Extract and plot feature importance for Random Forest
# rf_importance <- varImp(rf_model, scale = TRUE)
# rf_importance_df <- as.data.frame(rf_importance$importance)
# rf_importance_df$Feature <- rownames(rf_importance_df)
# rf_importance_df <- rf_importance_df %>% arrange(desc(Overall))
# 
# p3 <- ggplot(rf_importance_df, aes(x = reorder(Feature, Overall), y = Overall)) +
#   geom_bar(stat = "identity", fill = "coral") +
#   geom_text(aes(label = round(Overall, 2)), hjust = -0.2, size = 3) +
#   coord_flip() +
#   labs(title = "Feature Importance from Random Forest",
#        x = "Features",
#        y = "Importance") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
# 
# # Print the plots
# print(p1)
# print(p2)
# print(p3)

# Print the models
#print(knn_model)
#print(tree_model)
#print(log_reg_model)
#print(rf_model)

#### Model Evaluation #### 

#Prepare data for ROC curve
knn_pred <- prediction(knn_probs[,2], data_test$PurchaseStatus)
tree_pred <- prediction(tree_probs[,2], data_test$PurchaseStatus)
tree_pred_tuned <- prediction(tree_probs_tune[,2], data_test$PurchaseStatus)
log_reg_pred <- prediction(log_reg_probs[,2], data_test$PurchaseStatus)
rf_pred <- prediction(rf_probs[,2], data_test$PurchaseStatus)

#Calculate performance
knn_perf <- performance(knn_pred, "tpr", "fpr")
tree_perf <- performance(tree_pred, "tpr", "fpr")
tree_perf_tuned <- performance(tree_pred_tuned, "tpr", "fpr")
log_reg_perf <- performance(log_reg_pred, "tpr", "fpr")
rf_perf <- performance(rf_pred, "tpr", "fpr")

#Calculate AUC
log_reg_auc <- performance(log_reg_pred, measure = "auc")@y.values[[1]]
knn_auc <- performance(knn_pred, measure = "auc")@y.values[[1]]
tree_auc <- performance(tree_pred, measure = "auc")@y.values[[1]]
tree_auc_tuned <- performance(tree_pred_tuned, measure = "auc")@y.values[[1]]
rf_auc <- performance(rf_pred, measure = "auc")@y.values[[1]]

#Plot ROC curves with AUC Scores
plot(knn_perf, col = "blue", main = "ROC Curves with AUC")
plot(tree_perf, add = TRUE, col = "red")
plot(tree_perf_tuned, add = TRUE, col = "darkred")
plot(log_reg_perf, add = TRUE, col = "green")
plot(rf_perf, add = TRUE, col = "purple")
abline(a = 0, b = 1, lty = 2, col = "gray")

legend("bottomright", 
       legend = c(paste("KNN (AUC =", round(knn_auc, 3), ")"), 
                  paste("Decision Tree (AUC =", round(tree_auc, 3), ")"), 
                  paste("Decision Tree with HP Tuning (AUC =", round(tree_auc_tuned, 3), ")"), 
                  paste("Logistic Regression (AUC =", round(log_reg_auc, 3), ")"), 
                  paste("Random Forest (AUC =", round(rf_auc, 3), ")")), 
       col = c("blue", "red", "darkred", "green", "purple"), lty = 1)

#Evaluate models on both train and test datasets
models <- list(log_reg_model = log_reg_model, 
               knn_model = knn_model,
               tree_model = tree_model, 
               tree_model_tune = tree_model_tune,
               rf_model = rf_model)

results <- lapply(models, evaluate_model, train_data = data_train, test_data = data_test, target_colname = "PurchaseStatus")

#Print results
for (model_name in names(results)) {
  cat("\nPerformance for ", model_name, ":\n", sep = "")
  cat("Training Confusion Matrix:\n")
  print(results[[model_name]]$train_conf_matrix)
  cat("Test Confusion Matrix:\n")
  print(results[[model_name]]$test_conf_matrix)
  cat("\nTraining AUC: ", results[[model_name]]$train_auc, "\n", sep = "")
  cat("Test AUC: ", results[[model_name]]$test_auc, "\n", sep = "")
  cat("-------------------------------------------------------\n")
}

#Calculate accuracy for each model
log_reg_acc <- results$log_reg_model$test_conf_matrix$overall["Accuracy"]
knn_acc <- results$knn_model$test_conf_matrix$overall["Accuracy"]
tree_acc <- results$tree_model$test_conf_matrix$overall["Accuracy"]
tree_tuned_acc <- results$tree_model_tune$test_conf_matrix$overall["Accuracy"]
rf_acc <- results$rf_model$test_conf_matrix$overall["Accuracy"]

#Create a data frame with AUC and accuracy
model_performance <- data.frame(
  Model = c("Logistic Regression", "KNN", "Decision Tree", "Decision Tree (Tuned)", "Random Forest"),
  AUC = c(log_reg_auc, knn_auc, tree_auc, tree_auc_tuned, rf_auc),
  Accuracy = c(log_reg_acc, knn_acc, tree_acc, tree_tuned_acc, rf_acc)
)

#Transform Data into Uniform Shape
performance_long <- data.frame(
  Model = rep(model_performance$Model, 2),
  Metric = c(rep("AUC", nrow(model_performance)), rep("Accuracy", nrow(model_performance))),
  Value = c(model_performance$AUC, model_performance$Accuracy)
)

#Create bar chart
ggplot(performance_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3.5) +
  labs(title = "Model Performance on Test Set",
       x = "Model",
       y = "Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#Define cost matrix
cost_matrix <- matrix(c(0, 2, 1, 0.5), nrow = 2, byrow = TRUE)
rownames(cost_matrix) <- c("No", "Yes")
colnames(cost_matrix) <- c("No", "Yes")

#Function to calculate the total cost
calculate_cost <- function(conf_matrix, cost_matrix) {
  total_cost <- sum(conf_matrix$table * cost_matrix)
  return(total_cost)
}

#Calculate total costs for each model
total_cost_log_reg <- calculate_cost(results$log_reg_model$test_conf_matrix, cost_matrix)
total_cost_knn <- calculate_cost(results$knn_model$test_conf_matrix, cost_matrix)
total_cost_tree <- calculate_cost(results$tree_model$test_conf_matrix, cost_matrix)
total_cost_tree_tuned <- calculate_cost(results$tree_model_tune$test_conf_matrix, cost_matrix)
total_cost_rf <- calculate_cost(results$rf_model$test_conf_matrix, cost_matrix)

#Create a data frame with the total costs
model_costs <- data.frame(
  Model = c("Logistic Regression", "KNN", "Decision Tree", "Decision Tree (Tuned)", "Random Forest"),
  TotalCost = c(total_cost_log_reg, total_cost_knn, total_cost_tree, total_cost_tree_tuned, total_cost_rf)
)

#Sort the data frame by TotalCost in descending order
model_costs <- model_costs %>% arrange(desc(TotalCost))

#Plot the total costs
ggplot(model_costs, aes(x = reorder(Model, -TotalCost), y = TotalCost, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(TotalCost, 2)), vjust = -0.3, size = 3.5) +
  labs(title = "Total Costs for Each Model",
       x = "Model",
       y = "Total Cost ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))