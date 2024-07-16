
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GGally)

# Read the dataset
#data <- read.csv("customer_purchase_data.csv", stringsAsFactors = TRUE)
data <- read.csv("customer_purchase_data.csv")

# Structure of the data
str(data)
# Statistical summary of the data:
summary(data)

# Convert categorical variables to factors
data$Gender <- factor(data$Gender, levels = c(0, 1), labels = c("Male", "Female"))
data$ProductCategory <- factor(data$ProductCategory, levels = 0:4, labels = c("Electronics", "Clothing", "Home Goods", "Beauty", "Sports"))
data$LoyaltyProgram <- factor(data$LoyaltyProgram, levels = c(0, 1), labels = c("No", "Yes"))
data$PurchaseStatus <- factor(data$PurchaseStatus, levels = c(0, 1), labels = c("No", "Yes"))

####DISTRIBUTION PLOTS

# Plot distribution of Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue",color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'bin', binwidth = 5, aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Age", x = "Age", y = "Count")

# Plot distribution of Gender
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "purple", color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")

# Plot distribution of Annual Income
ggplot(data, aes(x = AnnualIncome)) +
  geom_histogram(binwidth = 10000, fill = "green",color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'bin', binwidth = 10000, aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Annual Income", x = "Annual Income", y = "Count")

# Plot distribution of Number of Purchases
ggplot(data, aes(x = NumberOfPurchases)) +
  geom_histogram(binwidth = 1, fill = "orange",color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'bin', binwidth = 1, aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Number of Purchases", x = "Number of Purchases", y = "Count")

# Plot distribution of Product Category
ggplot(data, aes(x = ProductCategory)) +
  geom_bar(fill = "red",color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Product Category", x = "Product Category", y = "Count")

# Plot distribution of Time Spent on Website
ggplot(data, aes(x = TimeSpentOnWebsite)) +
  geom_histogram(binwidth = 10, fill = "cyan",color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'bin', binwidth = 10, aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Time Spent on Website", x = "Time Spent on Website", y = "Count")

# Plot distribution of Loyalty Program
ggplot(data, aes(x = LoyaltyProgram)) +
  geom_bar(fill = "brown", color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Loyalty Program", x = "Loyalty Program", y = "Count")

# Plot distribution of Discounts Availed
ggplot(data, aes(x = DiscountsAvailed)) +
  geom_histogram(binwidth = 1, fill = "magenta",color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'bin', binwidth = 1, aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of Discounts Availed", x = "Discounts Availed", y = "Count")

# Plot distribution of PurchaseStatus
ggplot(data, aes(x = PurchaseStatus)) +
  geom_bar(fill = "grey", color="#e9ecef", alpha = 0.9) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Distribution of PurchaseStatus", x = "Purchase Status", y = "Count")

# Boxplot of Age
ggplot(data, aes(y = Age)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Boxplot of Age", y = "Age")

# Boxplot of Annual Income
ggplot(data, aes(y = AnnualIncome)) +
  geom_boxplot(fill = "green", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Boxplot of Annual Income", y = "Annual Income")

# Boxplot of Number of Purchases
ggplot(data, aes(y = NumberOfPurchases)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Boxplot of Number of Purchases", y = "Number of Purchases")

# Boxplot of Time Spent on Website
ggplot(data, aes(y = TimeSpentOnWebsite)) +
  geom_boxplot(fill = "cyan", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Boxplot of Time Spent on Website", y = "Time Spent on Website")

# Boxplot of Discounts Availed
ggplot(data, aes(y = DiscountsAvailed)) +
  geom_boxplot(fill = "magenta", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Boxplot of Discounts Availed", y = "Discounts Availed")

#####IMPACT OF VARIABLES ON TARGET VARIABLE

# Plot Age in relation to PurchaseStatus
ggplot(data, aes(x = PurchaseStatus, y = Age, fill = PurchaseStatus)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Age in Relation to Purchase Status", x = "Purchase Status", y = "Age")

# Plot Gender in relation to PurchaseStatus
ggplot(data, aes(x = Gender, fill = PurchaseStatus)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Gender in Relation to Purchase Status", x = "Gender", y = "Proportion")

# Plot Annual Income in relation to PurchaseStatus
ggplot(data, aes(x = PurchaseStatus, y = AnnualIncome, fill = PurchaseStatus)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Annual Income in Relation to Purchase Status", x = "Purchase Status", y = "Annual Income")

# Plot Number of Purchases in relation to PurchaseStatus
ggplot(data, aes(x = PurchaseStatus, y = NumberOfPurchases, fill = PurchaseStatus)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Number of Purchases in Relation to Purchase Status", x = "Purchase Status", y = "Number of Purchases")

# Plot Product Category in relation to PurchaseStatus
ggplot(data, aes(x = ProductCategory, fill = PurchaseStatus)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Product Category in Relation to Purchase Status", x = "Product Category", y = "Proportion")

# Plot Time Spent on Website in relation to PurchaseStatus
ggplot(data, aes(x = PurchaseStatus, y = TimeSpentOnWebsite, fill = PurchaseStatus)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Time Spent on Website in Relation to Purchase Status", x = "Purchase Status", y = "Time Spent on Website")

# Plot Loyalty Program in relation to PurchaseStatus
ggplot(data, aes(x = LoyaltyProgram, fill = PurchaseStatus)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Loyalty Program in Relation to Purchase Status", x = "Loyalty Program", y = "Proportion")

# Plot Discounts Availed in relation to PurchaseStatus
ggplot(data, aes(x = PurchaseStatus, y = DiscountsAvailed, fill = PurchaseStatus)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Discounts Availed in Relation to Purchase Status", x = "Purchase Status", y = "Discounts Availed")
