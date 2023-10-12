install.packages("caret")
install.packages("randomForest")
install.packages("stringi")
library(caret)s
library(randomForest)
library(ggplot2)
# Load data using read.csv
df<- read.csv("C:\\Users\\Mansi\\Desktop\\Datasets\\dataset_phishing_csv.csv")
# Create a data frame with the status counts
status_counts <- table(df$status)

# Create a bar plot
ggplot(data = as.data.frame(status_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Analysing status feature using bar-chart",
    x = "class labels",
    y = "number of records"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 15)
  )
# Summarize the dataset
summary(df)
# View the first few rows of the dataset
head(df)
# Rename the target variable to "Class" for convenience
names(df)[names(df) == "class"] <- "Class"
# Check the data type of the "status" variable
class(df$status)
# Convert the "status" variable to a factor using factor() and specifying levels
df$status <- factor(df$status, levels = c("legitimate", "phishing"))


# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$status, p = 0.7, list = FALSE)
data_train <- df[trainIndex,]
data_test <- df[-trainIndex,]

# Train a Random Forest model using "status" as the target variable
model <- randomForest(status ~ ., data = data_train)
# Make predictions on the test data
predictions <- predict(model, data_test)

# Confusion matrix
confusion_matrix <- table(data_test$status, predictions)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# You can also calculate other metrics like precision, recall, and F1-score.
# Calculate precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
print(paste("Precision (phishing):", precision))

# Calculate recall
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
print(paste("Recall (phishing):", recall))

# Calculate F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-score (phishing):", f1_score))
