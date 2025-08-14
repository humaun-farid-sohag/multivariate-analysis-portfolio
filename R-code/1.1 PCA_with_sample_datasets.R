
# Step 1 - Importing Required Libraries
library(caret)      # For scaling and train/test split
library(ggplot2)    # For plotting
library(gridExtra)  # For arranging plots
library(reshape2)   # For reshaping confusion matrix
library(dplyr)      # For data manipulation


# Step 2 - Creating Sample Dataset
df <- data.frame(
  Height = c(170, 165, 180, 175, 160, 172, 168, 177, 162, 158),
  Weight = c(65, 59, 75, 68, 55, 70, 62, 74, 58, 54),
  Age    = c(30, 25, 35, 28, 22, 32, 27, 33, 24, 21),
  Gender = factor(c(1, 0, 1, 1, 0, 1, 0, 1, 0, 0), labels = c("Female", "Male"))
)

# Step 3 - Standardizing the Data
X <- df %>% select(-Gender)
y <- df$Gender

preProc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preProc, X)

# Step 4 - Applying PCA algorithm
pca_model <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
X_pca <- pca_model$x[, 1:2]

# Train-test split (70% train, 30% test)
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X_pca[trainIndex, ]
X_test  <- X_pca[-trainIndex, ]
y_train <- y[trainIndex]
y_test  <- y[-trainIndex]

# Logistic Regression Model
train_data <- data.frame(PC1 = X_train[,1], PC2 = X_train[,2], Gender = y_train)
test_data  <- data.frame(PC1 = X_test[,1],  PC2 = X_test[,2],  Gender = y_test)

model <- glm(Gender ~ ., data = train_data, family = binomial)
y_pred_prob <- predict(model, test_data, type = "response")
y_pred <- factor(ifelse(y_pred_prob > 0.5, "Male", "Female"), levels = c("Female", "Male"))


# Step 5 - Evaluating with Confusion Matrix
cm <- confusionMatrix(y_pred, y_test)
print(cm)

# Create a heatmap-like plot
cm_table <- as.data.frame(cm$table)
colnames(cm_table) <- c("True", "Predicted", "Freq")

ggplot(cm_table, aes(x = Predicted, y = True, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "True Label") +
  theme_minimal()


# Step 6 - Visualizing PCA Result

# Before PCA: first 2 standardized features
scaled_df <- data.frame(Feature1 = X_scaled[,1], Feature2 = X_scaled[,2], Gender = y)

p1 <- ggplot(scaled_df, aes(x = Feature1, y = Feature2, color = Gender)) +
  geom_point(size = 3) +
  labs(title = "Before PCA: Using First 2 Standardized Features") +
  theme_minimal()

# After PCA
pca_df <- data.frame(PC1 = X_pca[,1], PC2 = X_pca[,2], Gender = y)

p2 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Gender)) +
  geom_point(size = 3) +
  labs(title = "After PCA: Projected onto 2 Principal Components") +
  theme_minimal()

# Arrange side by side
grid.arrange(p1, p2, ncol = 2)

