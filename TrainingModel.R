# Load dataset
salary_data <- read.csv("data/salary_prediction_data.csv", colClasses = c(
  Education = "factor",
  Experience = "integer",
  Location = "factor",
  Job_Title = "factor",
  Age = "integer",
  Gender = "factor",
  Salary = "numeric"
))

# Display the structure of the dataset
str(salary_data)

# View the first few rows of the dataset
head(salary_data)

# View the dataset in a separate viewer window
View(salary_data)

# Load required library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets
train_index <- createDataPartition(salary_data$Salary, p = 0.8, list = FALSE)
train_data <- salary_data[train_index, ]
test_data <- salary_data[-train_index, ]

# Display the dimensions of the training and testing sets
print("Dimensions of Training Set:")
print(dim(train_data))
print("Dimensions of Testing Set:")
print(dim(test_data))

# Load required library
library(boot)

# Define the function to calculate the statistic of interest (e.g., mean)
statistic_function <- function(data, indices) {
  sampled_data <- data[indices, ]
  # Here, you can compute any statistic you're interested in
  # For example, let's compute the mean of Salary
  mean_salary <- mean(sampled_data$Salary)
  return(mean_salary)
}

# Set seed for reproducibility
set.seed(123)

# Perform bootstrapping
bootstrap_results <- boot(data = salary_data, statistic = statistic_function, R = 1000)

# Print the bootstrapped confidence interval
print(boot.ci(bootstrap_results, type = "basic"))

#Cross-Validation
# Set seed for reproducibility
set.seed(123)

# Define training control
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using cross-validation
# Here, you can replace 'lm' with any other modeling function (e.g., 'glm', 'randomForest')
model <- train(Salary ~ ., data = salary_data, method = "lm", trControl = train_control)

# Print the cross-validation results
print(summary(model))

# Train a linear regression model
lm_model <- lm(Salary ~ ., data = salary_data)

# Print the summary of the trained model
summary(lm_model)

# Load required library
library(rpart)

# Train a decision tree model
tree_model <- rpart(Salary ~ ., data = salary_data)

# Print the summary of the trained model
print(tree_model)

# Load required library
library(randomForest)

# Train a random forest model
rf_model <- randomForest(Salary ~ ., data = salary_data)

# Print the summary of the trained model
print(rf_model)

# Load required library
library(gbm)

# Train a GBM model
gbm_model <- gbm(Salary ~ ., data = salary_data, distribution = "gaussian", n.trees = 100, interaction.depth = 4)

# Print the summary of the trained model
print(gbm_model)

# Define training control with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Define models to compare
models <- list(
  linear = train(Salary ~ ., data = salary_data, method = "lm", trControl = train_control),
  decision_tree = train(Salary ~ ., data = salary_data, method = "rpart", trControl = train_control),
  random_forest = train(Salary ~ ., data = salary_data, method = "rf", trControl = train_control),
  gbm = train(Salary ~ ., data = salary_data, method = "gbm", trControl = train_control)
)

# Compare model performance using resamples
model_comparison <- resamples(models)

# Summarize the model comparison results
summary(model_comparison)
