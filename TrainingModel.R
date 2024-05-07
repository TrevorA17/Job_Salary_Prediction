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
