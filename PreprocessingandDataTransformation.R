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

# Check for missing values
missing_values <- anyNA(salary_data)
if (missing_values) {
  print("Missing values are present in the dataset.")
} else {
  print("No missing values are present in the dataset.")
}
# Round off Salary to 2 decimal places
salary_data$Salary <- round(salary_data$Salary, 2)

# View the first few rows of the updated dataset
head(salary_data)
