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

# Measures of Frequency
# Frequency table for Education
education_freq <- table(salary_data$Education)
print("Frequency table for Education:")
print(education_freq)

# Frequency table for Location
location_freq <- table(salary_data$Location)
print("Frequency table for Location:")
print(location_freq)

# Frequency table for Job_Title
job_title_freq <- table(salary_data$Job_Title)
print("Frequency table for Job_Title:")
print(job_title_freq)

# Summary of Gender
gender_summary <- summary(salary_data$Gender)
print("Summary of Gender:")
print(gender_summary)
