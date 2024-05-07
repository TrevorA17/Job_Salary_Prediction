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

# Measures of Central Tendency
# Mean, Median, Mode for Experience
experience_mean <- mean(salary_data$Experience)
experience_median <- median(salary_data$Experience)
experience_mode <- as.numeric(names(sort(table(salary_data$Experience), decreasing = TRUE)[1]))
print("Measures of central tendency for Experience:")
print(paste("Mean:", experience_mean))
print(paste("Median:", experience_median))
print(paste("Mode:", experience_mode))

# Mean, Median, Mode for Age
age_mean <- mean(salary_data$Age)
age_median <- median(salary_data$Age)
age_mode <- as.numeric(names(sort(table(salary_data$Age), decreasing = TRUE)[1]))
print("Measures of central tendency for Age:")
print(paste("Mean:", age_mean))
print(paste("Median:", age_median))
print(paste("Mode:", age_mode))

# Mean, Median for Salary
salary_mean <- mean(salary_data$Salary)
salary_median <- median(salary_data$Salary)
print("Measures of central tendency for Salary:")
print(paste("Mean:", salary_mean))
print(paste("Median:", salary_median))

# Measures of Distribution
# Range for Experience
experience_range <- range(salary_data$Experience)
print("Range for Experience:")
print(experience_range)

# Range for Age
age_range <- range(salary_data$Age)
print("Range for Age:")
print(age_range)

# Range for Salary
salary_range <- range(salary_data$Salary)
print("Range for Salary:")
print(salary_range)

# Variance for Experience
experience_variance <- var(salary_data$Experience)
print("Variance for Experience:")
print(experience_variance)

# Variance for Age
age_variance <- var(salary_data$Age)
print("Variance for Age:")
print(age_variance)

# Variance for Salary
salary_variance <- var(salary_data$Salary)
print("Variance for Salary:")
print(salary_variance)

# Standard Deviation for Experience
experience_sd <- sd(salary_data$Experience)
print("Standard Deviation for Experience:")
print(experience_sd)

# Standard Deviation for Age
age_sd <- sd(salary_data$Age)
print("Standard Deviation for Age:")
print(age_sd)

# Standard Deviation for Salary
salary_sd <- sd(salary_data$Salary)
print("Standard Deviation for Salary:")
print(salary_sd)

# Quartiles for Experience
experience_quartiles <- quantile(salary_data$Experience, probs = c(0.25, 0.5, 0.75))
print("Quartiles for Experience:")
print(experience_quartiles)

# Quartiles for Age
age_quartiles <- quantile(salary_data$Age, probs = c(0.25, 0.5, 0.75))
print("Quartiles for Age:")
print(age_quartiles)

# Quartiles for Salary
salary_quartiles <- quantile(salary_data$Salary, probs = c(0.25, 0.5, 0.75))
print("Quartiles for Salary:")
print(salary_quartiles)

# Measures of Relationship
# Correlation between Age and Salary
age_salary_correlation <- cor(salary_data$Age, salary_data$Salary)
print("Correlation between Age and Salary:")
print(age_salary_correlation)

# Covariance between Experience and Salary
experience_salary_covariance <- cov(salary_data$Experience, salary_data$Salary)
print("Covariance between Experience and Salary:")
print(experience_salary_covariance)

# ANOVA for Salary across different levels of Education
anova_result <- aov(Salary ~ Education, data = salary_data)
print("ANOVA for Salary across different levels of Education:")
print(summary(anova_result))

# ANOVA for Salary across different levels of Location
anova_result <- aov(Salary ~ Location, data = salary_data)
print("ANOVA for Salary across different levels of Location:")
print(summary(anova_result))

# ANOVA for Salary across different levels of Job_Title
anova_result <- aov(Salary ~ Job_Title, data = salary_data)
print("ANOVA for Salary across different levels of Job_Title:")
print(summary(anova_result))
