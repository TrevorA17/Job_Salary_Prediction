# Load the saved Random Forest model
loaded_rf_model <- readRDS("./models/random_forest_model.rds")

#* @apiTitle Salary Prediction Model API
#* @apiDescription Used to predict salary.

#* @param Education Education level
#* @param Experience Years of experience
#* @param Location Location
#* @param Job_Title Job title
#* @param Age Age
#* @param Gender Gender

#* @post /predict_salary

predict_salary <- function(Education, Experience, Location, Job_Title, Age, Gender) {
  # Prepare new data for prediction
  new_data <- data.frame(
    Education = factor(Education, levels = levels(salary_data$Education)),
    Experience = as.integer(Experience),
    Location = factor(Location, levels = levels(salary_data$Location)),
    Job_Title = factor(Job_Title, levels = levels(salary_data$Job_Title)),
    Age = as.integer(Age),
    Gender = factor(Gender, levels = levels(salary_data$Gender))
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_rf_model, newdata = new_data)
  
  # Return the prediction
  return(prediction)
}
