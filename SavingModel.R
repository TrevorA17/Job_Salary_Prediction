# Saving the Random Forest model
saveRDS(rf_model, "./models/random_forest_model.rds")

# Load the saved model
loaded_rf_model <- readRDS("./models/random_forest_model.rds")

# Prepare new data for prediction
new_data <- data.frame(
  Education = factor("Bachelor", levels = levels(salary_data$Education)),  # Ensure factor levels match training data
  Experience = 8,
  Location = factor("Urban", levels = levels(salary_data$Location)),  # Ensure factor levels match training data
  Job_Title = factor("Manager", levels = levels(salary_data$Job_Title)),  # Ensure factor levels match training data
  Age = 35,
  Gender = factor("Male", levels = levels(salary_data$Gender))  # Ensure factor levels match training data
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
