library(data.table)
library(e1071)

dt_data <- fread("uber-app/app-data/uber-data.csv")

# Split the data into training and testing sets
train_idx <- sample(nrow(dt_data), 0.8 * nrow(dt_data))
train_data <- dt_data[train_idx]
test_data <- dt_data[-train_idx]

# Define the diferent parameters to test when tuning
# tune_params <- list(
#   kernel = c("linear", "polynomial", "radial", "sigmoid"),
#   gamma = c(0.1, 1, 5, 10),
#   cost = c(0.1, 1, 10, 100)
# )

# Perform hyperparameter tuning, requires supercomputer
# tune_results <- tune(
#   svm,
#   Formatted.Day ~ Borough + Formatted.Month + Formatted.Hour,
#   data = train_data,
#   ranges = tune_params,
#   tunecontrol = tune.control(cross = 5)
# )

# print(tune_results$best.parameters)
# best <- tune_results$best.parameters

best <- NULL
best$kernel <- "radial"
best$gamma <- "0.1"
best$cost <- "0.1"

# Create Support Vector Machine using tuned parameters
svm_model <- svm(
  as.factor(Formatted.Day) ~ Borough + Formatted.Month + Formatted.Hour,
  data = train_data,
  kernel = best$kernel, 
  gamma = best$gamma, 
  cost = best$cost,
  scale = TRUE
)
