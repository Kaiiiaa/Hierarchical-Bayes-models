# Load required libraries
library(bartMachine)
library(tidyr)
library(dplyr)

#  Java memory 
options(java.parameters = "-Xmx8g") 

data_wide <- df_hierarchical %>%
  filter(Indicator %in% c("ENT", "TRN", "EMP"), Source %in% c("Eurostat"), CountryCode %in% c("UK", "FR", "DE", "ES", "IT", "PL", "LT")) %>%
  select(-Unified_Size_Class) %>%
  #mutate(values = log(values))%>%
  pivot_wider(names_from = Indicator, values_from = values)%>%
  drop_na()

impute_lognormal <- function(x) {
  if (all(is.na(x))) return(x)
  observed <- x[!is.na(log(x))]
  x[is.na(x)] <- rlnorm(sum(is.na(x)), meanlog = mean(log(observed), na.rm = TRUE), sdlog = sd(log(observed), na.rm = TRUE))
  return(x)
}

data_wide_imputed <- data_wide %>%
  mutate(across(c("EMP", "ENT", "TRN"), impute_lognormal))
set.seed(42)  # For reproducibility
train_indices <- sample(1:nrow(data_wide_imputed), size = 0.8 * nrow(data_wide_imputed))
train_data <- data_wide_imputed[train_indices, ]
test_data <- data_wide_imputed[-train_indices, ]

predictors <- c("ENT", "TRN")  # predictors
response <- "EMP"  # Target column

bart_model <- bartMachine(
  X = train_data[, predictors, drop = FALSE],
  y = train_data[[response]],
  num_trees = 50,
  num_burn_in = 100,
  num_iterations_after_burn_in = 500,
  verbose = TRUE
)

y_pred <- predict(bart_model, test_data[, predictors, drop = FALSE])
y_test <- test_data[[response]]

rmse <- sqrt(mean((y_test - y_pred)^2))
cat("RMSE:", rmse, "\n")

plot(y_test, y_pred, xlab = "Observed EMP", ylab = "Predicted EMP", main = "BART Model: Observed vs Predicted")
abline(0, 1, col = "red", lty = 2)


print(summary(bart_model))

y_pred <- predict(bart_model, test_data[, predictors])

y_test <- test_data[[response]]
mse <- mean((y_test - y_pred)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))
cat("R-squared (R²):", r_squared, "\n")

plot(y_test, y_pred, pch = 19, col = "blue", main = "Observed vs Predicted",
     xlab = "Observed EMP", ylab = "Predicted EMP")
abline(0, 1, col = "red", lwd = 2)

residuals <- y_test - y_pred
hist(residuals, breaks = 30, col = "skyblue", main = "Residuals Distribution",
     xlab = "Residuals", ylab = "Frequency")
abline(v = 0, col = "red", lwd = 2)

# Residual Analysis
predicted_values <- bart_machine_get_posterior(bart_model, test_data[, predictors])$y_hat
observed_values <- test_data[[response]]
residuals <- observed_values - predicted_values

# Residual Histogram
hist(residuals, 
     breaks = 30, 
     col = "blue", 
     main = "Residuals Distribution", 
     xlab = "Residuals (Observed - Predicted)")

# Q-Q Plot of Residuals
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")

# Residuals vs. Predicted
plot(predicted_values, residuals, 
     main = "Residuals vs Predicted", 
     xlab = "Predicted EMP", 
     ylab = "Residuals", 
     col = "blue", 
     pch = 16)
abline(h = 0, col = "red", lwd = 2)

# Residuals vs Observed
plot(observed_values, residuals, 
     main = "Residuals vs Observed", 
     xlab = "Observed EMP", 
     ylab = "Residuals", 
     col = "blue", 
     pch = 16)
abline(h = 0, col = "red", lwd = 2)


# Add predictions to test data
test_data$Predicted_EMP <- predicted_values
test_data$Residuals <- residuals

# Grouping by Industry
library(ggplot2)

# Plot residuals by industry
ggplot(test_data, aes(x = Industry, y = Residuals)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Residuals by Industry", x = "Industry", y = "Residuals")

# Grouping by Size Class
ggplot(test_data, aes(x = Size_Class, y = Residuals)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Residuals by Size Class", x = "Size Class", y = "Residuals")

# Observed vs Predicted by Industry
ggplot(test_data, aes(x = Predicted_EMP, y = observed_values, color = Industry)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Observed vs Predicted by Industry", 
       x = "Predicted EMP", 
       y = "Observed EMP")



