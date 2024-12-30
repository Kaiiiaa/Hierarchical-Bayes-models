# Load the libraries
library(rstan)
library(bayesplot)
library(tidyverse)
library(fitdistrplus)
library(gamlss)


# Prepare the data
df_hierarchical <- df %>%
  filter(!is.na(Unified_Size_Class), !is.na(values)) %>%  # Remove NA classes
  dplyr::select(Industry, Size_Class, CountryCode, values, Year, Indicator, Source, Unified_Size_Class)%>%
  mutate(values = ifelse(values<0, NA, values),
         values = as.integer(values),
         Industry = as.factor(Industry),  # Ensure Industry is a factor
         CountryCode = as.factor(CountryCode)  # Ensure CountryCode is a factor
  )

df_hierarchical_og<- df_hierarchical
non_missing_data <- df_hierarchical %>%
  filter(!is.na(values))

# Fit a Zero-Inflated Log-Normal (ZILN) model to the data
# The ZALG family in gamlss fits a Zero-Adjusted Log-Normal model
# Initial fit with fewer iterations
fit_ziln <- gamlss(values ~ 1, family = ZAP, data = non_missing_data,
                   control = gamlss.control(n.cyc = 20, trace = TRUE))

# Continue fitting from the previous fit
fit_ziln1 <- gamlss(values ~ 1, family = ZALG, data = non_missing_data,
                   control = gamlss.control(n.cyc = 5, trace = TRUE))


filename <- paste0("after_fit_ziln_", Sys.Date(), "_", format(Sys.time(), "%H-%M-%S"), ".RData")

# Save the workspace with the dynamic filename
save.image(file = filename)
# View a summary of the fitted ZILN model
summary(fit_ziln)

# Number of missing values
num_missing <- sum(is.na(df_hierarchical$values))

# Predict expected values from the Zero-Adjusted Poisson model for missing entries
# Using the fitted ZAP model `fit_ziln` to predict the expected values
predicted_values <- predict(fit_ziln, what = "mu", type = "response")[is.na(df_hierarchical$values)]

# Impute missing values based on the fitted ZAP model
# For ZAP, we can use the expected value `mu` from the Poisson component
# `rpois` for Poisson sampling around the predicted mean

set.seed(123)
imputed_values <- rpois(num_missing, lambda = predicted_values)

# Replace missing values with the imputed values
df_hierarchical$values[is.na(df_hierarchical$values)] <- imputed_values
df_hierarchical<-read.csv("df_hierarchical.csv")
#write.csv(df_hierarchical, "df_hierarchical.csv")
# Create a filename with date and time
filename <- paste0("before_stand_model_code_", Sys.Date(), "_", format(Sys.time(), "%H-%M-%S"), ".RData")

# Save the workspace with the dynamic filename
save.image(file = filename)

