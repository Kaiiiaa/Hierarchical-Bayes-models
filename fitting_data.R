


# Load libraries
library(tidyverse)
library(fitdistrplus)
library(MCMCpack)
library(bayesplot)
library(rstan)
library(poweRlaw)
if (!require(flexmix)) install.packages("flexmix")
library(flexmix)
df <- data  


df <- df %>%
  mutate(Size_Class = as.character(Size_Class),
         Unified_Size_Class = ifelse(Size_Class %in% c("0-9", "0_1", "2-9", "S1", "S1T9", "S2T9", "TE_S1-", "_S1", "_S2-4", "_S5-9"), 1, 
                                     ifelse(Size_Class %in% c("10-19", "S10T19", "S1T19"), 2, 
                                            ifelse(Size_Class %in% c("20-49", "S20T49", "S10T49"), 3, 
                                                   ifelse(Size_Class %in% c("50-249", "S50T249", "S_GE50", "S_GE20", "S10T249", "S1T249", "S_GE10", "_S10-1", "_S20-4", "S_GE50"), 4, 
                                                          ifelse(Size_Class %in% c("GE250", "S_GE250", "_SGE50"), 5,
                                                                 ifelse(Size_Class %in% c("TOTAL", "_TOTAL", "_T", "TE_TOT", "TE_SGE"), 6, NA)))))))


df_filtered <- df %>%
  filter(!is.na(Unified_Size_Class), !is.na(values), Indicator %in% c("EMP", "ENT"), CountryCode %in% c("UK", "FR", "DE", "ES", "IT", "PL", "LT")) %>%  # Remove NA classes
  dplyr::select(Industry, Size_Class, CountryCode, values, Year, Indicator, Source, Unified_Size_Class)%>%
  mutate(values = ifelse(values<0, NA, values),
         values = as.integer(values))%>%
  group_by(CountryCode, Source, Industry, Indicator, Size_Class) %>%
  filter(all(values > 0)) %>%   
  ungroup() %>%                 
  drop_na()  

# Check distribution of firm sizes by the new unified size class
ggplot(df_filtered, aes(x = values)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  facet_wrap(~Unified_Size_Class + Indicator + CountryCode, scales = "free") +
  theme_minimal() +
  labs(title = "Firm Size Distribution by Unified Employee Class and Indicator",
       x = "Firm Size",
       y = "Count")



fit_distributions_by_class <- function(df_class) {
  cat("Fitting distributions for Unified_Size_Class:", unique(df_class$Unified_Size_Class), unique(df_class$Indicator), "\n")
  
  # Remove non-positive values for fitting Pareto distribution
  positive_values <- df_class$values[df_class$values > 0]
  
  # Fit Pareto distribution (using poweRlaw)
  fit_pareto <- tryCatch({
    if (length(positive_values) > 1) {
      pl <- conpl$new(positive_values)
      est <- estimate_xmin(pl, xmax = 1e+07)  # Increase xmax for the search space
      pl$setXmin(est)
      print(summary(pl))
      pl  # Return the fitted Pareto object
    } else {
      cat("Skipping Pareto fitting for this group due to insufficient positive values.\n")
      NULL
    }
  }, error = function(e) {
    cat("Error fitting Pareto distribution:", e$message, "\n")
    NULL
  })
  
  # Fit Poisson distribution
  fit_poisson <- tryCatch({
    # Check if all values are non-negative integers for Poisson fitting
    if (all(df_class$values >= 0) && all(df_class$values == round(df_class$values))) {
      fit_poisson <- fitdist(df_class$values, "pois")
      print(summary(fit_poisson))
      fit_poisson  # Return the fitted Poisson object
    } else {
      cat("Skipping Poisson fitting for this group due to non-integer or negative values.\n")
      NULL
    }
  }, error = function(e) {
    cat("Error fitting Poisson distribution:", e$message, "\n")
    NULL
  })
  
  if (!is.null(fit_poisson) && !is.null(fit_pareto)) {
    # Custom comparison plot
    ggplot() +
      geom_line(aes(x = positive_values, y = dpois(positive_values, fit_poisson$estimate["lambda"])),
                color = "blue", linetype = "dashed", size = 1, alpha = 0.8, label = "Poisson") +
      geom_line(aes(x = positive_values, y = dpareto(positive_values, xmin = fit_pareto$getXmin(), alpha = fit_pareto$getPars())),
                color = "red", linetype = "dotted", size = 1, alpha = 0.8, label = "Pareto") +
      labs(title = "Comparison of Fitted Distributions",
           x = "Values", y = "Density") +
      theme_minimal()
  }
}


dist_info <- df_filtered %>%
  group_by(Unified_Size_Class, Indicator) %>%
  group_split() %>%
  walk(fit_distributions_by_class)

