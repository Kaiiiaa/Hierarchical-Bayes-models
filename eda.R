
# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

data_og <- data


Countries <- c("FR", "DE", "ES", "UK")
Industries <- c("C", "B", "D", "E", "F", "G", "H", "M", "N", "B-S_X_O_S94")

data <- df%>%
  dplyr::filter(Indicator == "EMP", CountryCode %in% Countries, Industry %in% Industries, Source == "Eurostat")
  
data2 <- df%>%
  dplyr::filter(Indicator == "ENT", CountryCode %in% Countries, Industry == "B-S_X_O_S94", Source == "Eurostat")

data3 <- df%>%
  dplyr::filter(Indicator %in% c("EMP", "ENT"), CountryCode %in% Countries, Industry == "B-S_X_O_S94", Source == "Eurostat")

ggplot(data2, aes(x = CountryCode)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Firms by Country (Industry B-S_X_O_S94)",
       x = "Country",
       y = "Number of Firms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Industry, fill = Unified_Size_Class)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Employee Size Classes Across Industries (Industry B-S_X_O_S94)",
       x = "Industry",
       y = "Proportion",
       fill = "Size Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Year, y = values, color = CountryCode)) +
  geom_line() +
  facet_wrap(~ Industry) +
  labs(title = "Average Firm Size Over Time by Country and Industry",
       x = "Year",
       y = "Average Firm Size",
       color = "Country") +
  theme_minimal()


avg_size <- data3 %>%
  group_by(CountryCode, Indicator) %>%
  summarize(mean_size = mean(values, na.rm = TRUE))

ggplot(avg_size, aes(x = CountryCode, y = Indicator, fill = mean_size)) +
  geom_tile() +
  labs(title = "Average Number of employees and enterprises Size",
       x = "Country",
       y = "Industry") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

numeric_data <- data %>%
  select(values, Year, Unified_Size_Class) %>%
  drop_na()

# Compute correlation matrix
cor_matrix <- cor(numeric_data)

# Plot correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         title = "Correlation Heatmap of Key Variables",
         tl.col = "black", tl.srt = 45)

# summary statistics over time
summary_stats <- data %>%
  group_by(Year, CountryCode, Industry, Unified_Size_Class) %>%
  summarize(
    mean_size = mean(values, na.rm = TRUE),
    variance_size = var(values, na.rm = TRUE),
    median_size = median(values, na.rm = TRUE),
    q25_size = quantile(values, 0.25, na.rm = TRUE),
    q75_size = quantile(values, 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(summary_stats, aes(x = Year)) +
  geom_line(aes(y = mean_size, color = "Mean")) +
  geom_line(aes(y = variance_size, color = "Variance")) +
  geom_ribbon(aes(ymin = q25_size, ymax = q75_size), fill = "gray", alpha = 0.2) +
  facet_wrap(~ CountryCode + Industry) +
  labs(title = "Functional Summaries of Firm Size Over Time",
       x = "Year",
       y = "Firm Size",
       color = "Statistic") +
  theme_minimal()
