library(readxl)
library(tidyverse)

psy_controls_data <- read_excel("for_despina_and_giannis_200224.xlsx")

# List of columns to clean
columns_to_clean <- c(
  "number_of_sites",
  "Number_of_sessions",
  "frequency_weeks",
  "Length_of_sessions_mins",
  "Total_hours_of_intervention_hours",
  "Total_period_of_intervention_weeks"
)

# Convert non-numeric values to NA in specified columns
cleaned_data <- psy_controls_data %>%
  mutate_at(vars(columns_to_clean), ~as.numeric(as.character(.)))


group_function <- function(dataframe, my_groups,columns) {
  result_dataframe <- data.frame(
    Group = character(),
    Column = character(),
    N = numeric(),
    Mean = numeric(),
    Std_Dev = numeric(),
    Median = numeric(),
    IQR = numeric(),
    stringsAsFactors = FALSE
  )
  unique_groups <- unique(dataframe[[my_groups]])
  for (group in unique_groups) {
    group_data <- dataframe[dataframe[[my_groups]] == group, ]
    for (col in columns) {
      col_data <- group_data[[col]]
      n <- sum(!is.na(col_data))
      mean_val <- mean(col_data, na.rm = TRUE)
      std_dev <- sd(col_data, na.rm = TRUE)
      median_val <- median(col_data, na.rm = TRUE)
      iqr <- IQR(col_data, na.rm = TRUE)
      result_dataframe <- rbind(result_dataframe,
                                data.frame(Group = group,
                                           Column = col, N = n, Mean = mean_val,
                                           Std_Dev = std_dev, Median = median_val,
                                           IQR = iqr, stringsAsFactors = FALSE))
    }
  }
  return(result_dataframe)
}

sessions <- group_function(cleaned_data,"levels_var","Number_of_sessions")
print(sessions)

frequency <- group_function(cleaned_data,"levels_var","frequency_weeks")
print(frequency)

Length_sessions <- group_function(cleaned_data,"levels_var","Length_of_sessions_mins")
print(Length_sessions)

Total_hours <- group_function(cleaned_data,"levels_var","Total_hours_of_intervention_hours")
print(Total_hours)

Total_weeks <- group_function(cleaned_data,"levels_var","Total_period_of_intervention_weeks")
print(Total_weeks)

control_variables <- rbind(sessions, frequency, Length_sessions, Total_hours, Total_weeks)
write.csv(control_variables, "control_variables.csv")
