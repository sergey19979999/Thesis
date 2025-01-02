# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# List of variables to analyze
variables <- c("Age", "domicil", "emplrel", "region", "rlgdnm", "ccnthum", "vteurmmb", "mbtru")

# Function to calculate percentage of occurrences
calculate_percentage <- function(column) {
  column <- as.factor(column)  # Ensure the column is treated as categorical
  freq_table <- table(column) # Frequency table
  percentage_table <- prop.table(freq_table) * 100 # Convert to percentages
  return(as.data.frame(percentage_table))
}

# Loop through variables and calculate percentages
result_list <- lapply(variables, function(var) {
  if (var %in% names(data)) {
    percentage_data <- calculate_percentage(data[[var]])
    percentage_data$Variable <- var
    colnames(percentage_data) <- c("Value", "Percentage", "Variable")
    return(percentage_data)
  } else {
    return(data.frame(Value = NA, Percentage = NA, Variable = var)) # Handle missing variables # nolint
  }
})

# Combine results into a single table
final_table <- do.call(rbind, result_list)

# Print the table
print(final_table)
