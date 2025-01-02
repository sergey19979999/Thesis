# Load necessary library
library(dplyr)

# Read the data from CSV file
data <- read.csv("ESS11/ESS11_ita_NA.csv")

# Total number of elements in the dataset
total_elements <- prod(dim(data))

# Calculate total number of missing values
total_missing_values <- sum(is.na(data))

# Calculate total percentage of missing values
total_percentage_missing <- (total_missing_values / total_elements) * 100

# Print total percentage of missing values
print(paste("Total percentage of missing values:", total_percentage_missing))

# Calculate the percentage of rows with at least one missing value
rows_with_na <- sum(apply(data, 1, function(x) any(is.na(x))))
percentage_rows_with_na <- (rows_with_na / nrow(data)) * 100

# Print percentage of rows with at least one missing value
print(paste("Percentage of rows with at least one missing value:", percentage_rows_with_na))

# Calculate percentage of missing values for each variable
variable_na_percentage <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)

# Create a data frame for the variable percentages
na_percentage_table <- data.frame(Variable = names(variable_na_percentage), 
                                  Percentage = variable_na_percentage)

# Order the table by percentage of missing values
na_percentage_table <- na_percentage_table %>%
  arrange(desc(Percentage))

# Print the table
print("Table of variables by percentage of missing values:")
print(na_percentage_table)
