# Load necessary libraries
library(dplyr)

# Read data from the CSV file
data <- read.csv("ESS11/ESS11_ita_NA.csv")
data <- subset(data, select = -c(rlgdnm, chldhhe))
# Calculate the total number of NA values and the percentage of NA
total_na <- sum(is.na(data))
total_values <- prod(dim(data))
percentage_na <- (total_na / total_values) * 100

# Print the percentage of NA values
print(paste("Percentage of NA values:", percentage_na))

# Calculate the percentage of rows with at least one NA
rows_with_na <- sum(apply(data, 1, function(x) any(is.na(x))))
percentage_rows_with_na <- (rows_with_na / nrow(data)) * 100

# Print the percentage of rows with at least one NA
print(paste("Percentage of rows with at least one NA:", percentage_rows_with_na)) # nolint

# Create a table of variables and their percentage of NA values, ordered by percentage
na_count_per_variable <- sapply(data, function(x) sum(is.na(x)))
percentage_na_per_variable <- (na_count_per_variable / nrow(data)) * 100
na_table <- data.frame(Variable = names(percentage_na_per_variable), Percentage = percentage_na_per_variable) # nolint
ordered_na_table <- na_table %>%
  arrange(desc(Percentage))

# Print the table
print(ordered_na_table)
