# Load necessary library
library(dplyr)

# Read the main dataset
data <- read.csv("ESS11/ESS11.csv")

# Filter rows where 'cntry' equals 'IT'
filtered_data <- filter(data, cntry == "IT")

# Read the columns.csv which contains the names of the columns to retain
columns_to_keep <- read.csv("ESS11/column.csv")

# Select the columns specified in columns.csv from the filtered dataframe
final_data <- select(filtered_data, one_of(columns_to_keep$column))

# Print the final dataframe to check
# Save the final filtered and selected dataframe to a new CSV file
write.csv(final_data, "ESS11/ESS11_ita.csv", row.names = FALSE)
