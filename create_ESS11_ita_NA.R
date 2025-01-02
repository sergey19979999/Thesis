# Load necessary libraries
library(dplyr)
library(tidyr)

# Read the main dataset
data <- read.csv("ESS11/ESS11_ita.csv")

# Read the missing values specifications
missing_values <- read.csv("ESS11/missing_values.csv")

# Function to replace specified missing codes with NA for a single variable
replace_missing_values <- function(df, variable, codes) {
  missing_codes <- strsplit(codes, "-")[[1]]  # Split the codes string into separate codes # nolint: line_length_linter.
  df[[variable]] <- as.character(df[[variable]])  # Convert the factor to character if needed # nolint
  df[[variable]][df[[variable]] %in% missing_codes] <- NA  # Replace missing codes with NA # nolint
  df[[variable]] <- as.factor(df[[variable]])  # Convert back to factor if that was the original type # nolint: line_length_linter.
  return(df)
}

# Apply the function to each variable as specified in missing_values.csv
for (i in 1:nrow(missing_values)) { # nolint
  variable_name <- missing_values$variable_name[i]
  codes <- missing_values$missing_values_codes[i]
  data <- replace_missing_values(data, variable_name, codes)
}

# Optionally, write the modified dataframe to a new CSV file
write.csv(data, "ESS11/ESS11_ita_NA.csv", row.names = FALSE)
