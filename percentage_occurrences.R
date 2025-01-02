# Load necessary libraries
library(dplyr)
library(tidyr)
library(gridExtra) # for grid.table()

# Read data from CSV
data <- read.csv("ESS11/ESS11_ita_NA.csv")


# Select relevant variables
selected_data <- data %>% select(ppltrst, trstlgl, trstplc, trstplt, trstep, trstun, euftf, atchctr, atcherp, ccrdprs)

# Function to calculate percentage occurrences of each value and pivot to wide format
calculate_percentages_and_pivot <- function(data_frame) {
  results <- lapply(data_frame, function(x) {
    freq_table <- table(x, useNA = "always")
    percent_table <- prop.table(freq_table) * 100
    return(as.data.frame(percent_table))
  })
  
  # Convert list to data frame and pivot to wide format
  long_data <- do.call(rbind, lapply(names(results), function(name) {
    df <- data.frame(variable = name, results[[name]])
    colnames(df)[2:3] <- c("value", "percentage")
    return(df)
  }))
  
  # Pivot wider
  wide_data <- pivot_wider(long_data, names_from = value, values_from = percentage)
  
  return(wide_data)
}

# Applying the function to calculate percentages and format as a wide table
percentages_wide <- calculate_percentages_and_pivot(selected_data)

# Print the results in the console
print(percentages_wide)

# Save the table as PNG
png(filename = "Images/percentages_table_11.png", width = 900, height = 600)
grid.table(percentages_wide)
dev.off()

