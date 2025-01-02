library(ordinalLBM)
set.seed(1)
library(reshape2)

data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Exclude the last two columns as specified (adjust as necessary)
data <- data[, -((ncol(data) - 2):ncol(data))]

# Convert the first 60 columns to ordered factors, ensuring that they are not character type
data[1:60] <- lapply(data[1:60], function(x) if(is.character(x)) factor(x, ordered = TRUE) else x)

# Calculate the number of unique levels per column excluding NAs and handling data types properly
levels_count <- sapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    length(unique(na.omit(x)))
  } else if(is.numeric(x)) {
    length(unique(na.omit(x)))
  } else {
    NA  # Handle other unexpected data types gracefully
  }
})


# Check if any NA in levels_count, which indicates unexpected data types
if(any(is.na(levels_count))) {
  cat("There are columns with unsupported data types for counting levels.\n")
}

data[is.na(data)] <- 0

# Create a dataframe to store levels and column names for sorting
levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)

# Ensure no NAs are in levels for sorting
levels_info <- levels_info[!is.na(levels_info$levels), ]

# Order the dataframe by the number of levels
ordered_levels_info <- levels_info[order(levels_info$levels),]

# Use the ordered column names to reorder the columns in the original dataset
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)

variables <- c("likrisk", "liklead", "sothnds", "eqwrkbg", "eqpolbg", "edlvfit", 
               "ppltrst", "trstlgl", "trstplc", "trstplt", "trstep", "trstun", 
               "euftf", "atchctr")

# Initialize an empty data frame to store the results
results <- data.frame(Variable = character(), Level = character(), Percentage = numeric(), stringsAsFactors = FALSE)

# Calculate the percentage of occurrences for each level of each variable
for (var in variables) {
  # Create a frequency table for the current variable
  freq_table <- table(data_matrix[, var])
  
  # Convert frequency to percentage
  percentage_table <- prop.table(freq_table) * 100
  
  # Create a temporary data frame
  temp_df <- data.frame(Level = names(percentage_table), 
                        Percentage = as.numeric(percentage_table),
                        Variable = rep(var, length(percentage_table)))
  
  # Combine the temporary data frame with the main results data frame
  results <- rbind(results, temp_df)
}

# Pivot the data to a wide format
library(tidyr)
final_table <- pivot_wider(results, names_from = Level, values_from = Percentage, names_prefix = "Level_")

# Reorder columns (optional)
final_table <- final_table[, c("Variable", sort(setdiff(names(final_table), "Variable")))]


browser()
row_cluster <- 10
column_cluster <- 5

data_matrix_sixlvels <- data_matrix[,21:43]
test_results <- olbm(Y = data_matrix_sixlvels, Q = row_cluster, L= column_cluster, init = "kmeans", eps = 1e-04, it_max = 500,
      verbose = TRUE)

browser()