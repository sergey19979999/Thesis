# Load necessary libraries
library(dplyr)
library(tidyr)

# Read data from the file
data <- read.csv("ESS11/ESS11_sarsic_NA.csv")

# Create the new 'Age' variable from 'yrbrn'
data$Age <- 2023 - data$yrbrn

# Convert 'Age' into a categorical variable based on the defined age groups
data$Age <- cut(data$Age,
                breaks = c(15, 30, 40, 50, 60, 70, Inf),
                labels = c("15-30", "31-40", "41-50", "51-60", "61-70", "older than 70"), # nolint
                right = FALSE)

# Remove the 'yrbrn' variable
data$yrbrn <- NULL

data$Age <- case_when(
  data$Age == "15-30" ~ 1,
  data$Age == "31-40" ~ 2,
  data$Age == "41-50" ~ 3,
  data$Age == "51-60" ~ 4,
  data$Age == "61-70" ~ 5,
  data$Age == "older than 70" ~ 6,
)

data$vteurmmb <- case_when(
  data$vteurmmb == 1 ~ 1,
  data$vteurmmb %in% c(33,44,55) ~ 2,
  data$vteurmmb == 2 ~ 3,
)

data$mbtru <- case_when(
  data$mbtru %in% c(1,2) ~ 1,
  data$mbtru == 3 ~ 2,
)

data$domicil <- case_when(
  data$domicil == 1 ~ 5,
  data$domicil == 2 ~ 4,
  data$domicil == 3 ~ 3,
  data$domicil == 4 ~ 2,
  data$domicil == 5 ~ 1,
)

data$emplrel <- case_when(
  data$emplrel %in% c(2,3) ~ 2,
  data$emplrel == 1 ~ 1,
)
#merge level for educational level
data$edlvfit <- as.numeric(as.character(data$edlvfit))
# Now apply the transformation with case_when
data$edlvfit <- case_when(
  data$edlvfit <= 2 ~ data$edlvfit ,
  data$edlvfit %in% 3:6 ~ 2,
  data$edlvfit %in% 7:11 ~ 3,
  data$edlvfit %in% 12:16 ~ 4,
  data$edlvfit %in% 17:20 ~ 5,
  data$edlvfit == 21 ~ 6
)

adjust_ordinal <- function(column) {
  # Get unique values and sort them
  unique_vals <- sort(unique(column))
  # Create a mapping from original values to new values (1:length(unique_vals))
  val_mapping <- setNames(seq_along(unique_vals), unique_vals)
  # Apply mapping to the column
  return(val_mapping[as.character(column)])
}

# Apply the function to each column of the data frame
data <- data.frame(lapply(data, adjust_ordinal))

# Save the reordered dataset back to the same file
write.csv(data, "ESS11/ESS11_sarsic_prepro.csv", row.names = FALSE)
