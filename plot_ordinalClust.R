# Load necessary libraries
library(ordinalClust)  # For clustering functions
library(ggplot2)       # For plotting
library(dplyr)         # For data manipulation
library(readr)         # For reading CSV files

data_selected <- data_matrix_test_sixlevels 

# Assuming 'test' is already created using ordinalClust
# Ensure the 'test' object is loaded or created before this script runs
# Extract cluster information and add as a factor to order data by
data_selected$Cluster <- factor(test@zr)

# Order rows by cluster
data_selected <- data_selected %>% arrange(Cluster)

# Convert NA to a specific value for plotting (use 7 here, since 1 to 6 are valid entries)
data_selected[is.na(data_selected)] <- 7

# Melt the data for plotting
long_data <- reshape2::melt(data_selected, id.vars = "Cluster")

# Check for unique values in the 'value' column
unique_values <- unique(long_data$value)
print(unique_values)

# Define a color palette with enough colors for each unique value, including a specific color for NA
color_palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#A6A6A6", "#696969", "#000000")

# Check if there are more unique values than colors and adjust if necessary
if(length(unique_values) > length(color_palette)) {
  stop("Not enough colors provided for the scale. Please add more colors.")
}

# Creating the plot
p <- ggplot(long_data, aes(x = variable, y = as.numeric(as.factor(1:nrow(long_data))))) +
  geom_tile(aes(fill = factor(value)), color = "white") +
  scale_fill_manual(values = setNames(color_palette, unique_values),
                    na.value = "black") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "right")


# Save the plot if needed
ggsave("clustered_data_plot.png", plot = p, width = 12, height = 10, dpi = 300)
