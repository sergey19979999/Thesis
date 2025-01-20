library(ggplot2)
library(gridExtra)

best_model <- readRDS("Results/bosclust/object12.rds")
# Retrieve mu and pi matrices for a specific variable
data <- read.csv("ESS11/ESS11_ita_prepro.csv")
data <- subset(data, select = -c(prtvteit, prtclfit, lrscale))
data[1:60] <- lapply(data[1:60], function(x) if(is.character(x)) factor(x, ordered = TRUE) else x)

levels_count <- sapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    length(unique(na.omit(x)))
  } else if(is.numeric(x)) {
    length(unique(na.omit(x)))
  } else {
    NA  # Handle other unexpected data types gracefully
  }
})


levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)
levels_info <- levels_info[!is.na(levels_info$levels), ]
ordered_levels_info <- levels_info[order(levels_info$levels),]
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)

browser()
mu_matrix <- best_model@params[[block_number]]$mus  # Access the block_numberth matrix in the mus list
pi_matrix <- best_model@params[[block_number]]$pis  # Access the block_numberth matrix in the pis list

# Retrieve the specific column (variable) from each matrix
mu_data <- mu_matrix[, variable_number]  # variable_numberth column for mu
pi_data <- pi_matrix[, variable_number]  # variable_numberth column for pi

level_block <- c(2,3,4,5,6,7)
# Create a data frame for plotting
cluster_numbers <- 1:nrow(mu_matrix)  # Assuming cluster numbers are rows of the matrix
mu_df <- data.frame(Cluster = cluster_numbers, Estimate = mu_data, Type = "Mu")
pi_df <- data.frame(Cluster = cluster_numbers, Estimate = pi_data, Type = "Pi")

# Combine the data frames
plot_data <- rbind(mu_df, pi_df)

# Plotting using ggplot2
mu_plot <- ggplot(mu_df, aes(x = Cluster, y = Estimate, color = Type)) +
  geom_point() +
  labs(title = paste("Estimates of Mu for variable ", variable_name),
       x = "Cluster Number",
       y = "Estimate",
       color = "Parameter Type") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(1, level_block[block_number])) + # Setting specific y-axis limits for Mu
  theme(plot.background = element_rect(fill = "white", colour = "white"))

# Plot for Pi with y-axis range [0,1]
pi_plot <- ggplot(pi_df, aes(x = Cluster, y = Estimate, color = Type)) +
  geom_point() +
  labs(title = paste("Estimates of Pi for variable ", variable_name),
       x = "Cluster Number",
       y = "Estimate",
       color = "Parameter Type") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, 1)) + # Setting specific y-axis limits for Pi
  theme(plot.background = element_rect(fill = "white", colour = "white"))

# Combine the plots
combined_plot <- grid.arrange(mu_plot, pi_plot, ncol = 1)  # Change ncol to 2 for side-by-side

# Save the combined plot
ggsave(paste("Images/mu_pi_estimates_", variable_name, ".png"), plot = combined_plot, width = 10, height = 12, dpi = 300)