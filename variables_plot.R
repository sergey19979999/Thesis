library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# Load the data
data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Convert all character columns to factors to ensure consistency
data[] <- lapply(data, function(x) factor(x))

# Identify variables with exactly num-level (from environment) unique non-NA levels
numlevel_vars <- sapply(data, function(x) {
  # Levels function automatically excludes NA values in its count
  return(length(levels(x)) == num_level)
})

# Extract names of these variables
variable_names <- names(which(numlevel_vars))

# Select these variables and ensure 'lrscale' is available for coloring but not plotting
selected_data <- data %>%
  select(all_of(c(variable_names, "lrscale"))) %>%
  filter(!is.na(lrscale))  # Exclude rows where lrscale is NA

# Group and merge categories into new levels
selected_data$lrscale_grouped <- factor(selected_data$lrscale,
    levels = c(1:11),
    labels = c(
        "Sx", "Sx", "Sx", "Sx","Center",
        "Center", "Center", "Dx", "Dx", "Dx", "Dx"
    )
)

# Create a color palette for the grouped levels
grouped_colors <- c(
  "Dx" = "blue",
  "Sx" = "red",
  "Center" = "green"
)

# Prepare for plotting by excluding 'lrscale' and 'lrscale_grouped' from the plotting variables
plot_vars <- setdiff(variable_names, c("lrscale", "lrscale_grouped"))
browser()
# Generate plots
plot_list <- list()
plot_count <- 0
image_count <- 0

# Plot each variable against each other
for (i in 1:(length(plot_vars) - 1)) {
  for (j in (i + 1):length(plot_vars)) {
    p <- ggplot(selected_data, aes_string(x = plot_vars[i], y = plot_vars[j])) +
      geom_point(aes(color = lrscale_grouped), position = position_jitter(width = 0.2, height = 0.2), na.rm = TRUE, size = 5) +  # Augmented jittering
      scale_color_manual(values = grouped_colors) +
      labs(x = plot_vars[i], y = plot_vars[j]) +  # Removed color label for simplicity
      theme_minimal() +
      theme(legend.position = "none")  # Exclude legend in individual plots
    plot_list[[length(plot_list) + 1]] <- p
    plot_count <- plot_count + 1
    
    # Save every 2 plots
    if (plot_count == 2) {
      image_count <- image_count + 1
      grid_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1, nrow = 2))
      file_name <- sprintf("Images/plot_%d.png", image_count)
      ggsave(file_name, grid_plots, width = 40, height = 40, dpi = 300, units = "in")
      plot_list <- list()
      plot_count <- 0
    }
  }
}

# Handle the remaining plots if any
if (length(plot_list) > 0) {
  image_count <- image_count + 1
  grid_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1, nrow = ceiling(length(plot_list) / 2)))
  file_name <- sprintf("Images/plot_%d.png", image_count)
  ggsave(file_name, grid_plots, width = 40, height = 40, dpi = 300, units = "in")
}

# Save a legend separately for reference
legend_plot <- ggplot(selected_data, aes(x = 1, y = 1, color = lrscale_grouped)) +
  geom_point(size = 5) +
  scale_color_manual(values = grouped_colors) +
  labs(color = "Group") +
  theme_void() +
  theme(legend.position = "bottom")
ggsave("Images/legend_grouped.png", legend_plot, width = 10, height = 8, dpi = 300, units = "in")
