library(cluster)
library(smacof)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)  # To get a palette of colors

# Read CSV file
data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Transform binary variables (columns 1-8) from 1,2 to 0,1 and ensure they are numeric
data[, 43:50] <- lapply(data[, 43:50], function(x) as.numeric(as.character(x)) - 1)

# Ensure ordinal variables are treated as such
data[c(2:43,51:63)] <- lapply(data[c(2:43,51:63)], as.ordered)
data <- data[!is.na(data$lrscale),]
# Exclude variables from the Gower distance calculation
distance_data <- data[, !(names(data) %in% c("lrscale", "prtclfit", "prtvteit"))]
browser()
gower_distance <- daisy(distance_data, metric = "gower", type = list(symm = 40:47, ord = c(1:39,41:60)))

# Calculation of two-dimensional Multidimensional Scaling with smacof
mds_result <- mds(gower_distance, ndim = 10)
coordinates <- as.data.frame(mds_result$conf)

coordinates$lrscale <- as.factor(data$lrscale)  # Use lrscale for coloring

# Generate a color palette
colors <- RColorBrewer::brewer.pal(11, "Set3")  # Assuming lrscale has 9 unique values

for (k in 1:10) {
  plot_list <- list()
  for (i in 1:10) {
    if (k != i) {  # Ensure that we don't plot a dimension against itself
      p <- ggplot(data = coordinates, aes_string(x = colnames(coordinates)[k], y = colnames(coordinates)[i], color = "lrscale")) +
        geom_point() +
        scale_color_manual(values = colors) +
        labs(x = paste("Dimension", k), y = paste("Dimension", i), color = "Lrscale") +
        theme_minimal() +
        theme(legend.position = "right")
      plot_list[[i]] <- p
    }
  }
  grid_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 2, nrow = 5))
  file_name <- sprintf("Images/multidimensionalscaling_dimension%d.png", k)
  ggsave(file_name, grid_plots, width = 10, height = 20, dpi = 300, units = "in", limitsize = FALSE)
}
