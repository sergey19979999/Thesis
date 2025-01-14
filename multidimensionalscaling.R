library(cluster)
library(smacof)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)

# Read CSV file
data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Transform binary variables (columns 43-50) from 1,2 to 0,1 and ensure they are numeric
data[, 43:50] <- lapply(data[, 43:50], function(x) as.numeric(as.character(x)) - 1)

# Ensure ordinal variables are treated as such
data[c(2:43, 51:63)] <- lapply(data[c(2:43, 51:63)], as.ordered)
data <- data[!is.na(data$lrscale),]
distance_data <- data[, !(names(data) %in% c("lrscale", "prtclfit", "prtvteit"))]
gower_distance <- daisy(distance_data, metric = "gower", type = list(symm = 40:47, ord = c(1:39, 41:60)))

mds_result <- mds(gower_distance, ndim = 5)
coordinates <- as.data.frame(mds_result$conf)

# Group lrscale into three categories
coordinates$lrscale_grouped <- factor(data$lrscale,
    levels = c(1:11),
    labels = c("Sx", "Sx", "Sx", "Sx", "Center", "Center", "Center", "Dx", "Dx", "Dx", "Dx")
)

# Create a color palette for the grouped levels
grouped_colors <- c("Dx" = "blue", "Sx" = "red", "Center" = "green")

for (k in 1:5) {
  plot_list <- list()
  for (i in 1:5) {
    if (k != i) {
      p <- ggplot(data = coordinates, aes_string(x = colnames(coordinates)[k], y = colnames(coordinates)[i], color = "lrscale_grouped")) +
        geom_point() +
        scale_color_manual(values = grouped_colors) +
        labs(x = paste("Dimension", k), y = paste("Dimension", i), color = "Lrscale Group") +
        theme_minimal() +
        theme(legend.position = "right")
      plot_list[[i]] <- p
    }
  }
  grid_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1, nrow = 5))
  file_name <- sprintf("Images/multidimensionalscaling_dimension%d.png", k)
  ggsave(file_name, grid_plots, width = 10, height = 20, dpi = 300, units = "in", limitsize = FALSE)
}
