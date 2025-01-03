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

# Include the 'prtvteit' variable for coloring
data$prtvteit <- as.factor(data$prtvteit)

# Calculation of Gower distance matrix
gower_distance <- daisy(data, metric = "gower", type = list(symm = 43:50, ord = 9:60))

# Calculation of two-dimensional Multidimensional Scaling with smacof
mds_result <- mds(gower_distance, ndim = 5)
coordinates <- as.data.frame(mds_result$conf)
coordinates$prtvteit <- data$prtvteit  # Add 'prtvteit' to coordinates for plotting
colnames(coordinates) <- c(paste("Dim", 1:5, sep = "."), "prtvteit")

# Generate a color palette with 13 colors (12 colors + black for NA)
colors <- c(brewer.pal(12, "Set3"), "black")  # Adjust palette name and type as needed

levels_prtvteit <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "31")
labels_prtvteit <- c("Fratelli d'Italia", "Partido Democratico (PD)", "Movimento 5 Stelle", "Lega",
                     "Forza Italia", "Terzo Polo (Azione-Italia Viva)", "Alleanza Verdi e Sinistra", 
                     "+ Europa", "Italexit", "Unione Popolare", "Italia Sovrana e Popolare", "Altro")

data$prtvteit <- factor(data$prtvteit, levels = levels_prtvteit, labels = labels_prtvteit)

for (k in 1:5) {
  plot_list <- list()
  for (i in 1:5) {
    if (k != i) {  # Ensure that we don't plot a dimension against itself
      p <- ggplot(data = coordinates, aes_string(x = colnames(coordinates)[k], y = colnames(coordinates)[i], color = "prtvteit")) +
        geom_point() +
        scale_color_manual(values = colors) +
        labs(x = paste("Dimension", k), y = paste("Dimension", i), color = "Party") +
        theme_minimal() +
        theme(legend.position = "right")
      plot_list[[i]] <- p
    }
  }
  grid_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1, nrow = 5))
  file_name <- sprintf("Images/multidimensionalscaling_dimension%d.png", k)
  ggsave(file_name, grid_plots, width = 10, height = 20, dpi = 300, units = "in", limitsize = FALSE)
}