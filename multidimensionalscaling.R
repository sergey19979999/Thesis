library(cluster)
library(smacof)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)  # To get a palette of colors

# Leggere il file CSV
data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Escludere le ultime 3 variabili
data <- data[, 1:(ncol(data) - 3)]

# Trasformare le variabili binarie (colonne 1-8) da 1,2 a 0,1 e assicurarsi che siano numeriche
data[, 1:8] <- lapply(data[, 1:8], function(x) as.numeric(as.character(x)) - 1)

# Assicurarsi che le variabili ordinali siano trattate come tali
data[9:60] <- lapply(data[9:60], as.ordered)

# Creare un vettore dei tipi di variabili per il calcolo di Gower
variable_types <- c(rep("symm", 8), rep("ord", 52))

# Calcolo della matrice delle distanze di Gower
gower_distance <- daisy(data, metric = "gower", type = list(symm = 1:8, ord = 9:60))

# Calcolo del Multidimensional Scaling bidimensionale con smacof
mds_result <- mds(gower_distance, ndim = 5)
coordinates <- as.data.frame(mds_result$conf)
colnames(coordinates) <- paste("Dim", 1:5, sep = ".")

# Generate a color palette
colors <- brewer.pal(5, "Set1")  # Using a palette with 5 distinct colors

# Generate plots for each dimension against all others and save in separate files
for (k in 1:5) {
  plot_list <- list()
  for (i in 1:5) {
    # Create a ggplot object for the k-th dimension against the i-th dimension
    p <- ggplot(data = coordinates, aes_string(x = colnames(coordinates)[k], y = colnames(coordinates)[i])) +
      geom_point(color = colors[i]) +
      labs(x = paste("Dimension", k), y = paste("Dimension", i)) +
      theme_minimal() +
      theme(legend.position = "none")
    plot_list[[i]] <- p
  }
  grid_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 1, nrow = 5))
  # Save each grid to a separate file, adjusting dimensions to a more reasonable size
  file_name <- sprintf("multidimensionalscaling_dimension%d.png", k)
  ggsave(file_name, grid_plots, width = 10, height = 20, dpi = 300, units = "in", limitsize = FALSE)
}
