# Load required packages
library(ComplexHeatmap)
library(circlize)

# Ensure the IMAGES folder exists
if (!dir.exists("IMAGES")) {
  dir.create("IMAGES")
}

# Extract data and cluster membership from the model
data_matrix <- best_model$Y
row_clusters <- best_model$estR
col_clusters <- best_model$estC

# Reorder rows and columns based on cluster membership
ordered_rows <- order(row_clusters)
ordered_cols <- order(col_clusters)
data_matrix_ordered <- data_matrix[ordered_rows, ordered_cols]

# Define color scale (white to red)
col_fun <- colorRamp2(c(1, 6), c("white", "red"))

# Create row and column annotations for cluster separation
row_split <- factor(row_clusters[ordered_rows])  # Row cluster factor
col_split <- factor(col_clusters[ordered_cols])  # Column cluster factor

# Create the heatmap
heatmap <- Heatmap(
  data_matrix_ordered,
  name = "Heatmap",
  col = col_fun,                      # Color scale
  cluster_rows = FALSE,               # No dendrogram for rows
  cluster_columns = FALSE,            # No dendrogram for columns
  show_row_names = FALSE,             # Hide row names
  show_column_names = FALSE,          # Hide column names
  row_split = row_split,              # Separate rows by cluster
  column_split = col_split,           # Separate columns by cluster
  gap = unit(2, "mm"),                # Add gap between clusters
  border = TRUE                       # Add borders around blocks
)

# Save the heatmap to a PNG file
png(filename = "IMAGES/heatmap_ordinaLBM.png", width = 800, height = 800)
draw(heatmap)
dev.off()

