# Load necessary libraries
library(cluster)  # For clustering algorithms and indices
library(mclust)  # For extracting clustering results

# Read the original dataset
data <- read.csv("ESS11/ESS11_ita_prepro.csv")
levels_count <- sapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    length(unique(na.omit(x)))
  } else if(is.numeric(x)) {
    length(unique(na.omit(x)))
  } else {
    NA  # Handle other unexpected data types gracefully
  }
})

if(any(is.na(levels_count))) {
  cat("There are columns with unsupported data types for counting levels.\n")
}

levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)
levels_info <- levels_info[!is.na(levels_info$levels), ]
ordered_levels_info <- levels_info[order(levels_info$levels),]
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)
best_model <- readRDS("Results/bosclust/object12.rds")
# actually there is not need in reordering data since ARI is invariant under data reorder
best_model_membership <- best_model@zr
browser()
# Function to compute the adjusted Rand Index, removing NAs for each comparison
compute_ARI <- function(cluster_from_model, comparison_var) {
  # Removing NA entries from the comparison variable
  valid_data_indices <- !is.na(data_matrix[,comparison_var])
  
  # Subsetting both the cluster assignment and the comparison variable
  valid_clusters <- cluster_from_model[valid_data_indices]
  valid_comparison <- data[valid_data_indices,comparison_var]
  
  # Compute the Adjusted Rand Index
  ari <- adjustedRandIndex(valid_clusters, valid_comparison)
  return(ari)
}

# Compute Adjusted Rand Index for each additional variable
ari_lrscale <- compute_ARI(best_model_membership, "lrscale")
browser()
ari_prtvteit <- compute_ARI(best_model_membership, "prtvteit")
ari_prtclfit <- compute_ARI(best_model_membership, "prtclfit")

# Print results
print(list(lrscale = ari_lrscale, prtvteit = ari_prtvteit, prtclfit = ari_prtclfit))
