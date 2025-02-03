
library(ordinalLBM)

data <- read.csv("ESS11/ESS11_ita_prepro.csv")
data <- data[, -((ncol(data) - 2):ncol(data))]
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

if(any(is.na(levels_count))) {
  cat("There are columns with unsupported data types for counting levels.\n")
}
data[is.na(data)] <- 0
levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)
levels_info <- levels_info[!is.na(levels_info$levels), ]
ordered_levels_info <- levels_info[order(levels_info$levels),]
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)
browser()
data_matrix_sixlevels <- data_matrix[, 19:41]
set.seed(1)

best_model <- olbm(Y = data_matrix_sixlevels, Q = 11, L = 8, init = "random", eps = 1e-02, it_max = 500)

plot_olbm_custom_grouped <- function(x) {
  res <- x
  paletta <- RColorBrewer::brewer.pal(12, name = "Paired")
  palette(paletta)
  Rclus <- res$estR
  Cclus <- res$estC
  Y <- res$Y
  M <- nrow(Y)
  P <- ncol(Y)
  K <- max(Y)
  Q <- max(Rclus)
  L <- max(Cclus)
  mu <- res$mu
  s2 <- res$Sigma
  delta <- res$delta
  rho <- res$rho
  Pi <- res$Pi

  for (l in 1:L) {
    filename <- sprintf("IMAGES/cluster_col_%d.png", l)  # Define the filename for each image
    png(filename, width = 1200, height = 800)  # Open the PNG device

    # Calculate grid dimensions, assume preference for more horizontal than vertical if Q is even, else more vertical
    grid_cols <- ifelse(Q <= 6, Q, ceiling(Q / 2))
    grid_rows <- ceiling(Q / grid_cols)

    op <- par(mfrow = c(grid_rows, grid_cols), mar = c(2, 2, 2, 2), oma = c(0, 1, 1, 1), bty = "n")
    
    for (q in 1:Q) {
      selection <- Y[Rclus == q, Cclus == l]
      selection <- selection[selection != 0]
      hist(selection, probability = TRUE, main = "", col = (q + (l-1)*Q), 
           ylim = c(0, 1.2), breaks = seq(0.5, (K + 0.5), by = 1))
      if (sum(selection) != 0) {
        griglia <- seq(from = 0, to = (K + 1), by = 0.01)
        valori <- dnorm(griglia, mean = mu[q, l], sd = sqrt(s2[q, l]))
        points(griglia, valori, type = "l", col = "darkgrey", lwd = 2)
      }
      mtext(bquote(rho[.(as.character(q))] * delta[.(as.character(l))] == .(round(100 * rho[q] * delta[l], digits = 2)) ~ '%' * "    " * pi[.(paste(q, l, sep = ""))] == .(round(100 * Pi[q, l], digits = 2)) ~ '%'), cex = 0.65)
    }

    if (Q %% grid_cols != 0) {  # Check if there's an empty cell to be filled
      for (empty in 1:(grid_cols * grid_rows - Q)) {
        plot.new()
        par(mar = c(0, 0, 0, 0))
      }
    }

    par(op)  # Reset to previous parameters
    dev.off()  # Close the PNG device
  }
}
# Assuming `best_model` is already computed
plot_olbm_custom_grouped(best_model)
