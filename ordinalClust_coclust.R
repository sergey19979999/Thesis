library(ordinalClust)
library(doParallel)
library(foreach)
library(iterators)

set.seed(1)

# Load and preprocess the data
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

if(any(is.na(levels_count))) {
  cat("There are columns with unsupported data types for counting levels.\n")
}

levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)
levels_info <- levels_info[!is.na(levels_info$levels), ]
ordered_levels_info <- levels_info[order(levels_info$levels),]
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)

# Assuming data_matrix is your existing matrix and has been ordered as needed
for (i in 50:60) {  # Looping through columns 50 to 60
    data_matrix[, i] <- factor(apply(data_matrix[, i, drop = FALSE], 1, function(x) {
        if (x %in% c(1, 2)) {
            return(1)  # Merging levels 1 and 2
        } else if (x %in% c(3, 4)) {
            return(2)  # Merging levels 3 and 4
        } else if (x %in% c(5, 6, 7)) {
            return(x - 2)  # Levels 5, 6, 7 become 3, 4, 5 respectively
        } else if (x %in% c(8, 9)) {
            return(6)  # Merging levels 8 and 9
        } else if (x %in% c(10, 11)) {
            return(7)  # Merging levels 10 and 11
        } else {
            return(NA)  # In case of unexpected values
        }
    }), levels = 1:7, labels = 1:7)  # Using numeric labels directly
}

no_cores <- detectCores() - 1  # using one less than the total number of cores
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# Ensure the cluster workers have access to required libraries and objects
clusterEvalQ(cl, {
  library(ordinalClust)  # Load the required library within each worker
})
clusterExport(cl, varlist = c("data_matrix", "levels", "indexes", "nbsem", "nbsemburn", "nbindmini", "init"))

# Set parameters for bosclust
nbsem <- 150
nbsemburn <- 100
nbindmini <- 1
init <- "random"
levels <- c(2,3,4,5,6,7)
indexes <- c(1,9,10,14,21,45)
row_cluster <- 3:12
column_cluster <- list(c(2,1,1,2,2,2), c(3,1,1,2,2,2),
                    c(2,1,1,2,3,2), c(2,1,1,2,2,3), c(3,1,1,2,3,2),
                    c(3,1,1,2,2,3), c(2,1,1,2,3,3),
                    c(3,1,1,2,3,3), c(2,1,1,2,4,2), c(3,1,1,2,4,2),
                    c(3,1,1,2,4,3), c(2,1,1,2,4,3))

icl_matrix <- matrix(NA, nrow = length(row_cluster), ncol = length(column_cluster))
time_matrix <- matrix(NA, nrow = length(row_cluster), ncol = length(column_cluster))

results <- foreach(kr = row_cluster, .combine = 'cbind') %:% 
  foreach(kc = iter(column_cluster, by='row'), .combine = 'rbind') %dopar% {
    system.time({
      object <- boscoclust(x = data_matrix, kr = kr, kc = kc, m = levels, 
                           idx_list = indexes, nbSEM = nbsem, 
                           nbSEMburn = nbsemburn, nbindmini = nbindmini,
                           init = init)
    })
    list(icl = object$icl, time = proc.time()[3])  # Collect only the elapsed time
  }

stopCluster(cl)

# Processing the results matrix to fill the pre-allocated matrices
for (i in 1:length(row_cluster)) {
  for (j in 1:length(column_cluster)) {
    icl_matrix[i, j] <- results[[i, j]]$icl
    time_matrix[i, j] <- results[[i, j]]$time
  }
}

# Find the best model based on ICL
best_icl <- which(icl_matrix == max(icl_matrix, na.rm = TRUE), arr.ind = TRUE)
best_k <- row_cluster[best_icl[1]]
best_cc <- column_cluster[[best_icl[2]]]

best_model <- boscoclust(x = data_matrix, kr = best_k, 
                    kc = best_cc, m = levels, 
                    idx_list = indexes, nbSEM = nbsem,
                    nbSEMburn = nbsemburn, nbindmini = nbindmini,
                    init = init)

# Save the best model, time list, and ICL matrix
saveRDS(best_model, file = sprintf("Results/boscoclust/best_model_%d_%d.rds", best_k, best_cc))
saveRDS(time_matrix, file = "Results/boscoclust/time_list.rds")
saveRDS(icl_matrix, file = "Results/boscoclust/icl_matrix.rds")
