library(ordinalLBM)
library(reshape2)
library(doParallel)
library(foreach)

set.seed(1)

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

data_matrix_sixlevels <- data_matrix[, 19:41]

krow <- 3:12
column_cluster <- 3:10

icl_matrix <- matrix(NA, nrow = length(krow), ncol = length(column_cluster))
time_matrix <- matrix(NA, nrow = length(krow), ncol = length(column_cluster))

for (k in krow) {
  for (cc in column_cluster) {
    result <- tryCatch({
      time_taken <- system.time({
        object <- olbm(Y = data_matrix_sixlevels, Q = k, L = cc, init = "random",
        eps = 1e-02, it_max = 500, verbose = FALSE)
      })
      # If successful, return a list containing the icl value and time taken
      list(icl = object$icl, time = time_taken["user.self"]+
            time_taken["sys.self"])
    }, error = function(e) {
      # If an error occurs, print the error message and return -Inf for icl and Inf for time
      message("Error in olbm: ", e$message)
      list(icl = -Inf, time = Inf)
    })
    print(c(k,cc,result$icl,result$time))
    # Store the results appropriately
    time_matrix[k-2,cc-2]<- result$time
    icl_matrix[k-2,cc-2] <- result$icl
  }
}
# Find the best model based on ICL
best_icl <- which(icl_matrix == max(icl_matrix, na.rm = TRUE), arr.ind = TRUE)
best_k <- krow[best_icl[1]]
best_cc <- column_cluster[best_icl[2]]
browser()
# Rerun the best model to save it
set.seed(1)
pritn(c(k,cc))
best_model <- olbm(Y = data_matrix_sixlevels, Q = best_k, L = best_cc, init = "random", eps = 1e-02, it_max = 500, verbose = FALSE)

# Save the best model, time list, and ICL matrix
saveRDS(best_model, file = sprintf("Results/olbmclust/best_model_%d_%d.rds",best_k, best_cc))
saveRDS(time_list, file = "Results/olbmclust/time_list.rds")
saveRDS(icl_matrix, file = "Results/olbmclust/icl_matrix.rds")


