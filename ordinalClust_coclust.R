library(ordinalClust)

set.seed(1)

data <- read.csv("ESS11/ESS11_ita_prepro.csv")
data <- subset(data, select = -c(prtvteit, prtclfit, lrscale))
data[1:60] <- lapply(data[1:60], function(x) if(is.character(x)) factor(x, ordered = TRUE) else x)

levels_count <- sapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    length(unique(na.omit(x)))
  } else if(is.numeric(x)) {
    length(unique(na.omit(x)))
  } else {
    NA
  }
})

levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)
levels_info <- levels_info[!is.na(levels_info$levels), ]
ordered_levels_info <- levels_info[order(levels_info$levels),]
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)

for (i in 50:60) {
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
            return(NA)
        }
    }), levels = 1:7, labels = 1:7)  # Using numeric labels directly
}

# Set parameters for boscoclust
nbsem <- 25
nbsemburn <- 15
nbindmini <- 1
init <- "random"
levels <- c(2,3,4,5,6,7,7)
indexes <- c(1,9,10,14,21,45,50)

browser()
row_cluster <- 8:12
column_cluster <- list(
                    # c(2,1,1,2,2,1,1), c(3,1,1,2,2,1,1),
                    # c(2,1,1,2,3,1,1), c(2,1,1,2,2,1,2), c(3,1,1,2,3,1,1),
                    # c(3,1,1,2,2,1,2), c(2,1,1,2,3,1,2),
                    c(3,1,1,2,3,1,2), c(2,1,1,2,4,1,1), c(3,1,1,2,4,1,1),
                    c(3,1,1,2,4,1,2), c(2,1,1,2,4,1,2))

icl_matrix <- matrix(NA, nrow = length(row_cluster), ncol = length(column_cluster))
set.seed(1)
browser()
for (kr in row_cluster) {
    col_index <- 1
    for(cc in column_cluster){
        print(sprintf("Model: row_clusters = %d, column_clusters = %s", kr, toString(cc)))
        object <- boscoclust(x = data_matrix, kr = kr, kc = cc, m = levels, 
                                idx_list = indexes, nbSEM = nbsem, 
                                nbSEMburn = nbsemburn, nbindmini = nbindmini,
                                init = init)
        # Store the results appropriately
        if (length(object@icl) > 0){
        icl_matrix[kr-7,col_index] <- object@icl
        browser()
        }
        else {
           icl_matrix[kr-7,col_index] <- -Inf
        }
        col_index <- col_index + 1
        print(icl_matrix)
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
browser()
# Save the best model, time list, and ICL matrix
saveRDS(best_model, file = sprintf("Results/boscoclust/best_model.rds", best_k, best_cc))
saveRDS(icl_matrix, file = "Results/boscoclust/icl_matrix.rds")
