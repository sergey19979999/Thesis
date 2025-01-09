library(ordinalClust)

set.seed(1)

krow <- arg_kr
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

# Set parameters for bosclust
nbsem <- 150
nbsemburn <- 100
nbindmini <- 1
init <- "random"
levels <- c(2,3,4,5,6,7)
kcol <- c(2,1,2,2,5,2,3)
indexes <- c(1,9,10,14,21,45)

browser()

time_taken <- system.time({
  object <- bosclust(x = data_matrix, kr = kr, m = levels, 
                      idx_list = indexes, nbSEM = nbsem,
                      nbSEMburn = nbsemburn, nbindmini = nbindmini,
                      init = init)
})
file_name_object <- sprintf("Results/bosclust/object%d.rds", kr)
saveRDS(object, file = file_name_object)
file_name_time <- sprintf("Results/bosclust/time_taken%d.rds", kr)
saveRDS(time_taken, file = file_name_time)

