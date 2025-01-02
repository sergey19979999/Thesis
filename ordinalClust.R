library(ordinalClust)
set.seed(1)

data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Exclude columns prtclfit, prtvteit, lrscale
data <- subset(data, select = -c(prtvteit, prtclfit,lrscale))
# Convert columns to ordered factors, ensuring that they are not character type
data[1:60] <- lapply(data[1:60], function(x) if(is.character(x)) factor(x, ordered = TRUE) else x)

# Calculate the number of unique levels per column excluding NAs and handling data types properly
levels_count <- sapply(data, function(x) {
  if(is.factor(x) || is.character(x)) {
    length(unique(na.omit(x)))
  } else if(is.numeric(x)) {
    length(unique(na.omit(x)))
  } else {
    NA  # Handle other unexpected data types gracefully
  }
})

# Check if any NA in levels_count, which indicates unexpected data types
if(any(is.na(levels_count))) {
  cat("There are columns with unsupported data types for counting levels.\n")
}

# Create a dataframe to store levels and column names for sorting
levels_info <- data.frame(column_name = names(levels_count), levels = levels_count, stringsAsFactors = FALSE)
# Ensure no NAs are in levels for sorting
levels_info <- levels_info[!is.na(levels_info$levels), ]
# Order the dataframe by the number of levels
ordered_levels_info <- levels_info[order(levels_info$levels),]
# Use the ordered column names to reorder the columns in the original dataset
data_ordered <- data[ordered_levels_info$column_name]
data_matrix <- as.matrix(data_ordered)

nbsem <- 150
nbsemburn <- 100
nbindmini <- 1
init <- "random"

print(ordered_levels_info)

levels <- c(2,3,4,5,6,7,11)
krow <- 10
kcol <- c(5,1,2,2,5,2,3)
indexes <- c(1,9,10,14,21,45,50)
browser()



object_twolevels <- boscoclust(x = data_matrix,kr = krow, kc = kcol, m = levels,
                idx_list = indexes, nbSEM = nbsem,
                nbSEMburn = nbsemburn, nbindmini = nbindmini,
                init = init)
