library(ordinalClust)
set.seed(1)

data <- read.csv("ESS11/ESS11_ita_prepro.csv")

# Exclude columns prtclfit, prtvteit, lrscale
data <- subset(data, select = -c(prtvteit, prtclfit,lrscale))
# Convert columns to ordered factors, ensuring that they are not character type
data[1:40] <- lapply(data[1:40], function(x) if(is.character(x)) factor(x, ordered = TRUE) else x)

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

m_2<- c(6,11)
kr_2<- 10
kc_2 <- c(5,2)
d.list_2 <- c(1,30)
browser()

data_matrix_onelevel <- data_matrix[1:100,1:29]

# object <- boscoclust(x = data_matrix_onelevel,kr = 10, kc = 5, m = 6,
#                 nbSEM = nbsem,
#                 nbSEMburn = nbsemburn, nbindmini = nbindmini,
#                 init = init)
object_twolevels <- boscoclust(x = data_matrix,kr = kr_2, kc = kc_2, m = m_2,
                idx_list = d.list_2, nbSEM = nbsem,
                nbSEMburn = nbsemburn, nbindmini = nbindmini,
                init = init)
