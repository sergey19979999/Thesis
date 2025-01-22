# Assuming `best_model` and `ordered_levels_info` are already loaded in your R environment

# Function to find the group and index within the group for a given variable index
get_group_and_index <- function(variable_index) {
  group_limits <- c(0, 8, 9, 13, 20, 44, 60)  # Ends of each group interval
  group <- findInterval(variable_index, group_limits, left.open = TRUE)
  local_index <- variable_index - group_limits[group]
  return(list(group = group, index = local_index))
}

# Create an empty data frame to store the results
result_df <- data.frame(matrix(vector(), nrow = 60, ncol = 12))
# Loop over each variable in the filtered ordered_levels_info
for (j in 1:12){
for (i in 1:60) {
  # Determine which group and local index this variable corresponds to
  location_info <- get_group_and_index(i)
  group <- location_info$group
  index <- location_info$index
  
  # Assign the values to the data frame
  result_df[i, j] <- best_model@params[[group]]$mus[j, index]
}

}
# View the resulting data frame
rownames(result_df) <- ordered_levels_info[[1]][1:60]
print(result_df)
