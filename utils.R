merge_categories <- dunction(ordinal_vars, threeshold)
{
    for (var in ordinal_vars) {
        # Calculate proportions and find low percentage categories
        cat_percents <- prop.table(table(data[[var]]))
        categories <- as.numeric(names(cat_percents))
        low_percent_cats <- categories[cat_percents < threeshold]
        print(var)
        # Continue if at least two low percentage categories are found
        if (length(low_percent_cats) < 2) {
            next
        }

        # Find all groups of categories to combine
        to_combine <- list()
        temp_group <- NULL

        for (i in seq_along(low_percent_cats)) {
            if (is.null(temp_group)) {
            temp_group <- low_percent_cats[i]
            } else {
            if (low_percent_cats[i] == max(temp_group) + 1) {
                temp_group <- c(temp_group, low_percent_cats[i])
            } else {
                to_combine[[length(to_combine) + 1]] <- temp_group
                temp_group <- low_percent_cats[i]
            }
            }
        }

        if (!is.null(temp_group)) {
            to_combine[[length(to_combine) + 1]] <- temp_group
        }

        # Perform combination for each group
        for (group in to_combine) {
            min_cat <- min(group)
            data[[var]][data[[var]] %in% group] <- min_cat
        }

        # Update factor levels after all combinations
        data[[var]] <- factor(data[[var]])


        # Relevel to create consecutive numbering
        data[[var]] <- factor(data[[var]])
        data[[var]] <- factor(data[[var]], levels = unique(data[[var]]), labels = seq_along(unique(data[[var]]))) # nolint
        }
}
