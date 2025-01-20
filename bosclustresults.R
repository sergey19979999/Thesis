# Load required libraries
library(ggplot2)
library(ordinalClust)

# Set the working directory to the folder containing the data files
setwd("Results/bosclust")

# Prepare to collect data for plotting
cpu_times <- data.frame(Model = integer(), Time = numeric())
icl_values <- data.frame(Model = integer(), ICL = numeric())

# Loop over the range of models
for (n in 3:12) {
  # Construct file names
  time_file <- paste0("time_taken", n, ".rds")
  object_file <- paste0("object", n, ".rds")
  
  # Read the system.time() object
  time_data <- readRDS(time_file)
  
  # Extract user time and system time, sum them for total CPU time
  total_cpu_time <- time_data["user.self"] + time_data["sys.self"]
  
  # Append to the cpu_times data frame
  cpu_times <- rbind(cpu_times, data.frame(Model = n, Time = total_cpu_time))
  
  # Read the bosclust object
  object_data <- readRDS(object_file)
  
  # Extract ICL value
  icl_value <- object_data@icl
  
  # Append to the icl_values data frame
  icl_values <- rbind(icl_values, data.frame(Model = n, ICL = icl_value))
}

# Plotting CPU times
cpu_plot <- ggplot(cpu_times, aes(x = Model, y = Time)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 3:12) + # Ensuring all numbers between 3 and 12 are shown
  labs(title = "CPU Time by Model", x = "Row clusters number", y = "Total CPU Time") +
  theme_classic()

# Save CPU time plot
ggsave("cpu_times_plot.png", cpu_plot, width = 10, height = 6)

# Plotting ICL values with improved x-axis labels and scales
icl_plot <- ggplot(icl_values, aes(x = Model, y = ICL)) +
  geom_line(color = "blue") + geom_point(color = "red") +
  scale_x_continuous(breaks = 3:12) + # Ensuring all numbers between 3 and 12 are shown
  labs(title = "ICL by Model", x = "Row clusters number", y = "ICL Value") +
  theme_classic()

# Save ICL plot
ggsave("icl_values_plot.png", icl_plot, width = 10, height = 6)

# Output message
print("Plots are saved in the boclust directory.")
