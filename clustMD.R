library(clustMD)

data <- read.csv("ESS11/ESS11_ita_prepro.csv")
data <- data[, -((ncol(data) - 2):ncol(data))]  # Adjust according to your data
data[1:56] <- lapply(data[1:56], function(x) factor(x, ordered = TRUE))
data[57:66] <- lapply(data[57:66], function(x) as.factor(x))

# Correctly convert factor variables to their integer codes
data <- data.frame(lapply(data, function(x) if(is.factor(x)) as.integer(x) else x)) # nolint

data_matrix <- as.matrix(data)
# Now convert the entire data frame to a matrix

if(any(is.na(data_matrix))) {
  print("There are NAs in the data matrix.")
}
data_matrix_test = data_matrix[1:100,c(1:8,10:15)] # nolint
browser()
# c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "BD") complete model
res <- clustMD(X = data_matrix_test, G = 3, CnsIndx = 0, OrdIndx = 14, Nnorms = 2000, # nolint
        MaxIter = 5000, model = "VVI", store.params = FALSE, scale = FALSE, # nolint
        startCL = "kmeans", autoStop = TRUE, ma.band = 30, stop.tol = 0.001)
browser()
