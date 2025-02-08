## Variogram map
# Load necessary libraries
library(gstat)
library(sp)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Step 1: Generate spatial data (Simulate a spatial process)
n <- 200  # Number of points
x <- runif(n, 0, 100)  # Random x-coordinates
y <- runif(n, 0, 100)  # Random y-coordinates

# Generate spatially autocorrelated data using a Gaussian process
coordinates <- cbind(x, y)
dist_matrix <- as.matrix(dist(coordinates))
cov_matrix <- exp(-dist_matrix / 20)  # Exponential covariance function
z <- chol(cov_matrix) %*% rnorm(n)  # Simulated spatial values

# Create a SpatialPointsDataFrame
spatial_data <- data.frame(x = x, y = y, z = z)
coordinates(spatial_data) <- ~x + y

# Step 2: Compute the variogram map
var_map <- variogram(z ~ 1, spatial_data, width = 5, cutoff = 50, map = TRUE)

# Step 3: Plot the variogram map
plot(var_map, main = "Variogram Map", col.regions = terrain.colors(100))
