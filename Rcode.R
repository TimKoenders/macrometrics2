rm(list = ls())

# Set seed for reproducibility
set.seed(123)

##question 1a 

# Define parameters
mu <- 5
sigma <- 3
n <- 100

# Generate data
x <- rnorm(n, mean = mu, sd = sigma)

# Compute estimates of the mean with the first 1, ..., n draws
estimates <- cumsum(x) / seq_along(x)

# Plot estimates
plot(estimates, type = "l", col = "blue", xlab = "Sample size", ylab = "Estimate of the mean")

# Add true mean as a horizontal line
abline(h = mu, col = "red")

##question 1b

# set seed
set.seed(123)

# Define parameters

mu <- 0
sigma <- 1
n <- 1000

# Generate two random samples from a standard normal distribution
x <- rnorm(n, mean = mu, sd = sigma)
y <- rnorm(n, mean = mu, sd = sigma)

# Convert the standard normal samples to a Cauchy distribution with scale one
z <- x / y

# Compute estimates of the mean with the first 1, ..., n draws
estimates <- cumsum(z) / seq_along(z)

# Plot estimates
plot(estimates, type = "l", col = "blue", xlab = "Sample size", ylab = "Estimate of the mean")

# Add true mean as a horizontal line
abline(h = mu, col = "red")

# Question 3

simulate_data <- function(n, k, alpha, beta, sigma) {
  X <- matrix(rnorm(n * k, mean = 0, sd = 1), n, k)
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  y <- alpha + X %*% beta + epsilon
  return(list(X = X, y = y))
}



