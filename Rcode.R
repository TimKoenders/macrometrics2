rm(list = ls())

#question 1

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

set.seed(123)

# Creating function
sim_linear_model_1 <- function(n, k, alpha, beta, sigma) {
  X <- matrix(rnorm(n * k, mean = 0, sd = 1), n, k)
  x <- X[,1]
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  y <- alpha + X %*% beta + epsilon
  return(list(x = x, y = y))
  
}

# Create scatterplot with regression line
sim_data_1 <- sim_linear_model_1(n = 100, k = 1, alpha = 2, beta = 3, sigma = 1)
plot(sim_data_1$x, sim_data_1$y, xlab = "x", ylab = "y")
abline(lm(y ~ x, data = sim_data_1), col = "red")

# Define numbers of simulations

n_sims <- 1000

# Create an empty vector to store the estimated slope coefficients
beta_lse <- numeric(n_sims)

# Loop through the simulations
for (i in 1:n_sims) {
  # Simulate the data
  sim_data_1 <- sim_linear_model_1(n = 100, k = 1, alpha = 2, beta = 3, sigma = 1)
  
  # Estimate the slope coefficient using linear regression
  fit <- lm(sim_data_1$y ~ sim_data_1$x)
  beta_lse[i] <- coef(fit)[2]
}

# Creating histogram
hist(beta_lse, main = "Histogram of Beta Estimates", xlab = "Beta Estimate")

library(MASS)  # for the mvrnorm function

# Define function to simulate data
sim_linear_model_prior <- function(n, k, alpha, beta_prior_mean, beta_prior_sd, sigma) {
  X <- matrix(rnorm(n * k, mean = 0, sd = 1), n, k)
  beta <- rnorm(k, mean = beta_prior_mean, sd = beta_prior_sd)
  epsilon <- rnorm(n, mean = 0, sd = sigma)
  y <- alpha + X %*% beta + epsilon
  return(list(X = X, y = y))
}

# Set seed for reproducibility
set.seed(123)

# Set prior parameters mu_0 and sigma_0
mu_0 <- 1
sigma_0 <- 2

# Compute posterior for n = 50, 100, and 200
for (n in c(50, 100, 200)) {
  
  # Simulate data using sim_linear_model_prior()
  k <- 1
  alpha <- 0
  beta_prior_mean <- 0
  beta_prior_sd <- 1
  sigma <- 1
  
  sim_data <- sim_linear_model_prior(n, k, alpha, beta_prior_mean, beta_prior_sd, sigma)
  X <- sim_data$X
  y <- sim_data$y
  
  # Compute posterior parameters
  sigma_n <- solve(sigma_0^(-1) + t(X) %*% X)
  mu_n <- sigma_n %*% (sigma_0^(-1) * mu_0 + t(X) %*% y)
  
  # Plot posterior density
  x_grid <- seq(-5, 5, length.out = 1000)
  post_dens <- dnorm(x_grid, mean = mu_n, sd = sqrt(sigma_n))
  plot(x_grid, post_dens, type = "l", lwd = 2, main = paste0("Posterior Density (n = ", n, ")"))
  
}
