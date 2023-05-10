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


#Exercise 2

theta <- seq(0,1,length=1000)
plot(theta, dbeta(theta, 1,1), col="lightblue", lwd=2,
     type="l", ylab="density", xlab="theta", main="Prior", ylim=c(0,20))
lines(theta, dbeta(theta, 20,2), col="coral", lwd=2)
lines(theta, dbeta(theta, 2,20), col="lightgreen", lwd=2)
lines(theta, dbeta(theta, 50,50), col="pink", lwd=2)
legend("topright", c("a0=b0=1", "a0=20, b0=2", "a0=2, b0=20", "a0=b0=50"),
       lwd=c(2,2,2), col=c("lightblue", "coral", "lightgreen", "pink"))


plot(theta, dbeta(theta, 1+10,1+20), type="l", col="lightblue", lwd=2,
     ylab="density", xlab="theta", main="Posterior",ylim=c(0,10))
lines(theta, dbeta(theta, 20+10,2+20), col="coral", lwd=2)
lines(theta, dbeta(theta, 2+10,20+20), col="lightgreen", lwd=2)
lines(theta, dbeta(theta, 50+10,50+20), col="pink", lwd=2)
legend("topright", c("a0=b0=1", "a0=20, b0=2", "a0=2, b0=20", "a0=b0=50"),
       lwd=c(2,2,2), col=c("lightblue", "coral", "lightgreen", "pink"))
theta <- seq(0,1,length=1000)
plot(theta, dbeta(theta, 1,1), col="lightblue", lwd=2,
     type="l", ylab="density", xlab="theta", main="Prior", ylim=c(0,20))
lines(theta, dbeta(theta, 1,2), col="coral", lwd=2)
lines(theta, dbeta(theta, 2,1), col="lightgreen", lwd=2)
lines(theta, dbeta(theta, 2,2), col="pink", lwd=2)
legend("topright", c("a0=b0=1", "a0=20, b0=2", "a0=2, b0=20", "a0=b0=50"),
       lwd=c(2,2,2), col=c("lightblue", "coral", "lightgreen", "pink"))

plot(theta, dbeta(theta, 1+10,1+20), type="l", col="lightblue", lwd=2,
     ylab="density", xlab="theta", main="Posterior",ylim=c(0,10))
lines(theta, dbeta(theta, 1+10,2+20), col="coral", lwd=2)
lines(theta, dbeta(theta, 2+10,1+20), col="lightgreen", lwd=2)
lines(theta, dbeta(theta, 2+10,2+20), col="pink", lwd=2)
legend("topright", c("a0=b0=1", "a0=1, b0=2", "a0=2, b0=1", "a0=b0=2"),
       lwd=c(2,2,2), col=c("lightblue", "coral", "lightgreen", "pink"))



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

#Question 4

n <- 10000
set.seed(202020)
prior1 <- rnorm(n)
prior2 <- rnorm(n,mean=2,sd=1/2)

par(mfrow=c(1,2))
hist(prior1,xlab="First Prior",col="lightblue",main="N(0,1)")
hist(prior2,xlab="Second Prior",col="coral",main="N(2,0.5)")


set.seed(1234)
prior1 <- rgamma(10000,shape=0.5,rate=0.01)
prior2 <- rgamma(10000,shape=0.5,rate=1)
prior3 <- rgamma(10000,shape=0.5,rate=100)

plot(density(prior1),col="lightblue",xlim=c(0,250),lwd=2,main="Prior Density")
lines(density(prior2), col = "coral",lwd=2)
lines(density(prior3), col = "lightgreen",lwd=2)
legend("topright", c("n=0.01", "n=1", "n=100"),
       col =c("lightblue","coral","lightgreen"), lwd=2)



# Set the number of simulations
n_sim <- 10000

# Choose a suitable prior distribution for 𝜂
eta <- rgamma(n_sim, shape = 5 , rate = 5)

# Simulate draws from the prior distribution of 𝜎^2|𝜂 using an inverse-gamma distribution
sigma2 <- 1/rgamma(n_sim, shape = 0.5, rate = eta)

# Visualize the prior distribution of 𝜎^2|𝜂
hist(sigma2, main = "Hyperprior from G(5,5) of sigma^2|eta", xlab = "sigma^2", col="lightblue")
plot(density(sigma2), main = "Prior distribution of sigma^2|eta", xlab = "sigma^2",col="lightblue")
