# Load necessary libraries
library(nloptr)
library(stargazer)

# Set the seed for reproducibility
set.seed(100)

# Generate the data set
N <- 100000
K <- 10

X <- matrix(rnorm(N * K), nrow = N, ncol = K)
X[, 1] <- 1

sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)

beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- X %*% beta + eps

# Compute OLS estimate using the closed-form solution
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_ols)

# Compute OLS estimate using gradient descent
beta_gd <- rep(0, K)
learning_rate <- 0.0000003
num_iterations <- 1000

for (i in 1:num_iterations) {
  gradient <- -2 * t(X) %*% (Y - X %*% beta_gd)
  beta_gd <- beta_gd - learning_rate * gradient
}
print(beta_gd)

# Compute OLS estimate using nloptr's L-BFGS algorithm
objective <- function(beta) {
  sum((Y - X %*% beta)^2)
}

gradient <- function(beta) {
  -2 * t(X) %*% (Y - X %*% beta)
}

result_lbfgs <- nloptr(x0 = rep(0, K), eval_f = objective, eval_grad_f = gradient, opts = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-6))
print(result_lbfgs$solution)

# Compute OLS estimate using nloptr's Nelder-Mead algorithm
result_nm <- nloptr(x0 = rep(0, K), eval_f = objective, opts = list(algorithm = "NLOPT_LN_NELDERMEAD", xtol_rel = 1e-6))
print(result_nm$solution)

# Compute MLE estimate using nloptr's L-BFGS algorithm
objective_mle <- function(theta) {
  beta <- theta[1:K]
  sig <- theta[K + 1]
  
  # Check for invalid values and handle them
  if (sig <= 0) {
    return(Inf)
  }
  
  ll <- sum(dnorm(Y, mean = X %*% beta, sd = sig, log = TRUE))
  
  # Check for NaNs and handle them
  if (is.nan(ll)) {
    return(Inf)
  }
  
  return(-ll)
}

gradient_mle <- function(theta) {
  beta <- theta[1:K]
  sig <- theta[K + 1]
  
  # Check for invalid values and handle them
  if (sig <= 0) {
    return(rep(0, length(theta)))
  }
  
  grad <- as.vector(rep(0, length(theta)))
  grad[1:K] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[K + 1] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  
  # Check for NaNs and handle them
  if (any(is.nan(grad))) {
    return(rep(0, length(theta)))
  }
  
  return(grad)
}

result_mle <- nloptr(x0 = c(rep(0, K), 1), eval_f = objective_mle, eval_grad_f = gradient_mle, opts = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-6))
print(result_mle$solution)

# Compute OLS estimate using lm() and export the output to a .tex file
model <- lm(Y ~ X - 1)
stargazer(model, type = "latex", out = "regression_output.tex")

