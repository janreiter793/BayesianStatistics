library(magrittr)
set.seed(100)

# Params
T <- 50000 # Number of samples
k <- 10
y <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t <- c(94.320, 15.720, 62.880, 125.760, 5.240, 31.440, 1.048, 
       1.048, 2.096, 10.480)

# Proposal distribution
proposal <- function(condition, j) {
  prop <- 0
  tuning <- sqrt(y[j]) / t[j]
  # Sample until we have positive value
  while(prop <= 0) {
    prop <- rnorm(n = 1, mean = condition, sd = tuning)
  }
  return(prop)
}

# Target distribution
target <- function(lambda) {
  prod(lambda^y * exp(-t * lambda)) / (40 + sum(lambda))^11
}

# Hastings ratio
H <- function(x, z) {
   target(x) / target(z)
}

# Initialize lambda
lambdas <- matrix(nrow = k, ncol = T)
lambdas[,1] <- y / t                   # Naive parameter estimate

# The Metropolis-within-a-Gibbs sampler
acceptance_rate <- numeric(k)
for(i in 2:T) { # Gibbs sampler
  for(j in 1:k) { # Metropolis-Hastings
    # Define condition = (lambda_1, ..., lambda_(i-1), lambda_(i+1), ..., lambda_k)
    if(j == 1) {
      condition <- lambdas[1:k, i - 1]
    } else if(j == k) {
      condition <- c(lambdas[1:(k - 1), i], lambdas[k, i - 1]) 
    } else {
      condition <- c(lambdas[1:(j - 1), i], lambdas[j:k, i - 1]) 
    }
     
    # Generate proposal
    new_cond <- condition
    new_cond[j] <- proposal(condition[j], j)
    
    # Reject/accept with prop
    if(runif(1) < min(1, H(new_cond, condition))) {
      lambdas[j, i] <- new_cond[j]
      acceptance_rate[j] <- acceptance_rate[j] + 1
    } else {
      lambdas[j, i] <- lambdas[j, i - 1] 
    }
  }
}

acceptance_rate <- acceptance_rate / T

lambdas[1,] %>% acf(main = "ACF for lambda_1"); grid()

# Perform a thinning
step <- 50                            # With a thinning of 50 we have no
thinning <- 1:(T / step) * step       # significant autocorrelation
thinned_lambdas <- lambdas[,thinning]

thinned_lambdas[1,] %>% acf(main = "ACF for thinned lambda_1"); grid()
thinned_lambdas[5,] %>% acf(main = "ACF for lambda_5"); grid()
thinned_lambdas[10,] %>% acf(main = "ACF for lambda_10"); grid()
thinned_lambdas[1,] %>% plot(type = "l", 
                             panel.first = grid(),
                             main = "Samples of lambda_1")
thinned_lambdas[5,] %>% plot(type = "l", 
                             panel.first = grid(),
                             main = "Samples of lambda_5")
thinned_lambdas[10,] %>% plot(type = "l", 
                             panel.first = grid(),
                             main = "Samples of lambda_10")

