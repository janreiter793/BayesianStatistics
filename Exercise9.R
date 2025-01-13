library(tidyverse)
library(magrittr)
library(dplyr)
set.seed(100)

# Specify y and initial values for beta and lambda
y <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t <- c(94.320, 15.720, 62.880, 125.760, 5.240, 31.440, 1.048, 
       1.048, 2.096, 10.480)
lambda_0 <- rep(1, 10)
beta_0 <- 1

# Function to sample lambda
rlambda <- function(beta) {
  temp <- numeric(10)
  for(i in 1:10) {
    temp[i] <- rgamma(1, 
                      shape = y[i] + 1,
                      rate = t[i] + beta) 
  }
  return(temp)
}

# Function to sample beta
rbeta <- function(lambda) {
  rgamma(1, 
         shape = 11,
         rate = sum(lambda) + 40) 
}

# The Gibbs sampler
n_samples <- 10000
samples_lambda <- matrix(ncol = 10, nrow = n_samples)
samples_beta <- numeric(n_samples) 
samples_lambda[1,] <- lambda_0
samples_beta[1] <- beta_0
for(i in 2:n_samples) { 
  samples_beta[i] <- rbeta(samples_lambda[i - 1]) 
  samples_lambda[i,] <- rlambda(samples_beta[i]) 
}

# Illustrate the B-parameter
par(mfrow = c(1,2))
samples_beta %>% hist(prob = TRUE, breaks = 50,
                               main = "Histogram of B samples")
samples_beta %>% plot(type = "l", panel.first = grid(),
                               main = "Traceplot of B")

# Looking at the first 100 samples of B
par(mfrow = c(1, 1))
samples_beta[1:100] %>% plot(type = "l", panel.first = grid(),
                               main = "Traceplot of B")

# Converges relatively quickly towards distribution, choose burnin to be 5
burnin <- 5
samples_beta[burnin:100] %>% plot(type = "l", panel.first = grid(),
                             main = "Traceplot of B (Burnin = 5)")

# acf of samples
acf(samples_beta[burnin:100], main = "ACF of beta"); grid()

# Instead of burnin we could also use thinning, where we pick every second
# sample
samples_beta[1:100 * 2] %>% plot(type = "l", panel.first = grid(),
                                 main = "Traceplot of B (Thinning = 2)")

par(mfrow = c(1,3)) 
samples_lambda[1:100,1]%>% plot(type = "l", panel.first = grid(),
                           main = "Traceplot of lambda_1")
samples_lambda[1:100,5]%>% plot(type = "l", panel.first = grid(),
                           main = "Traceplot of lambda_5")
samples_lambda[1:100,10]%>% plot(type = "l", panel.first = grid(),
                           main = "Traceplot of lambda_10")

samples_lambda[,1] %>% acf(main = "Lambda_1"); grid()
samples_lambda[,5] %>% acf(main = "Lambda_5"); grid()
samples_lambda[,10] %>% acf(main = "Lambda_10"); grid()

