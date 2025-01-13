set.seed(100)
install.packages("dplyr")
library(dplyr)


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
samples_beta %>% plot(type = "l", first.panel = grid(),
                      main = "Traceplot of B")

# Looking at the first 100 samples of B
par(mfrow = c(1, 1))
samples_beta[1:100] %>% plot(type = "l", first.panel = grid(),
                             main = "Traceplot of B")

# Converges relatively quickly towards distribution, choose burnin to be 5
burnin <- 5
samples_beta[burnin:100] %>% plot(type = "l", first.panel = grid(),
                                  main = "Traceplot of B (Burnin = 5)")

# acf of samples
acf(samples_beta[burnin:100], main = "ACF of beta"); grid()

# Instead of burnin we could also use thinning, where we pick every second
# sample
samples_beta[1:100 * 2] %>% plot(type = "l", first.panel = grid(),
                                 main = "Traceplot of B (Thinning = 2)")

par(mfrow = c(1,3)) 
samples_lambda[1:100,1]%>% plot(type = "l", first.panel = grid(),
                                main = "Traceplot of lambda_1")
samples_lambda[1:100,5]%>% plot(type = "l", first.panel = grid(),
                                main = "Traceplot of lambda_5")
samples_lambda[1:100,10]%>% plot(type = "l", first.panel = grid(),
                                 main = "Traceplot of lambda_10")

samples_lambda[,1] %>% acf(main = "Lambda_1"); grid()
samples_lambda[,5] %>% acf(main = "Lambda_5"); grid()
samples_lambda[,10] %>% acf(main = "Lambda_10"); grid()

###### Exercise 11 ######
#We use the samples of lambda to draw from Y
posterior_predictive<-rpois(100000, lambda=samples_lambda%*%diag(t))

par(mfrow=c(5,2))
for (i in 1:10){
hist(samples_lambda[,i],main=paste("lambda", i),xlab="value")
abline(v=mean(samples_lambda[,i]),col="red")
abline(v=quantile(samples_lambda[,i],0.975), lty="dotted")
abline(v=quantile(samples_lambda[,i],0.025),lty="dotted")
}

#here we calculate the CPI
dim(apply(samples_lambda[],2,quantile,probs=c(0.025,0.975)))
sampleconf=apply(samples_lambda[],2,quantile,probs=c(0.025,0.975))

colMeans(samples_lambda)

par(mfrow=c(1,1))

##### Exercise 12 #####
#Here we generate the vectors for the upper and lower interval 
vector1=y/t-1.96*sqrt(y/(t**2))
vector2=y/t+1.96*sqrt(y/(t**2))
vector3= sampleconf[1,]
vector4=sampleconf[2,]

V1 <- c(vector1, vector2)  # 
V2 <- c(vector3, vector4)  # 
groups <- rep(1:10, times = 2)

# Plot the first box plot
boxplot(V1 ~ groups, col = adjustcolor("lightblue", alpha.f = 0.3), 
        main = "Overlapping Box Plot of confidence intervals", 
        xlab = "lambda", ylab = "Values", 
        outline = FALSE, notch = FALSE, 
        ylim = range(c(-1, 5)),medcol = NA
)

# Overlay the second box plot
boxplot(V2 ~ groups, col = adjustcolor("lightgreen", alpha.f = 0.3), 
        add = TRUE, notch = FALSE, outline = FALSE,medcol = NA)
legend("topleft", legend=c("Confidence interval", "CPI"),
       fill=c("lightblue", "lightgreen"),)

for (i in 1:10){
  print(quantile(samples_lambda[,i],0.025))
  print(quantile(samples_lambda[,i],0.975))
}

#here we find the posterior predictive
y_pred<-rpois(100000, lambda=samples_lambda%*%diag(t))%>% matrix(nrow=10000)
colMeans(y_pred)

##### Exercise 13 #####
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

# Function to sample beta with rate parameter = 60
rbeta <- function(lambda) {
  rgamma(1, 
         shape = 11,
         rate = sum(lambda) + 60)
         #rate = sum(lambda) + 20)
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

for (i in 1:10){
  print(quantile(samples_lambda[,i],0.025))
  print(quantile(samples_lambda[,i],0.975))
}
