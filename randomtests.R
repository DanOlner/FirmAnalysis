#Random tests


#MLE tests

# Simulate data: Assume we drew 10 marbles, 6 were black and 4 were white
n <- 10  # Number of trials
observed_black <- 6  # Number of black marbles drawn

# Define the likelihood function for a binomial distribution
log_likelihood <- function(p, n, observed_black) {
  if (p <= 0 || p >= 1) return(-Inf)  # Ensure p is between 0 and 1
  dbinom(observed_black, size = n, prob = p, log = TRUE)
}

# Optimize the likelihood function to find the MLE for p
mle_result <- optim(par = 0.5,  # Initial guess for p
                    fn = function(p) -log_likelihood(p, n, observed_black),
                    method = "Brent", 
                    lower = 0, 
                    upper = 1)

# Extract the MLE for p
mle_p <- mle_result$par

# Print the MLE result
cat("The maximum likelihood estimate (MLE) for p is:", mle_p, "\n")
