## JHU inferential statistics class final project
## Exploration of the exponential distribution

# Use R function rexp(n, rate=0.2)
# where n is the number of observations and rate is set to 0.2
# rate = lambda
# for the exponential distribution mean = 1/lambda
# and standard deviation = 1/lambda

# investigate the distribution of averages of 40 exponentials
# doing 1000 simulations

# Address the following:
# 1. show the sample mean and compare it to the theoretical mean
# 2. show how variable the sample is and compare it to the theoretical variance
# 3. show that the distribution is approximately normal

## MOTIVATING EXAMPLE
# Distribution of 1000 random uniforms:
hist(runif(1000))
# distribution is flat

# compare with the distribution of 1000 averages of 40 random samples
mns <- NULL
for (i in 1:1000){
    mns <- c(mns, mean(runif(40)))
}
hist(mns)   
# distribution of the means is approximately normal
 
## Now run a similar simulation of the exponential distribution

# define parameters:
lambda <- 0.2
n <- 40

# histogram of data drawn from an exponential distribution:
hist(rexp(1000, rate = lambda))

# histogram of 1000 means of 40 samples each from exponential distribution:
exp_mns <- NULL
exp_vars <- NULL
for (i in 1:1000){
    samp <- rexp(n, rate = lambda)
    exp_mns <- c(exp_mns, mean(samp))
    exp_vars <- c(exp_vars, var(samp))
}
hist(exp_mns)
hist(sqrt(exp_vars))
# both centered at 5!

# What is mean and variance of the distribution of means?
obs_mn <- mean(exp_mns)
obs_var <- var(exp_mns)
# 5.014 and 0.616

# relationship between the variance of means and population variance
# var = (sigma^2)/n, var*n should approximate theoretical variance
approx_theor_var <- obs_var*n
# 24.65

# Theoretical mean is 1/lambda
theor_mn <- 1/lambda
theor_var <- (1/lambda)^2
# mean = 5.0, variance = 25.0
# so 5.014 and 24.64 are pretty darn close!

# Finally, show that the distribution of means is approximately normal
mns_norm <- sqrt(n)*(exp_mns - theor_mn)/(1/lambda)
#using freq = FALSE plot the probabilities instead of the counts
hist(mns_norm, freq = FALSE)

# To compare, let's plot a normal distribution
hist(rnorm(10000, mean = 0, sd = 1), freq = FALSE)

## Yes, the distribution is approximately normal.




