############################################################
# PRACTICAL 1: MLE AND CONFIDENCE INTERVAL FOR EXPONENTIAL RATE
############################################################

# AIM:
# To generate a random sample of size 50 from an exponential
# distribution with parameter lambda = 0.5, obtain the
# Maximum Likelihood Estimator (MLE) of lambda, and calculate
# its 95% confidence interval.

# PROBLEM:
# Generate a random sample of size 50 from an exponential
# distribution with parameter lambda = 0.5.
#
# (i) Obtain the Maximum Likelihood Estimator of lambda
#     based on the generated sample.
# (ii) Obtain the 95% confidence interval for lambda.

# THEORY:
#
# 1. EXPONENTIAL DISTRIBUTION
#
# The exponential distribution is a continuous probability
# distribution commonly used to model the waiting time
# between events occurring at a constant rate.
#
# Its probability density function is:
#
#             f(x; lambda) = lambda * exp(-lambda*x),
#                             x >= 0, lambda > 0
#
# Here, lambda is called the rate parameter.
#
# Mean:
#             E(X) = 1/lambda
#
# Variance:
#             Var(X) = 1/lambda^2
#
# 2. LIKELIHOOD FUNCTION
#
# Let X1, X2, ..., Xn be a random sample from a population
# having probability density function f(x; theta).
#
# Since the observations are independent, their joint
# density is the product of their individual densities.
# When the observed values are fixed, this joint density
# is viewed as a function of the unknown parameter theta.
# This function is called the likelihood function.
#
#             L(theta) = product(i=1 to n) f(xi; theta)
#
# The likelihood function indicates how well different
# values of the unknown parameter explain the observed
# sample.
#
# Probability describes the likelihood of possible data
# for a fixed parameter, whereas likelihood compares
# different parameter values for the observed data.
#
# 3. MAXIMUM LIKELIHOOD ESTIMATION (MLE)
#
# Maximum Likelihood Estimation is a method of estimating
# unknown population parameters. It selects the parameter
# value that makes the observed sample most likely.
#
# The maximum likelihood estimator is the value of theta
# that maximizes the likelihood function:
#
#             theta_hat = arg max L(theta)
#
# Instead of maximizing the likelihood directly, we
# generally maximize its natural logarithm, called the
# log-likelihood function:
#
#             l(theta) = log L(theta)
#
# Taking logarithms simplifies products into sums and
# makes differentiation easier.
#
# To obtain the MLE:
# 1. Write the likelihood function.
# 2. Take its natural logarithm.
# 3. Differentiate the log-likelihood with respect to
#    the unknown parameter.
# 4. Equate the derivative to zero and solve.
# 5. Check the second derivative to verify that the
#    solution gives a maximum.
#
# 4. DERIVATION OF MLE FOR EXPONENTIAL DISTRIBUTION
#
# Let X1, X2, ..., Xn be a random sample from an
# exponential distribution with rate parameter lambda.
#
# The PDF is:
#
#             f(x; lambda) = lambda * exp(-lambda*x)
#
# The likelihood function is:
#
#             L(lambda) = product(i=1 to n)
#                         [lambda * exp(-lambda*xi)]
#
# Therefore,
#
#             L(lambda) = lambda^n *
#                         exp[-lambda * sum(xi)]
#
# Taking natural logarithm:
#
#             l(lambda) = n log(lambda)
#                         - lambda * sum(xi)
#
# Differentiating with respect to lambda:
#
#             dl(lambda)/d(lambda) = n/lambda - sum(xi)
#
# Equating to zero:
#
#             n/lambda - sum(xi) = 0
#
#             n/lambda = sum(xi)
#
# Hence,
#
#             lambda_hat = n / sum(xi)
#
# Since x_bar = sum(xi)/n, we get:
#
#             lambda_hat = 1/x_bar
#
# To verify the maximum, differentiate again:
#
#             d^2l(lambda)/d(lambda)^2 = -n/lambda^2
#
# Since lambda > 0, the second derivative is negative.
# Therefore, the log-likelihood is maximized at:
#
#             MLE(lambda) = 1/x_bar
#
# 5. CONFIDENCE INTERVAL
#
# A confidence interval gives a range of plausible values
# for an unknown population parameter based on sample data.
# A 95% confidence interval is constructed by a method
# that covers the true parameter in 95% of repeated
# samples.
#
# For a random sample from the exponential distribution,
# the following result holds:
#
#             2*lambda*sum(Xi) ~ Chi-square(2n)
#
# Therefore,
#
# P[ chi-square(0.025, 2n) <= 2*lambda*sum(Xi)
#                    <= chi-square(0.975, 2n) ] = 0.95
#
# Rearranging the inequalities gives the exact 95%
# confidence interval for lambda:
#
# Lower limit = chi-square(0.025, 2n) / [2*sum(Xi)]
#
# Upper limit = chi-square(0.975, 2n) / [2*sum(Xi)]
#
# Here, chi-square(p, 2n) represents the pth quantile
# of the chi-square distribution with 2n degrees of freedom.
#
# In R:
# rexp() generates exponential random observations.
# mean() calculates the sample mean.
# sum() calculates the sum of observations.
# qchisq() gives quantiles of the chi-square distribution.

# R CODE:

# Generate a random sample
set.seed(123)
n <- 50
lambda <- 0.5

x <- rexp(n, rate=lambda)
x

# (i) Maximum Likelihood Estimate of lambda
lambda_mle <- 1 / mean(x)
lambda_mle

# (ii) 95% Confidence Interval for lambda
CI <- qchisq(c(0.025, 0.975), df=2*n) / (2*sum(x))
CI

# Display the confidence interval
cat("95% Confidence Interval for lambda:",
    CI[1], "to", CI[2], "\n")

# RESULT AND CONCLUSION:
#
# A random sample of size 50 was generated from an
# exponential distribution with rate parameter 0.5.
#
# The MLE of lambda was obtained using:
#
#             lambda_hat = 1 / sample mean
#
# The exact 95% confidence interval for lambda was
# calculated using the chi-square distribution with
# 2n = 100 degrees of freedom.
#
# The numerical MLE and confidence limits are displayed
# when the R code is executed.

############################################################
# END OF PRACTICAL 1
############################################################


































############################################################
#                         PRACTICAL 2
#       RANDOM NUMBERS, PMF/PDF AND HAZARD RATE FUNCTION
############################################################

# AIM:
# To generate random samples of size 25 from Poisson,
# Geometric, Lognormal and Weibull distributions and plot
# their PMF/PDF and Hazard Rate Function (HRF) for four
# choices of model parameters.

# PROBLEM:
# Generate random numbers of size 25 from:
# a) Poisson distribution
# b) Geometric distribution
# c) Lognormal distribution
# d) Weibull distribution
#
# Plot the PMF/PDF and HRF for four possible choices of
# model parameters. Give suitable x-axis, y-axis labels
# and main titles. Use par(mfrow).

# THEORY:
#
# 1. RANDOM NUMBER GENERATION
#
# Random number generation means obtaining observations
# from a specified probability distribution.
#
# In R:
# r-functions generate random observations.
# d-functions calculate PMF/PDF values.
# p-functions calculate cumulative probabilities (CDF).
#
# The PMF is used for a discrete random variable, while
# the PDF is used for a continuous random variable.
#
# The hazard rate function describes the chance/rate of
# an event occurring at a given point, conditional on the
# event not having occurred before that point.
#
# For a discrete random variable:
# h(x) = P(X=x)/P(X>=x)
#
# For a continuous random variable:
# h(x) = f(x)/[1-F(x)]
#
# 2. POISSON DISTRIBUTION
#
# The Poisson distribution is a discrete probability
# distribution used to model the number of events occurring
# in a fixed interval of time or space, when events occur
# independently at a constant average rate.
#
# Parameter:
# lambda > 0, representing the average number of events.
#
# PMF:
# P(X=x) = exp(-lambda)*lambda^x/x!, x=0,1,2,...
#
# CDF:
# F(x) = P(X<=x)
#
# Discrete hazard rate:
# h(x) = P(X=x)/P(X>=x)
#      = dpois(x,lambda)/(1-ppois(x-1,lambda))
#
# R functions: rpois(), dpois(), ppois().
#
# 3. GEOMETRIC DISTRIBUTION
#
# The Geometric distribution gives the number of failures
# before the first success in a sequence of independent
# Bernoulli trials, each having probability of success p.
#
# Parameter:
# 0 < p <= 1.
#
# PMF:
# P(X=x) = (1-p)^x*p, x=0,1,2,...
#
# CDF:
# F(x) = 1-(1-p)^(x+1), for integer x>=0.
#
# Discrete hazard rate:
# h(x) = P(X=x)/P(X>=x) = p.
#
# Thus, the Geometric distribution has a constant hazard
# rate for this R parameterization.
#
# R functions: rgeom(), dgeom(), pgeom().
#
# 4. LOGNORMAL DISTRIBUTION
#
# A positive random variable X follows a Lognormal
# distribution if its logarithm follows a normal
# distribution:
#
# log(X) ~ N(meanlog, sdlog^2).
#
# Parameters:
# meanlog = mean of log(X)
# sdlog = standard deviation of log(X), sdlog > 0.
#
# PDF:
# f(x) = 1/[x*sdlog*sqrt(2*pi)] *
#        exp[-(log(x)-meanlog)^2/(2*sdlog^2)], x>0.
#
# CDF:
# F(x) = Phi[(log(x)-meanlog)/sdlog].
#
# Hazard rate:
# h(x) = dlnorm(x,meanlog,sdlog) /
#        [1-plnorm(x,meanlog,sdlog)].
#
# R functions: rlnorm(), dlnorm(), plnorm().
#
# 5. WEIBULL DISTRIBUTION
#
# The Weibull distribution is a continuous distribution
# commonly used in reliability, lifetime and survival
# analysis.
#
# Parameters:
# k = shape parameter, k>0
# lambda = scale parameter, lambda>0
#
# PDF:
# f(x) = (k/lambda)*(x/lambda)^(k-1) *
#        exp[-(x/lambda)^k], x>=0.
#
# CDF:
# F(x) = 1-exp[-(x/lambda)^k].
#
# Hazard rate:
# h(x) = f(x)/[1-F(x)]
#      = (k/lambda)*(x/lambda)^(k-1).
#
# Interpretation of Weibull shape:
# k<1: decreasing hazard rate
# k=1: constant hazard rate
# k>1: increasing hazard rate.
#
# R functions: rweibull(), dweibull(), pweibull().
#
# NOTE ON PARAMETER VALUES:
# The parameter choices below follow the practical work:
# Poisson lambda = 2, 4, 5, 8.
# Geometric p = 0.2, 0.4, 0.6, 0.8.
# Lognormal (meanlog,sdlog) = (0,0.5), (0,1),
#                            (1,0.5), (1,1).
# Weibull shape = 1, 2, 3, 4 with scale = 1.

# R FUNCTIONS USED:
#
# set.seed()  : makes random samples reproducible.
# rpois()     : generates Poisson random numbers.
# dpois()     : calculates Poisson PMF.
# ppois()     : calculates Poisson CDF.
# rgeom()     : generates Geometric random numbers.
# dgeom()     : calculates Geometric PMF.
# pgeom()     : calculates Geometric CDF.
# rlnorm()    : generates Lognormal random numbers.
# dlnorm()    : calculates Lognormal PDF.
# plnorm()    : calculates Lognormal CDF.
# rweibull()  : generates Weibull random numbers.
# dweibull()  : calculates Weibull PDF.
# pweibull()  : calculates Weibull CDF.
# plot()      : draws graphs.
# par(mfrow)  : arranges plots in rows and columns.
# seq()       : creates equally spaced values for plotting.

# CODE AND OUTPUT:

############################################################
# I. POISSON DISTRIBUTION
############################################################

set.seed(123)

# Generate 25 random numbers for four lambda values
x1 <- rpois(25, lambda=2)
x2 <- rpois(25, lambda=4)
x3 <- rpois(25, lambda=5)
x4 <- rpois(25, lambda=8)

# Display random numbers
x1
x2
x3
x4

# Values of x for plotting
x <- 0:30

# PMF
y1 <- dpois(x, lambda=2)
y2 <- dpois(x, lambda=4)
y3 <- dpois(x, lambda=5)
y4 <- dpois(x, lambda=8)

par(mfrow=c(2,2))
plot(x,y1,type="h",lwd=2,xlab="x",ylab="P(X=x)",
     main="Poisson PMF: lambda=2")
plot(x,y2,type="h",lwd=2,xlab="x",ylab="P(X=x)",
     main="Poisson PMF: lambda=4")
plot(x,y3,type="h",lwd=2,xlab="x",ylab="P(X=x)",
     main="Poisson PMF: lambda=5")
plot(x,y4,type="h",lwd=2,xlab="x",ylab="P(X=x)",
     main="Poisson PMF: lambda=8")

# Hazard Rate Function
# h(x) = dpois(x,lambda)/[1-ppois(x-1,lambda)]
h1 <- dpois(x,lambda=2)/(1-ppois(x-1,lambda=2))
h2 <- dpois(x,lambda=4)/(1-ppois(x-1,lambda=4))
h3 <- dpois(x,lambda=5)/(1-ppois(x-1,lambda=5))
h4 <- dpois(x,lambda=8)/(1-ppois(x-1,lambda=8))

# Display hazard rates
h1
h2
h3
h4

# Plot HRF using the same vertical scale for comparison
par(mfrow=c(2,2))
plot(x,h1,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Poisson HRF: lambda=2")
plot(x,h2,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Poisson HRF: lambda=4")
plot(x,h3,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Poisson HRF: lambda=5")
plot(x,h4,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Poisson HRF: lambda=8")


############################################################
# II. GEOMETRIC DISTRIBUTION
############################################################

set.seed(123)

# Generate 25 random numbers for four p values
x1 <- rgeom(25, prob=0.2)
x2 <- rgeom(25, prob=0.4)
x3 <- rgeom(25, prob=0.6)
x4 <- rgeom(25, prob=0.8)

# Display random numbers
x1
x2
x3
x4

# Values of x for plotting
x <- 0:15

# PMF
y1 <- dgeom(x, prob=0.2)
y2 <- dgeom(x, prob=0.4)
y3 <- dgeom(x, prob=0.6)
y4 <- dgeom(x, prob=0.8)

par(mfrow=c(2,2))
plot(x,y1,type="h",lwd=2,xlab="Number of failures",
     ylab="P(X=x)",main="Geometric PMF: p=0.2")
plot(x,y2,type="h",lwd=2,xlab="Number of failures",
     ylab="P(X=x)",main="Geometric PMF: p=0.4")
plot(x,y3,type="h",lwd=2,xlab="Number of failures",
     ylab="P(X=x)",main="Geometric PMF: p=0.6")
plot(x,y4,type="h",lwd=2,xlab="Number of failures",
     ylab="P(X=x)",main="Geometric PMF: p=0.8")

# Hazard Rate Function
h1 <- dgeom(x,prob=0.2)/(1-pgeom(x-1,prob=0.2))
h2 <- dgeom(x,prob=0.4)/(1-pgeom(x-1,prob=0.4))
h3 <- dgeom(x,prob=0.6)/(1-pgeom(x-1,prob=0.6))
h4 <- dgeom(x,prob=0.8)/(1-pgeom(x-1,prob=0.8))

par(mfrow=c(2,2))
plot(x,h1,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Geometric HRF: p=0.2")
plot(x,h2,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Geometric HRF: p=0.4")
plot(x,h3,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Geometric HRF: p=0.6")
plot(x,h4,type="h",lwd=2,ylim=c(0,1),xlab="x",ylab="HRF",
     main="Geometric HRF: p=0.8")


############################################################
# III. LOGNORMAL DISTRIBUTION
############################################################

set.seed(123)

# Generate 25 random numbers for four parameter choices
x1 <- rlnorm(25, meanlog=0, sdlog=0.5)
x2 <- rlnorm(25, meanlog=0, sdlog=1)
x3 <- rlnorm(25, meanlog=1, sdlog=0.5)
x4 <- rlnorm(25, meanlog=1, sdlog=1)

# Display random numbers
x1
x2
x3
x4

# Values of x for plotting
x <- seq(0.01,10,length.out=500)

# PDF
y1 <- dlnorm(x,meanlog=0,sdlog=0.5)
y2 <- dlnorm(x,meanlog=0,sdlog=1)
y3 <- dlnorm(x,meanlog=1,sdlog=0.5)
y4 <- dlnorm(x,meanlog=1,sdlog=1)

par(mfrow=c(2,2))
plot(x,y1,type="l",lwd=2,xlab="x",ylab="Density",
     main="Lognormal PDF: (0, 0.5)")
plot(x,y2,type="l",lwd=2,xlab="x",ylab="Density",
     main="Lognormal PDF: (0, 1)")
plot(x,y3,type="l",lwd=2,xlab="x",ylab="Density",
     main="Lognormal PDF: (1, 0.5)")
plot(x,y4,type="l",lwd=2,xlab="x",ylab="Density",
     main="Lognormal PDF: (1, 1)")

# Hazard Rate Function
h1 <- dlnorm(x,meanlog=0,sdlog=0.5)/
  (1-plnorm(x,meanlog=0,sdlog=0.5))
h2 <- dlnorm(x,meanlog=0,sdlog=1)/
  (1-plnorm(x,meanlog=0,sdlog=1))
h3 <- dlnorm(x,meanlog=1,sdlog=0.5)/
  (1-plnorm(x,meanlog=1,sdlog=0.5))
h4 <- dlnorm(x,meanlog=1,sdlog=1)/
  (1-plnorm(x,meanlog=1,sdlog=1))

par(mfrow=c(2,2))
plot(x,h1,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Lognormal HRF: (0, 0.5)")
plot(x,h2,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Lognormal HRF: (0, 1)")
plot(x,h3,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Lognormal HRF: (1, 0.5)")
plot(x,h4,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Lognormal HRF: (1, 1)")


############################################################
# IV. WEIBULL DISTRIBUTION
############################################################

set.seed(123)

# Generate 25 random numbers for four shape values
x1 <- rweibull(25,shape=1,scale=1)
x2 <- rweibull(25,shape=2,scale=1)
x3 <- rweibull(25,shape=3,scale=1)
x4 <- rweibull(25,shape=4,scale=1)

# Display random numbers
x1
x2
x3
x4

# Values of x for plotting
x <- seq(0.01,3,length.out=500)

# PDF
y1 <- dweibull(x,shape=1,scale=1)
y2 <- dweibull(x,shape=2,scale=1)
y3 <- dweibull(x,shape=3,scale=1)
y4 <- dweibull(x,shape=4,scale=1)

par(mfrow=c(2,2))
plot(x,y1,type="l",lwd=2,xlab="x",ylab="Density",
     main="Weibull PDF: shape=1")
plot(x,y2,type="l",lwd=2,xlab="x",ylab="Density",
     main="Weibull PDF: shape=2")
plot(x,y3,type="l",lwd=2,xlab="x",ylab="Density",
     main="Weibull PDF: shape=3")
plot(x,y4,type="l",lwd=2,xlab="x",ylab="Density",
     main="Weibull PDF: shape=4")

# Hazard Rate Function
# For Weibull, h(x) = (shape/scale)*(x/scale)^(shape-1)
# This direct expression is numerically stable.
h1 <- (1/1)*(x/1)^(1-1)
h2 <- (2/1)*(x/1)^(2-1)
h3 <- (3/1)*(x/1)^(3-1)
h4 <- (4/1)*(x/1)^(4-1)

par(mfrow=c(2,2))
plot(x,h1,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Weibull HRF: shape=1")
plot(x,h2,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Weibull HRF: shape=2")
plot(x,h3,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Weibull HRF: shape=3")
plot(x,h4,type="l",lwd=2,xlab="x",ylab="HRF",
     main="Weibull HRF: shape=4")

par(mfrow=c(1,1))

# RESULT AND CONCLUSION:
# Random samples of size 25 were generated from all four
# distributions for the specified parameter choices.
# Their PMF/PDF and hazard rate functions were plotted.
# The graphs show how the distribution and hazard rate
# change with different parameter values.

# INTERPRETATION:
# In the Poisson distribution, the hazard rate generally
# increases with x and approaches 1. A larger lambda shifts
# this increase towards larger values of x.
#
# In the Geometric distribution, the hazard rate remains
# constant and equals the success probability p. Hence,
# increasing p raises the constant hazard level.
#
# In the Lognormal distribution, the hazard rate is not
# constant; it changes with x and depends on meanlog and
# sdlog. The parameter values also affect the spread and
# shape of the density curve.
#
# In the Weibull distribution, the shape parameter controls
# the hazard pattern. Shape=1 gives a constant hazard,
# while shape values greater than 1 give an increasing
# hazard rate. Thus, the four graphs demonstrate the
# effect of model parameters on the distributions.

############################################################
#                    END OF PRACTICAL 2
############################################################
















############################################################
#                         PRACTICAL 3
#       CENTRAL LIMIT THEOREM AND TOTAL INSURANCE CLAIMS
############################################################

# AIM:
# To approximate the total claim amount using the Central
# Limit Theorem, calculate its 95th percentile and determine
# the premium loading factor.

# PROBLEM:
# A life insurance company issues one-year term life
# insurance contracts with benefit amounts 1, 2, 3 and 4
# units. The corresponding probabilities of death are
# 0.01, 0.02, 0.03 and 0.04, respectively.
#
# The numbers of individuals in the four classes are
# 50, 60, 40 and 50, respectively.
#
# The total claim amount is to be approximated using the
# Central Limit Theorem. The company wants to collect an
# amount equal to the 95th percentile of the distribution
# of total claims. Find the 95th percentile and the
# premium loading factor.

# THEORY:
#
# 1. CENTRAL LIMIT THEOREM
#
# The Central Limit Theorem states that the sum of a large
# number of independent random variables with finite means
# and variances is approximately normally distributed.
#
# If X1, X2, ..., Xn are independent random variables and
# S = X1 + X2 + ... + Xn, then:
#
# E(S) = Sum E(Xi)
#
# Var(S) = Sum Var(Xi)
#
# Therefore, for a sufficiently large number of variables:
#
# S is approximately N(mu, sigma^2)
#
# where mu is the mean of S and sigma^2 is its variance.
#
# 2. CLAIM AMOUNT FOR ONE POLICYHOLDER
#
# Consider a policy with benefit amount b and probability
# of death q. Let X denote the claim paid by the insurer.
#
# If death occurs, the insurer pays b units.
# If death does not occur, the insurer pays zero.
#
# Thus:
# X = b with probability q
# X = 0 with probability 1-q
#
# Expected claim:
#
# E(X) = b*q + 0*(1-q)
#      = b*q
#
# Variance of claim:
#
# Var(X) = E(X^2) - [E(X)]^2
#
# E(X^2) = b^2*q + 0^2*(1-q)
#        = b^2*q
#
# Hence:
# Var(X) = b^2*q - (b*q)^2
#        = b^2*q*(1-q)
#
# 3. MEAN OF TOTAL CLAIMS
#
# Suppose there are n_i policyholders in class i, each
# having benefit amount b_i and probability of death q_i.
#
# The total claim amount is the sum of all individual claims.
# By the additivity of expectation:
#
# E(S) = Sum[n_i*b_i*q_i]
#
# For the given four classes:
#
# Class 1: 50*1*0.01 = 0.50
# Class 2: 60*2*0.02 = 2.40
# Class 3: 40*3*0.03 = 3.60
# Class 4: 50*4*0.04 = 8.00
#
# Therefore:
# E(S) = 0.50 + 2.40 + 3.60 + 8.00
#      = 14.50 units
#
# 4. VARIANCE OF TOTAL CLAIMS
#
# Since the individual claims are assumed independent,
# the variance of the total is the sum of their variances.
#
# Var(S) = Sum[n_i*b_i^2*q_i*(1-q_i)]
#
# Class 1:
# 50*1^2*0.01*(1-0.01) = 0.495
#
# Class 2:
# 60*2^2*0.02*(1-0.02) = 4.704
#
# Class 3:
# 40*3^2*0.03*(1-0.03) = 10.476
#
# Class 4:
# 50*4^2*0.04*(1-0.04) = 30.720
#
# Therefore:
# Var(S) = 0.495 + 4.704 + 10.476 + 30.720
#        = 46.395
#
# Standard deviation:
# SD(S) = sqrt(46.395) = 6.8114 units (approximately)
#
# 5. NORMAL APPROXIMATION USING CLT
#
# By the Central Limit Theorem, the total claim amount
# is approximated by a normal distribution:
#
# S approximately follows N(14.50, 46.395)
#
# Its mean is 14.50 and its standard deviation is 6.8114.
#
# 6. 95TH PERCENTILE
#
# The 95th percentile is the amount such that 95% of the
# distribution of total claims lies at or below that amount.
#
# Let P95 denote the 95th percentile. Then:
#
# P(S <= P95) = 0.95
#
# Standardising the normal variable:
#
# (P95-mu)/sigma = z_0.95
#
# Therefore:
#
# P95 = mu + z_0.95*sigma
#
# From the standard normal distribution:
# z_0.95 = 1.644854
#
# Hence:
# P95 = 14.50 + 1.644854*6.8114
#     = approximately 25.7037 units
#
# 7. PREMIUM LOADING FACTOR
#
# The expected claim amount is called the pure premium.
# The insurer may collect an additional amount above the
# pure premium to provide a margin for claim uncertainty.
#
# Premium loading amount:
# = 95th percentile - Expected total claims
#
# = 25.7037 - 14.50
# = approximately 11.2037 units
#
# Premium loading factor:
# = (95th percentile - Expected claims) /
#   Expected claims
#
# Premium loading percentage:
# = Premium loading factor * 100
#
# Premium multiplier:
# = 95th percentile / Expected claims
#
# The loading factor is the additional proportion above
# the pure premium; the premium multiplier includes the
# pure premium itself.

# R FUNCTIONS USED:
#
# c()       : creates vectors.
# sum()     : calculates the sum of values.
# sqrt()    : calculates square root.
# qnorm()   : obtains a quantile of the normal distribution.
# cat()     : displays the results.

# CODE AND OUTPUT:

# Number of individuals in each class
n <- c(50, 60, 40, 50)

# Benefit amounts
b <- c(1, 2, 3, 4)

# Probability of death
q <- c(0.01, 0.02, 0.03, 0.04)

# Expected total claim
mean_claim <- sum(n*b*q)

# Variance of total claim
var_claim <- sum(n*b^2*q*(1-q))

# Standard deviation
sd_claim <- sqrt(var_claim)

# 95th percentile of standard normal distribution
z <- qnorm(0.95)

# 95th percentile of total claim distribution
percentile_95 <- mean_claim + z*sd_claim

# Premium loading amount
loading_amount <- percentile_95-mean_claim

# Premium loading factor and percentage
loading_factor <- loading_amount/mean_claim
loading_percentage <- loading_factor*100

# Premium multiplier
premium_multiplier <- percentile_95/mean_claim

# Display results
cat("Mean of total claims =",mean_claim,"\n")
cat("Variance of total claims =",var_claim,"\n")
cat("Standard deviation =",sd_claim,"\n")
cat("95th percentile =",percentile_95,"\n")
cat("Premium loading amount =",loading_amount,"\n")
cat("Premium loading factor =",loading_factor,"\n")
cat("Premium loading percentage =",loading_percentage,"%\n")
cat("Premium multiplier =",premium_multiplier,"\n")

# RESULT AND CONCLUSION:
#
# The expected total claim amount is 14.50 units.
# The variance is 46.395 and the standard deviation is
# approximately 6.8114 units.
#
# Using the Central Limit Theorem, the 95th percentile of
# the total claim distribution is approximately 25.7037
# units. The premium loading amount is approximately
# 11.2037 units, corresponding to a loading percentage
# of approximately 77.27%.

# INTERPRETATION:
#
# The expected total claim of 14.50 units represents the
# average amount the company would expect to pay over many
# similar groups of policyholders.
#
# The 95th percentile of approximately 25.7037 units means
# that, under the normal approximation, there is a 95%
# probability that the total claims will not exceed this
# amount. There is therefore approximately a 5% probability
# that total claims will be higher.
#
# To collect an amount equal to this 95th percentile, the
# company needs to collect approximately 11.2037 units
# more than the expected claims. This is a loading of
# approximately 77.27% over the pure premium.
#
# Thus, the premium amount based on this criterion is
# approximately 1.7727 times the expected total claim.
# This calculation is based on the assumptions of
# independent claims and the normal approximation.

############################################################
#                    END OF PRACTICAL 3
############################################################




























############################################################
#                         PRACTICAL 4
#              AGGREGATE CLAIMS IN AN INSURANCE PORTFOLIO
############################################################

# AIM:
# To calculate the exact probability that the aggregate
# claim amount of an insurance portfolio exceeds three
# times its expected aggregate claim amount.

# PROBLEM:
# An insurance portfolio produces N claims, where N is a
# random variable with the following probability distribution:
#
#       N       0      1      3
#     P(N)     0.5    0.4    0.1
#
# The individual claim amount X has the distribution:
#
#       X       1      10
#     P(X)     0.9    0.1
#
# Assume that individual claim amounts are independent
# and are also independent of N.
#
# Let the aggregate claim amount be:
#
#       S = X1 + X2 + ... + XN
#
# Calculate the exact probability:
#
#       P(S > 3 E(S))

# THEORY:
#
# 1. RANDOM NUMBER OF CLAIMS
#
# N represents the number of claims produced by the
# insurance portfolio. It is a discrete random variable
# with the given probability distribution.
#
# Its expected value is:
#
# E(N) = Sum[n*P(N=n)]
#
# For this problem:
#
# E(N) = 0(0.5) + 1(0.4) + 3(0.1)
#      = 0 + 0.4 + 0.3
#      = 0.7
#
# 2. INDIVIDUAL CLAIM AMOUNT
#
# X represents the amount paid for one claim.
# Its probability distribution is:
#
# X=1  with probability 0.9
# X=10 with probability 0.1
#
# The expected individual claim amount is:
#
# E(X) = Sum[x*P(X=x)]
#
# E(X) = 1(0.9) + 10(0.1)
#      = 0.9 + 1
#      = 1.9
#
# 3. EXPECTED AGGREGATE CLAIM AMOUNT
#
# The aggregate claim amount is:
#
# S = X1 + X2 + ... + XN
#
# Since the individual claim amounts are independent
# of N and have the same distribution, the expected
# aggregate claim amount is:
#
# E(S) = E(N)*E(X)
#
# E(S) = 0.7*1.9
#      = 1.33
#
# Therefore:
#
# 3E(S) = 3*1.33 = 3.99
#
# We need to calculate:
#
# P(S > 3.99)
#
# Since the aggregate claim amount can only take integer
# values in this problem:
#
# P(S > 3.99) = P(S >= 4)
#
# 4. EXACT DISTRIBUTION OF AGGREGATE CLAIMS
#
# We consider the possible values of N separately.
#
# CASE 1: N=0
#
# No claims occur, so S=0.
#
# P(S=0) = P(N=0) = 0.5
#
# CASE 2: N=1
#
# There is one claim, so S=X1.
#
# P(S=1)  = P(N=1)*P(X=1)
#         = 0.4*0.9 = 0.36
#
# P(S=10) = P(N=1)*P(X=10)
#         = 0.4*0.1 = 0.04
#
# CASE 3: N=3
#
# There are three independent individual claims:
#
# S = X1+X2+X3
#
# Each individual claim is 1 with probability 0.9
# and 10 with probability 0.1.
#
# All three claims are 1:
#
# P(S=3) = 0.1*(0.9)^3
#        = 0.0729
#
# Exactly one claim is 10 and two claims are 1:
#
# P(S=12) = 0.1*3*(0.1)*(0.9)^2
#         = 0.0243
#
# Exactly two claims are 10 and one claim is 1:
#
# P(S=21) = 0.1*3*(0.1)^2*(0.9)
#         = 0.0027
#
# All three claims are 10:
#
# P(S=30) = 0.1*(0.1)^3
#         = 0.0001
#
# 5. REQUIRED PROBABILITY
#
# The aggregate claim exceeds 3.99 in the following cases:
#
# N=1 and X1=10, giving S=10.
#
# N=3 and at least one of the three individual claims
# is 10, giving S=12, 21 or 30.
#
# Therefore:
#
# P(S>3.99)
# = P(N=1)*P(X1=10)
#   + P(N=3)*P(at least one of X1,X2,X3 is 10)
#
# = 0.4*0.1 + 0.1*[1-(0.9)^3]
#
# = 0.04 + 0.1*(0.271)
#
# = 0.04 + 0.0271
#
# = 0.0671
#
# Thus, the exact required probability is 0.0671,
# or 6.71%.

# R FUNCTIONS USED:
#
# c()       : creates vectors.
# sum()     : calculates the sum of values.
# prod()    : calculates the product of values.
# choose()  : calculates a binomial coefficient.
# cat()     : displays the results.

# CODE AND OUTPUT:

# Probability distribution of number of claims
N <- c(0,1,3)
PN <- c(0.5,0.4,0.1)

# Probability distribution of individual claim amount
X <- c(1,10)
PX <- c(0.9,0.1)

# Expected number of claims
EN <- sum(N*PN)

# Expected individual claim amount
EX <- sum(X*PX)

# Expected aggregate claim amount
ES <- EN*EX

# Three times the expected aggregate claim
limit <- 3*ES

# Exact probability that aggregate claims exceed the limit
# For N=1: the individual claim must be 10.
prob_N1 <- 0.4*0.1

# For N=3: at least one of the three claims must be 10.
prob_N3 <- 0.1*(1-(0.9)^3)

# Total required probability
probability <- prob_N1+prob_N3

# Display the results
cat("Expected number of claims =",EN,"\n")
cat("Expected individual claim amount =",EX,"\n")
cat("Expected aggregate claim amount =",ES,"\n")
cat("Three times expected aggregate claims =",limit,"\n")
cat("Probability P(S > 3E(S)) =",probability,"\n")
cat("Probability in percentage =",probability*100,"%\n")

# RESULT AND CONCLUSION:
#
# The expected number of claims is 0.7 and the expected
# individual claim amount is 1.9 units.
#
# Therefore, the expected aggregate claim amount is
# E(S)=1.33 units.
#
# Three times the expected aggregate claim amount is
# 3E(S)=3.99 units.
#
# The exact probability that the aggregate claim amount
# exceeds 3.99 units is 0.0671, or 6.71%.

# INTERPRETATION:
#
# The result 0.0671 means that there is a 6.71% probability
# that the total amount paid by the insurance portfolio
# will be greater than three times its expected aggregate
# claim amount.
#
# In other words, in approximately 6.71 out of every 100
# portfolios with the same claim structure, the aggregate
# claims would exceed 3.99 units.
#
# This probability is relatively small, indicating that
# aggregate claims exceeding three times their expected
# value are unlikely under the given probability model.
#
# The calculation is exact and uses the given distributions;
# no normal or Central Limit Theorem approximation is needed.

############################################################
#                    END OF PRACTICAL 4
############################################################















































############################################################
#                         PRACTICAL 5
#              INSURANCE AND EXPONENTIAL UTILITY
############################################################

# AIM:
# To calculate the maximum premium an individual is willing
# to pay for complete insurance and the minimum premium an
# insurance company is willing to accept, using exponential
# utility functions.

# PROBLEM:
#
# (a) An individual has wealth W=100 units and faces a
# random loss X with an exponential distribution having
# mean 0.2. Find the maximum amount this individual will
# pay for complete insurance against the specified random
# loss if the utility function is:
#
#       U(w) = -exp(-2w)
#
# (b) Suppose the wealth of an insurance company is
# W=10,00,000 units. Find the minimum acceptable amount
# that this company will charge for complete insurance
# against the random loss X, if its utility function is:
#
#       U(w) = -exp(-w)

# THEORY:
#
# 1. UTILITY FUNCTION
#
# A utility function represents the satisfaction or
# preference an individual or company derives from wealth.
# A decision-maker generally prefers a higher utility.
#
# The exponential utility function is:
#
#       U(w) = -exp(-a*w)
#
# where:
# w = wealth
# a = positive risk-aversion parameter
#
# The larger the value of a, the more risk-averse the
# decision-maker is.
#
# The exponential utility function has constant absolute
# risk aversion, which means its absolute risk-aversion
# coefficient is equal to a.
#
# 2. EXPONENTIAL DISTRIBUTION OF LOSS
#
# The random loss X follows an exponential distribution
# with mean 0.2.
#
# For an exponential distribution:
#
#       Mean = 1/lambda
#
# Therefore:
#
#       lambda = 1/0.2 = 5
#
# Its probability density function is:
#
#       f(x) = lambda*exp(-lambda*x), x >= 0
#
# The moment generating function (MGF) is:
#
#       M_X(t) = E[exp(tX)] = lambda/(lambda-t)
#
# provided t < lambda.
#
# Hence, for lambda=5:
#
#       M_X(t) = 5/(5-t)
#
# 3. PART (a): MAXIMUM PREMIUM PAID BY THE INDIVIDUAL
#
# Initial wealth of the individual is W=100.
# The individual faces a random loss X.
#
# If the individual does not purchase insurance, final
# wealth is:
#
#       W-X
#
# Expected utility without insurance is:
#
#       E[U(W-X)]
#       = E[-exp(-2(W-X))]
#       = -exp(-2W)*E[exp(2X)]
#
# If the individual purchases complete insurance by paying
# a certain premium P, the loss is fully covered and final
# wealth is:
#
#       W-P
#
# Utility with complete insurance is:
#
#       U(W-P) = -exp[-2(W-P)]
#
# The maximum premium is the indifference premium. At this
# amount, the individual is equally satisfied with buying
# insurance and remaining uninsured:
#
#       U(W-P) = E[U(W-X)]
#
# Substituting:
#
#       -exp[-2(W-P)] = -exp(-2W)*E[exp(2X)]
#
# Cancelling the common term and simplifying:
#
#       exp(2P) = E[exp(2X)]
#
# Taking logarithms:
#
#       2P = ln E[exp(2X)]
#
# Therefore:
#
#       P = (1/2)*ln[M_X(2)]
#
# Since M_X(t)=5/(5-t):
#
#       P = (1/2)*ln[5/(5-2)]
#         = (1/2)*ln(5/3)
#         = 0.2554 units (approximately)
#
# This is the maximum premium the individual is willing
# to pay for complete insurance.
#
# 4. PART (b): MINIMUM PREMIUM ACCEPTED BY THE COMPANY
#
# The insurance company's initial wealth is:
#
#       W = 10,00,000 units
#
# It receives a premium P and pays the random claim X.
# Its final wealth after providing complete insurance is:
#
#       W+P-X
#
# The company will accept the insurance contract if its
# expected utility after accepting the contract is at
# least as large as its utility from retaining its initial
# wealth without taking the risk.
#
# At the minimum acceptable premium, the company is
# indifferent between accepting and rejecting the contract:
#
#       E[U(W+P-X)] = U(W)
#
# Using U(w)=-exp(-w):
#
#       E[-exp(-(W+P-X))] = -exp(-W)
#
#       -exp(-W-P)*E[exp(X)] = -exp(-W)
#
# Simplifying:
#
#       exp(-P)*E[exp(X)] = 1
#
# Taking logarithms:
#
#       P = ln E[exp(X)]
#
# Therefore:
#
#       P = ln[M_X(1)]
#
# Using the exponential MGF:
#
#       P = ln[5/(5-1)]
#         = ln(5/4)
#         = 0.2231 units (approximately)
#
# The initial wealth W cancels because the company has
# an exponential utility function with constant absolute
# risk aversion.
#
# This is the minimum premium the company will accept
# under the stated indifference condition.

# R FUNCTIONS USED:
#
# log()  : calculates the natural logarithm.
# exp()  : calculates the exponential function.
# cat()  : displays the calculated results.

# CODE AND OUTPUT:

# Mean of exponential loss distribution
mean_loss <- 0.2

# Rate parameter
lambda <- 1/mean_loss

# Part (a): Maximum premium for the individual
W_individual <- 100
a <- 2

premium_individual <- log(lambda/(lambda-a))/a

# Part (b): Minimum premium for the insurance company
W_company <- 1000000
b <- 1

premium_company <- log(lambda/(lambda-b))/b

# Display results
cat("Rate parameter =",lambda,"\n")
cat("Part (a): Maximum premium =",premium_individual,"\n")
cat("Part (b): Minimum acceptable premium =",premium_company,"\n")

# RESULT AND CONCLUSION:
#
# (a) The maximum premium the individual is willing to pay
# for complete insurance is approximately 0.2554 units.
#
# (b) The minimum premium the insurance company is willing
# to accept for complete insurance is approximately
# 0.2231 units.
#
# Both values are obtained by equating the expected utility
# under the risky position with the utility under the
# certain insured position.

# INTERPRETATION:
#
# In part (a), the individual is willing to pay at most
# approximately 0.2554 units to remove the uncertainty
# associated with the random loss. If the premium is higher
# than this amount, the individual would prefer to remain
# uninsured, according to the given utility function.
#
# In part (b), the insurance company requires at least
# approximately 0.2231 units to accept the risk. A premium
# below this amount would make the company worse off in
# expected utility terms compared with not accepting the
# contract.
#
# The company's initial wealth of 10,00,000 units does not
# affect the calculated minimum premium because exponential
# utility has constant absolute risk aversion.
#
# The individual and company have different risk-aversion
# parameters: 2 for the individual and 1 for the company.
# Consequently, their indifference premiums are different.
#
# These are reservation premiums based on expected utility,
# not premiums including expenses, profit margins or other
# business costs.

############################################################
#                    END OF PRACTICAL 5
############################################################








































############################################################
#                         PRACTICAL 6
#          FORCE OF MORTALITY, SURVIVAL AND DENSITY
############################################################

# AIM:
# To obtain the survival function S(x), probability density
# function f(x), and the probability density function g(t)
# of the future lifetime T(20), using the given force of
# mortality, and to check whether f(x) and g(t) are density
# functions.

# PROBLEM:
# Suppose the life length X is modelled by a distribution
# with force of mortality specified as:
#
#             0.01,     if 0 <= x < 15
#   mu_x  =   0.02,     if 15 <= x < 25
#             0.03,     if x >= 25
#
# Find the corresponding S(x), f(x), and PDF g(t) of
# T(20). Check whether f(x) and g(t) are density functions.

# THEORY:
#
# 1. FORCE OF MORTALITY
#
# The force of mortality at age x, denoted by mu_x, is the
# instantaneous rate of death at age x, conditional on the
# individual surviving up to that age.
#
# It is defined as:
#
#       mu_x = f(x)/S(x)
#
# where:
# f(x) = probability density function of the life length
# S(x) = survival function
#
# The force of mortality is also related to the survival
# function by:
#
#       mu_x = -d/dx [ln S(x)]
#
# Therefore:
#
#       S(x) = exp[- integral from 0 to x of mu_u du]
#
# 2. SURVIVAL FUNCTION
#
# The survival function gives the probability that a
# person survives beyond age x:
#
#       S(x) = P(X>x)
#
# Since the force of mortality is piecewise constant, we
# calculate the integral separately over each interval.
#
# For 0 <= x < 15:
#
#       S(x) = exp[-0.01*x]
#
# For 15 <= x < 25:
#
#       integral from 0 to x of mu_u du
#       = 0.01(15) + 0.02(x-15)
#
#       S(x) = exp[-0.15 - 0.02(x-15)]
#
# For x >= 25:
#
#       integral from 0 to x of mu_u du
#       = 0.01(15) + 0.02(10) + 0.03(x-25)
#
#       S(x) = exp[-0.35 - 0.03(x-25)]
#
# Hence the complete survival function is:
#
#       exp(-0.01*x),                  0 <= x < 15
# S(x)= exp[-0.15-0.02(x-15)],        15 <= x < 25
#       exp[-0.35-0.03(x-25)],        x >= 25
#
# 3. PROBABILITY DENSITY FUNCTION f(x)
#
# The force of mortality and survival function are related
# to the density function by:
#
#       f(x) = mu_x*S(x)
#
# Therefore:
#
# For 0 <= x < 15:
#
#       f(x) = 0.01*exp(-0.01*x)
#
# For 15 <= x < 25:
#
#       f(x) = 0.02*exp[-0.15-0.02(x-15)]
#
# For x >= 25:
#
#       f(x) = 0.03*exp[-0.35-0.03(x-25)]
#
# 4. FUTURE LIFETIME T(20)
#
# T(20) denotes the future lifetime of a person who has
# already survived to age 20.
#
# The survival function of the future lifetime is:
#
#       P(T(20)>t) = S(20+t)/S(20)
#
# The density function of T(20) is:
#
#       g(t) = f(20+t)/S(20)
#
#                 = mu_(20+t)*S(20+t)/S(20)
#
# Since age 20 lies in the interval 15 <= x < 25:
#
#       S(20) = exp[-0.15-0.02(20-15)]
#             = exp(-0.25)
#
# For 0 <= t < 5, the attained age 20+t is less than 25.
# The force of mortality is 0.02. Thus:
#
#       S(20+t)/S(20) = exp(-0.02*t)
#
# and:
#
#       g(t) = 0.02*exp(-0.02*t), 0 <= t < 5
#
# For t >= 5, the attained age is at least 25. The person
# first survives 5 years with force 0.02 and then has force
# 0.03:
#
#       S(20+t)/S(20)
#       = exp[-0.02(5)-0.03(t-5)]
#
# Therefore:
#
#       g(t) = 0.03*exp[-0.10-0.03(t-5)], t >= 5
#
# Hence:
#
#       0.02*exp(-0.02*t),             0 <= t < 5
# g(t)=
#       0.03*exp[-0.10-0.03(t-5)],    t >= 5
#
# 5. CHECKING WHETHER f(x) IS A DENSITY FUNCTION
#
# A function is a probability density function if:
#
# (i) It is non-negative for all x.
# (ii) Its integral over the entire range is equal to 1.
#
# Since mu_x >= 0 and S(x)>0, f(x)=mu_x*S(x) is
# non-negative.
#
# Also:
#
# Integral from 0 to infinity of f(x) dx
# = 0.01 integral_0^15 exp(-0.01x) dx
#   + 0.02 integral_15^25 exp[-0.15-0.02(x-15)] dx
#   + 0.03 integral_25^infinity exp[-0.35-0.03(x-25)] dx
#
# = [1-exp(-0.15)]
#   + [exp(-0.15)-exp(-0.35)]
#   + exp(-0.35)
#
# = 1
#
# Thus, f(x) is a valid density function.
#
# 6. CHECKING WHETHER g(t) IS A DENSITY FUNCTION
#
# The function g(t) is non-negative.
#
# Its integral is:
#
# Integral from 0 to infinity of g(t) dt
# = 0.02 integral_0^5 exp(-0.02t) dt
#   + 0.03 integral_5^infinity
#       exp[-0.10-0.03(t-5)] dt
#
# = [1-exp(-0.10)] + exp(-0.10)
#
# = 1
#
# Thus, g(t) is also a valid density function.

# R FUNCTIONS USED:
#
# ifelse()   : applies the appropriate formula in each
#              interval.
# integrate(): numerically evaluates a definite integral.
# exp()      : calculates the exponential function.
# cat()      : displays results.
# curve()    : plots a mathematical function.

# CODE AND OUTPUT:

# Force of mortality
mu <- function(x) {
  ifelse(x < 15, 0.01,
         ifelse(x < 25, 0.02, 0.03))
}

# Survival function S(x)
S <- function(x) {
  ifelse(x < 15,
         exp(-0.01*x),
         ifelse(x < 25,
                exp(-0.15-0.02*(x-15)),
                exp(-0.35-0.03*(x-25))))
}

# Density function f(x)
f <- function(x) {
  mu(x)*S(x)
}

# Survival probability at age 20
S20 <- S(20)

# Density function g(t) of future lifetime T(20)
g <- function(t) {
  ifelse(t < 5,
         0.02*exp(-0.02*t),
         0.03*exp(-0.10-0.03*(t-5)))
}

# Display survival probability at age 20
cat("S(20) =",S20,"\n")

# Integrate f(x) separately over its three intervals.
# Splitting at 15 and 25 handles the changes in force
# of mortality explicitly.
integral_f1 <- integrate(f,lower=0,upper=15)$value
integral_f2 <- integrate(f,lower=15,upper=25)$value
integral_f3 <- integrate(f,lower=25,upper=Inf)$value
integral_f <- integral_f1+integral_f2+integral_f3

# Integrate g(t) separately at t=5, where the force
# of mortality changes for a person aged 20.
integral_g1 <- integrate(g,lower=0,upper=5)$value
integral_g2 <- integrate(g,lower=5,upper=Inf)$value
integral_g <- integral_g1+integral_g2

cat("Integral of f(x) =",integral_f,"\n")
cat("Integral of g(t) =",integral_g,"\n")

# Display the density checks
if (abs(integral_f-1) < 1e-6) {
  cat("f(x) is a probability density function.\n")
} else {
  cat("f(x) is not a probability density function.\n")
}

if (abs(integral_g-1) < 1e-6) {
  cat("g(t) is a probability density function.\n")
} else {
  cat("g(t) is not a probability density function.\n")
}

# Plot survival and density functions
par(mfrow=c(2,2))

curve(S(x),from=0,to=50,
      xlab="x",ylab="S(x)",
      main="Survival Function S(x)",lwd=2)

curve(f(x),from=0,to=50,
      xlab="x",ylab="f(x)",
      main="Density Function f(x)",lwd=2)

curve(g(x),from=0,to=30,
      xlab="t",ylab="g(t)",
      main="Future Lifetime Density g(t)",lwd=2)

curve(mu(x),from=0,to=50,
      xlab="x",ylab="mu(x)",
      main="Force of Mortality",lwd=2)

par(mfrow=c(1,1))

# RESULT AND CONCLUSION:
#
# The survival function is:
#
# S(x) = exp(-0.01*x),                  0 <= x < 15
#        exp[-0.15-0.02(x-15)],        15 <= x < 25
#        exp[-0.35-0.03(x-25)],        x >= 25
#
# The density function is:
#
# f(x) = 0.01*exp(-0.01*x),             0 <= x < 15
#        0.02*exp[-0.15-0.02(x-15)],   15 <= x < 25
#        0.03*exp[-0.35-0.03(x-25)],   x >= 25
#
# The density function of T(20) is:
#
# g(t) = 0.02*exp(-0.02*t),             0 <= t < 5
#        0.03*exp[-0.10-0.03(t-5)],   t >= 5
#
# The integrals of both f(x) and g(t) are equal to 1.
# Hence, both functions are valid probability density
# functions.
#
# Numerically:
# Integral f(x) dx = 1
# Integral g(t) dt = 1
# Also, S(20) = exp(-0.25) = approximately 0.7788008.

# INTERPRETATION:
#
# The survival function S(x) gives the probability that
# an individual survives beyond age x. It decreases as
# age increases because the probability of remaining alive
# becomes smaller.
#
# The density f(x) describes the distribution of the
# individual's age at death. It is obtained by multiplying
# the force of mortality by the probability of surviving
# to that age.
#
# For a person who has already reached age 20, the future
# lifetime density g(t) uses the mortality rate applicable
# at the attained age 20+t. The force of mortality is 0.02
# for the first five future years and 0.03 thereafter.
#
# Since both density functions are non-negative and each
# integrates to 1, both f(x) and g(t) satisfy the conditions
# required of probability density functions.
#
# The density functions have a jump at x=15 and x=25
# (and at t=5 for g), because the force of mortality changes
# at those points. This does not affect their validity as
# density functions.

############################################################
#                    END OF PRACTICAL 6
############################################################


























































############################################################
#                         PRACTICAL 7
#             INCREASE IN LIFE EXPECTANCY OF A TV
############################################################

# AIM:
# To calculate the increase in life expectancy at time 0
# for a new type of TV using the given survival functions.

# PROBLEM:
#
# The life of a TV set is specified by the survival function:
#
#              1,              x < 0
# S(x) =       1 - x/omega,    0 <= x <= omega
#              0,              x > omega
#
# It is given that the complete life expectancy at time 0
# is e-circle_0 = 10.
#
# For a proposed new type of TV, with the same omega,
# the new survival function is:
#
#              1,                    0 <= x <= 4
# S*(x) =      (omega-x)/(omega-4),  4 < x <= omega
#
# Calculate the increase in life expectancy at time 0.

# THEORY:
#
# 1. SURVIVAL FUNCTION
#
# The survival function S(x) gives the probability that
# the lifetime T of an item exceeds x:
#
#       S(x) = P(T>x)
#
# It describes the probability that the item continues
# to function beyond a specified time x.
#
# 2. LIFE EXPECTANCY AT TIME 0
#
# The life expectancy at time 0 is the expected lifetime
# of a newly installed item. In actuarial notation, this
# is represented by e-circle_0.
#
# For a non-negative continuous lifetime random variable:
#
#       e-circle_0 = E(T) = Integral_0^infinity S(x) dx
#
# Since the given survival function becomes zero after
# x=omega, the integral is evaluated from 0 to omega.
#
# 3. FINDING omega FROM THE GIVEN LIFE EXPECTANCY
#
# The original survival function is:
#
#       S(x) = 1-x/omega,  0 <= x <= omega
#
# Therefore:
#
#       e-circle_0 = Integral_0^omega (1-x/omega) dx
#
#                  = [x-x^2/(2*omega)]_0^omega
#
#                  = omega-omega/2
#
#                  = omega/2
#
# Given:
#
#       e-circle_0 = 10
#
# Hence:
#
#       omega/2 = 10
#
#       omega = 20
#
# 4. LIFE EXPECTANCY OF THE ORIGINAL TV
#
# The original life expectancy is given as:
#
#       E(T) = e-circle_0 = 10 time units.
#
# 5. LIFE EXPECTANCY OF THE NEW TV
#
# The new survival function is:
#
#       S*(x) = 1,                    0 <= x <= 4
#               (omega-x)/(omega-4),  4 < x <= omega
#
# Its life expectancy is:
#
#       E(T*) = Integral_0^omega S*(x) dx
#
#              = Integral_0^4 1 dx
#                + Integral_4^omega (omega-x)/(omega-4) dx
#
# The first integral is:
#
#       Integral_0^4 1 dx = 4
#
# The second integral is:
#
#       Integral_4^omega (omega-x)/(omega-4) dx
#       = (omega-4)/2
#
# Therefore:
#
#       E(T*) = 4+(omega-4)/2
#
# Substituting omega=20:
#
#       E(T*) = 4+(20-4)/2
#             = 4+8
#             = 12 time units
#
# 6. INCREASE IN LIFE EXPECTANCY
#
# Increase in life expectancy:
#
#       = E(T*)-E(T)
#
#       = 12-10
#
#       = 2 time units
#
# Therefore, the new TV increases expected lifetime
# by 2 time units.

# R FUNCTIONS USED:
#
# integrate() : calculates the area under a function.
# curve()     : plots a mathematical function.
# cat()       : displays the results.

# CODE AND OUTPUT:

# Given complete life expectancy at time 0
e0 <- 10

# Find omega using e0 = omega/2
omega <- 2*e0

# Original survival function
S <- function(x) {
  ifelse(x < 0, 1,
         ifelse(x <= omega, 1-x/omega, 0))
}

# New survival function
S_new <- function(x) {
  ifelse(x < 0, 1,
         ifelse(x <= 4, 1,
                ifelse(x <= omega,
                       (omega-x)/(omega-4), 0)))
}

# Life expectancy of the original TV
life_old <- integrate(S,lower=0,upper=omega)$value

# Life expectancy of the new TV
life_new <- integrate(S_new,lower=0,upper=omega)$value

# Increase in life expectancy
increase <- life_new-life_old

# Display results
cat("Given e-circle_0 =",e0,"\n")
cat("Calculated omega =",omega,"\n")
cat("Life expectancy of original TV =",life_old,"\n")
cat("Life expectancy of new TV =",life_new,"\n")
cat("Increase in life expectancy =",increase,"\n")

# Plot both survival functions
curve(S(x),from=0,to=omega,
      xlab="Time (x)",ylab="Survival Probability",
      main="Survival Functions of TV Sets",
      lwd=2)

curve(S_new(x),from=0,to=omega,
      add=TRUE,lwd=2,lty=2)

legend("topright",
       legend=c("Original TV","New TV"),
       lty=c(1,2),lwd=2)

# RESULT AND CONCLUSION:
#
# From the given complete life expectancy:
#
#       e-circle_0 = omega/2 = 10
#
# Therefore:
#
#       omega = 20
#
# The life expectancy of the original TV is 10 time units.
#
# The life expectancy of the new TV is 12 time units.
#
# Hence, the increase in life expectancy is:
#
#       12-10 = 2 time units.
#
# Therefore, the proposed new TV increases expected
# lifetime by 2 time units.

# INTERPRETATION:
#
# The original TV has an expected lifetime of 10 time units,
# as given in the question. The value omega=20 is obtained
# from the original survival function and the given
# actuarial life expectancy.
#
# The new TV has a survival probability equal to 1 for the
# first 4 time units. This means that, according to the
# model, it is certain to survive during this initial
# period. After 4 time units, its survival probability
# decreases linearly to zero at time omega=20.
#
# The area under the new survival curve is 12, compared
# with 10 for the original TV. Since life expectancy is
# the area under the survival function, the new TV has an
# expected lifetime that is 2 time units greater.
#
# Thus, the proposed design improves the expected lifetime
# by 2 time units, which is a 20% increase over the
# original expected lifetime of 10 time units.

############################################################
#                    END OF PRACTICAL 7
############################################################

































































############################################################
#                         PRACTICAL 8
#       PROBABILITIES USING A GIVEN SURVIVAL FUNCTION
############################################################

# AIM:
# To calculate different conditional probabilities of death
# using the given survival function.

# PROBLEM:
#
# The given survival function is:
#
#       S(x) = exp{-0.00133[(1.07152)^x - 1]}
#
# where 0 <= x <= 100.
#
# Using the given survival function, calculate:
#
# (a) The probability that a person aged 40 will die
#     in the next decade.
#
# (b) The probability that a person aged 30 will die
#     in his 60th year of age.
#
# (c) The probability that a person aged 40 will die
#     between his 50th and 60th birthday.
#
# (d) The probability that a person aged 40 will die
#     either between ages 45 and 50 or between ages 60
#     and 70.

# THEORY:
#
# 1. SURVIVAL FUNCTION
#
# The survival function S(x) gives the probability that
# a person survives beyond age x.
#
#       S(x) = P(X>x)
#
# Consequently, the probability that a person dies between
# ages x and y, where y>x, is:
#
#       P(x<X<=y) = S(x)-S(y)
#
# This is because S(x) is the probability of surviving
# beyond age x, while S(y) is the probability of surviving
# beyond age y.
#
# 2. CONDITIONAL PROBABILITY FOR A PERSON ALREADY AGED x
#
# If a person has already survived to age x, the probability
# that the person dies before age y is:
#
#       P(x<X<=y | X>x)
#
#       = [S(x)-S(y)]/S(x)
#
#       = 1-S(y)/S(x)
#
# This formula is used because the person is known to have
# survived up to age x.
#
# 3. PART (a): PERSON AGED 40 DIES IN THE NEXT DECADE
#
# The next decade for a person aged 40 is the interval
# from age 40 to age 50.
#
# Required probability:
#
#       P(40<X<=50 | X>40)
#
#       = [S(40)-S(50)]/S(40)
#
#       = 1-S(50)/S(40)
#
# 4. PART (b): PERSON AGED 30 DIES IN HIS 60TH YEAR
#
# The 60th year of age is the period from the 59th birthday
# to the 60th birthday. Therefore, the required interval
# is from age 59 to age 60.
#
# Since the person is already aged 30, the probability is:
#
#       P(59<X<=60 | X>30)
#
#       = [S(59)-S(60)]/S(30)
#
# Note: The denominator is S(30), because the person is
# known to have survived to age 30.
#
# 5. PART (c): PERSON AGED 40 DIES BETWEEN 50TH AND
#    60TH BIRTHDAY
#
# The required probability is:
#
#       P(50<X<=60 | X>40)
#
#       = [S(50)-S(60)]/S(40)
#
# 6. PART (d): PERSON AGED 40 DIES IN EITHER OF TWO
#    AGE INTERVALS
#
# The two possible intervals are:
#       (45,50] and (60,70]
#
# These intervals are disjoint. Therefore, their
# probabilities can be added.
#
# Required probability:
#
#       P((45<X<=50) or (60<X<=70) | X>40)
#
#       = {S(45)-S(50) + S(60)-S(70)}/S(40)
#
# 7. SUBSTITUTION OF THE GIVEN SURVIVAL FUNCTION
#
# For every required age x, calculate:
#
#       S(x) = exp{-0.00133[(1.07152)^x - 1]}
#
# Then substitute the corresponding survival probabilities
# into the formulas derived above.

# R FUNCTIONS USED:
#
# function() : defines the survival function.
# exp()      : calculates the exponential function.
# ^          : raises a number to a power.
# cat()      : displays the calculated probabilities.

# CODE AND OUTPUT:

# Define the given survival function
S <- function(x) {
  exp(-0.00133*((1.07152)^x-1))
}

# (a) Person aged 40 dies in the next decade
p_a <- (S(40)-S(50))/S(40)

# (b) Person aged 30 dies in the 60th year of age
# 60th year means from age 59 to age 60
p_b <- (S(59)-S(60))/S(30)

# (c) Person aged 40 dies between 50th and 60th birthday
p_c <- (S(50)-S(60))/S(40)

# (d) Person aged 40 dies between 45-50 or 60-70
p_d <- ((S(45)-S(50))+(S(60)-S(70)))/S(40)

# Display results
cat("Part (a) =",p_a,"\n")
cat("Part (b) =",p_b,"\n")
cat("Part (c) =",p_c,"\n")
cat("Part (d) =",p_d,"\n")

# Display results as percentages
cat("\nProbabilities in percentage:\n")
cat("Part (a) =",p_a*100,"%\n")
cat("Part (b) =",p_b*100,"%\n")
cat("Part (c) =",p_c*100,"%\n")
cat("Part (d) =",p_d*100,"%\n")

# RESULT AND CONCLUSION:
#
# (a) Probability that a person aged 40 dies in the
#     next decade:
#     0.0207615, approximately 2.0761%.
#
# (b) Probability that a person aged 30 dies in the
#     60th year of age (age 59 to 60):
#     0.0052198, approximately 0.5220%.
#
# (c) Probability that a person aged 40 dies between
#     the 50th and 60th birthday:
#     0.0401457, approximately 4.0146%.
#
# (d) Probability that a person aged 40 dies either
#     between ages 45 and 50 or between ages 60 and 70:
#     0.0873535, approximately 8.7353%.
#
# Therefore, all four required probabilities have been
# calculated using the conditional survival probability
# formula.

# INTERPRETATION:
#
# Part (a): A person who has reached age 40 has approximately
# a 2.08% probability of dying before reaching age 50.
#
# Part (b): A person who has reached age 30 has approximately
# a 0.52% probability of dying during the 60th year of age,
# that is, between the 59th and 60th birthdays.
#
# Part (c): A person aged 40 has approximately a 4.01%
# probability of dying between ages 50 and 60.
#
# Part (d): A person aged 40 has approximately an 8.74%
# probability of dying in either of the two specified
# intervals: ages 45 to 50 or ages 60 to 70.
#
# In every part, the probability is conditional on the
# person having survived to the age stated in the question.
# Therefore, the denominator is the survival probability
# at that person's current age.

############################################################
#                    END OF PRACTICAL 8
############################################################








































############################################################
#                         PRACTICAL 9
#        CONTINUOUS TERM INSURANCE AND NET SINGLE PREMIUM
############################################################

# AIM:
# To calculate the actuarial present value of a 5-year
# continuous term insurance and the net single premium for
# an n-year term insurance using R programming. To study
# the effect of changes in n and force of interest delta.

# PROBLEM:
#
# The force of mortality is:
#
#       mu_s = 0.04,  0 <= s < 15
#              0.08, 15 <= s < 25
#              0.12, 25 <= s < 35
#              0.18, s >= 35
#
# (i) Find A-bar^(1)_(30:5), taking delta = 0.06.
#
# (ii) Find the net single premium for an n-year continuous
#      term insurance for a benefit of 1000, payable at the
#      moment of death, for n = 1,...,10 and
#      delta = 0.05, 0.06, 0.07, 0.08, 0.09, 0.10.
#
# Comment on the changes in values as n and delta change.

# THEORY:
#
# 1. FORCE OF MORTALITY
#
# The force of mortality mu_x is the instantaneous rate
# of death at age x, given that the person has survived
# to that age.
#
# For a life aged 30, the relevant mortality rates are:
#       From age 30 to 35: mu = 0.12
#       From age 35 onwards: mu = 0.18
#
# Thus, measured from age 30:
#       mu_(30+t) = 0.12, 0 <= t < 5
#                   0.18, t >= 5
#
# The survival probability is:
#       _t p_30 = exp(-0.12*t),                 0 <= t <= 5
#       _t p_30 = exp[-0.12*5-0.18*(t-5)],      t > 5
#
# 2. CONTINUOUS TERM INSURANCE
#
# A continuous term insurance pays the sum assured
# immediately at the moment of death, provided death
# occurs during the policy term.
#
# For benefit 1, term n and force of interest delta:
#
#       A-bar^(1)_(30:n)
#       = integral from 0 to n of
#         exp(-delta*t) * _t p_30 * mu_(30+t) dt
#
# For a benefit B, the net single premium is:
#
#       NSP = B * A-bar^(1)_(30:n)
#
# 3. PART (i): FIVE-YEAR TERM INSURANCE
#
# During the first five years, the force of mortality
# is constant at 0.12. Therefore:
#
#       A-bar^(1)_(30:5)
#       = integral_0^5 exp(-delta*t)*exp(-0.12*t)*0.12 dt
#
#       = 0.12/(0.12+delta)
#           * [1-exp(-5*(0.12+delta))]
#
# Taking delta = 0.06:
#
#       A-bar^(1)_(30:5)
#       = 0.12/0.18 * [1-exp(-0.18*5)]
#       = 0.3956202 (approximately)
#
# 4. PART (ii): NET SINGLE PREMIUM
#
# For n <= 5, the mortality rate remains 0.12 throughout:
#
#       NSP = B * 0.12/(0.12+delta)
#                   * [1-exp(-(0.12+delta)*n)]
#
# For n > 5, split the integral at t=5.
#
# First five years:
#
#       0.12/(0.12+delta)
#       * [1-exp(-5*(0.12+delta))]
#
# After five years:
#
#       exp(-0.6-5*delta) * 0.18/(0.18+delta)
#       * [1-exp(-(0.18+delta)*(n-5))]
#
# Hence, for n > 5:
#
# NSP = B * {
#       0.12/(0.12+delta) *
#       [1-exp(-5*(0.12+delta))]
#       +
#       exp(-0.6-5*delta) * 0.18/(0.18+delta) *
#       [1-exp(-(0.18+delta)*(n-5))]
#       }
#
# The factor exp(-0.6-5*delta) accounts for survival
# through the first five years and discounting to year 5.

# CODE AND OUTPUT:

# PART (i): 5-Year Continuous Term Insurance
mu <- 0.12
delta <- 0.06
n <- 5

A <- (mu/(mu+delta)) *
  (1-exp(-(mu+delta)*n))

cat("Actuarial Present Value =",A,"\n")

# PART (ii): Net Single Premium
Benefit <- 1000
n_values <- 1:10
delta_values <- seq(0.05,0.10,by=0.01)

# Function to calculate NSP
NSP <- function(n,delta) {
  
  if(n <= 5) {
    
    premium <- Benefit *
      (0.12/(0.12+delta)) *
      (1-exp(-(0.12+delta)*n))
    
  } else {
    
    premium <- Benefit * (
      (0.12/(0.12+delta)) *
        (1-exp(-5*(0.12+delta))) +
        
        exp(-0.6-5*delta) *
        (0.18/(0.18+delta)) *
        (1-exp(-(0.18+delta)*(n-5)))
    )
  }
  
  return(premium)
}

# Create result matrix and calculate premiums
results <- matrix(0,nrow=length(n_values),
                  ncol=length(delta_values))

for(i in 1:length(n_values)) {
  for(j in 1:length(delta_values)) {
    results[i,j] <- NSP(n_values[i],delta_values[j])
  }
}

# Display results
results <- as.data.frame(round(results,2))
results <- cbind(n=n_values,results)
colnames(results) <- c("n",paste0("delta_",delta_values))

cat("\nNet Single Premium Table:\n")
print(results)


# Plot net single premium against policy term for each delta
matplot(n_values, as.matrix(results[,-1]),
        type="b", pch=1:6, lty=1:6, lwd=2,
        xlab="Policy term (n)",
        ylab="Net Single Premium",
        main="Continuous Term Insurance: Net Single Premium")

legend("topleft",
       legend=paste("delta =", delta_values),
       col=1:6, pch=1:6, lty=1:6,
       cex=0.8)

# RESULT AND CONCLUSION:
#
# The actuarial present value of the 5-year continuous
# term insurance for a life aged 30, with delta=0.06, is:
#
#       A-bar^(1)_(30:5) = 0.3956202
#
# The R program also calculates the net single premiums
# for all specified combinations of n and delta.

# INTERPRETATION:
#
# 1. Effect of n:
# The net single premium increases as the term n increases,
# because a longer term provides insurance protection for
# a longer period and increases the chance of a claim.
#
# 2. Effect of delta:
# For a fixed term, the net single premium decreases as
# delta increases because future death benefits are
# discounted more heavily.
#
# 3. Effect of mortality:
# The force of mortality increases from 0.12 to 0.18 after
# the first five years. This increases the chance of death
# during the later part of a policy lasting more than five
# years.
#
# Therefore, the net single premium increases with n and
# decreases with delta.

############################################################
#                    END OF PRACTICAL 9
############################################################






























############################################################
#                         PRACTICAL 10
#                    ENDOWMENT INSURANCE
############################################################

# AIM:
# To calculate the net single premium for an n-year
# continuous endowment insurance with a benefit of 1000
# using R programming, and to study the effect of changes
# in n and force of interest delta.

# PROBLEM:
#
# The force of mortality is:
#
#       mu_s = 0.04,  0 <= s < 15
#              0.08, 15 <= s < 25
#              0.12, 25 <= s < 35
#              0.18, s >= 35
#
# Find the net single premium for an n-year endowment
# insurance for a benefit of 1000, for n = 1,...,10 and
# delta = 0.05, 0.06, 0.07, 0.08, 0.09, 0.10.
#
# Comment on the changes in values as n and delta change.

# ASSUMPTION:
#
# As in Practical 9, the calculations are for a life aged
# 30 at entry. Therefore, measured from the start of the
# policy:
#       mu = 0.12 during the first five years
#       mu = 0.18 after the first five years.
#
# This is consistent with the age-30 setup used in the
# accompanying Practical 9 work.

# THEORY:
#
# 1. ENDOWMENT INSURANCE
#
# An n-year endowment insurance pays the sum assured B:
# (i) immediately at the moment of death if death occurs
#     during the policy term; or
# (ii) at the end of n years if the policyholder survives
#      the entire term.
#
# Thus, the net single premium is the sum of:
#       (a) EPV of the death benefit
#       (b) EPV of the survival (maturity) benefit
#
# 2. SURVIVAL PROBABILITY FOR A LIFE AGED 30
#
# The force of mortality is:
#       mu = 0.12, 0 <= t < 5
#       mu = 0.18, t >= 5
#
# Hence:
#       _t p_30 = exp(-0.12*t),              0 <= t <= 5
#       _t p_30 = exp[-0.6-0.18*(t-5)],      t > 5
#
# 3. PRESENT VALUE OF THE DEATH BENEFIT
#
# The death benefit is paid at the moment of death.
# Its expected present value for benefit B is:
#
#       B * integral_0^n exp(-delta*t) *
#                      _t p_30 * mu_(30+t) dt
#
# For n <= 5:
#
#       Death EPV = B * 0.12/(0.12+delta)
#                         * [1-exp(-(0.12+delta)*n)]
#
# For n > 5, split the integral at t=5:
#
#       Death EPV = B * {
#          0.12/(0.12+delta) *
#          [1-exp(-5*(0.12+delta))]
#          +
#          exp(-0.6-5*delta) * 0.18/(0.18+delta) *
#          [1-exp(-(0.18+delta)*(n-5))]
#       }
#
# 4. PRESENT VALUE OF THE MATURITY BENEFIT
#
# If the life survives to time n, benefit B is paid at
# the end of the policy term. Therefore:
#
#       Survival EPV = B * exp(-delta*n) * _n p_30
#
# For n <= 5:
#
#       Survival EPV = B * exp(-(0.12+delta)*n)
#
# For n > 5:
#
#       Survival EPV = B * exp[-0.6-0.18*(n-5)-delta*n]
#
# 5. NET SINGLE PREMIUM
#
# NSP = Death EPV + Survival EPV
#
# For n <= 5:
#
# NSP = B * {
#       0.12/(0.12+delta) *
#       [1-exp(-(0.12+delta)*n)]
#       + exp(-(0.12+delta)*n)
#       }
#
# For n > 5:
#
# NSP = B * {
#       0.12/(0.12+delta) *
#       [1-exp(-5*(0.12+delta))]
#       +
#       exp(-0.6-5*delta) * 0.18/(0.18+delta) *
#       [1-exp(-(0.18+delta)*(n-5))]
#       +
#       exp[-0.6-0.18*(n-5)-delta*n]
#       }
#
# The last term is the discounted probability of surviving
# the entire policy term and receiving the maturity benefit.

# CODE AND OUTPUT:

# Given values
Benefit <- 1000
n_values <- 1:10
delta_values <- seq(0.05,0.10,by=0.01)

# Function to calculate NSP
NSP <- function(n,delta) {
  
  if(n <= 5) {
    
    death_benefit <-
      (0.12/(0.12+delta)) *
      (1-exp(-(0.12+delta)*n))
    
    survival_benefit <-
      exp(-(0.12+delta)*n)
    
  } else {
    
    death_benefit <-
      (0.12/(0.12+delta)) *
      (1-exp(-5*(0.12+delta))) +
      
      exp(-0.6-5*delta) *
      (0.18/(0.18+delta)) *
      (1-exp(-(0.18+delta)*(n-5)))
    
    survival_benefit <-
      exp(-0.6-0.18*(n-5)-delta*n)
  }
  
  premium <- Benefit *
    (death_benefit+survival_benefit)
  
  return(premium)
}

# Create result matrix
results <- matrix(0,nrow=length(n_values),
                  ncol=length(delta_values))

# Calculate premiums
for(i in 1:length(n_values)) {
  for(j in 1:length(delta_values)) {
    results[i,j] <- NSP(n_values[i],delta_values[j])
  }
}

# Display results
results <- as.data.frame(round(results,2))
results <- cbind(n=n_values,results)
colnames(results) <- c("n",paste0("delta_",delta_values))

cat("Net Single Premium for Endowment Insurance:\n")
print(results)


# Plot net single premium against policy term for each delta
matplot(n_values, as.matrix(results[,-1]),
        type="b", pch=1:6, lty=1:6, lwd=2,
        xlab="Policy term (n)",
        ylab="Net Single Premium",
        main="Endowment Insurance: Net Single Premium")

legend("topright",
       legend=paste("delta =", delta_values),
       col=1:6, pch=1:6, lty=1:6,
       cex=0.8)

# RESULT AND CONCLUSION:
#
# The net single premiums for all specified combinations
# of n and delta are displayed in the R output table.
# Each premium includes both the death benefit and the
# maturity benefit.

# INTERPRETATION:
#
# 1. Effect of n:
# The net single premium decreases as n increases. The
# maturity payment is made later for a longer policy term,
# and its present value decreases. The death-benefit
# component increases as the term is extended, but the
# combined endowment premium decreases for the given
# assumptions.
#
# 2. Effect of delta:
# For a fixed n, the net single premium decreases as delta
# increases because both death and maturity benefits are
# discounted more heavily.
#
# 3. Effect of mortality:
# The force of mortality increases from 0.12 to 0.18 after
# the first five years. This affects the death-benefit
# component for terms longer than five years and reduces
# the probability of receiving the maturity benefit.
#
# Therefore, for the specified values, the net single
# premium decreases as n and delta increase.

############################################################
#                    END OF PRACTICAL 10
############################################################




































































############################################################
#                         PRACTICAL 11
#       M-THLY WHOLE LIFE ANNUITY-DUE AND ANNUITY-IMMEDIATE
############################################################

# AIM:
# To calculate the actuarial present values of m-thly
# whole life annuity-due and annuity-immediate contracts
# for lives aged 25 and 30 under Gompertz's law of mortality,
# using the uniform distribution of deaths (UDD) assumption
# for fractional ages.

# PROBLEM:
#
# Suppose the force of mortality follows Gompertz's law:
#
#             mu_x = B*C^x
#
# Find the actuarial present values of m-thly whole life
# annuity-due and annuity-immediate for an annual payment
# amount of 100 issued to lives aged 25 and 30, for:
#
#       m = 2, 4, 12
#
# Take:
#       delta = 0.05
#       B = 0.0001151
#       C = 1.096
#
# Use the uniform distribution of deaths assumption
# for fractional ages.
#
# ASSUMPTION:
# The annual annuity amount is 100, paid in m equal
# instalments of 100/m each year.

# THEORY:
#
# 1. GOMPERTZ'S LAW OF MORTALITY
#
# Under Gompertz's law, the force of mortality at age x is:
#
#       mu_x = B*C^x
#
# where B and C are positive constants and C > 1.
# In this question:
#       B = 0.0001151 and C = 1.096.
#
# 2. ONE-YEAR SURVIVAL PROBABILITY
#
# The probability that a life aged x survives one year is:
#
#       p_x = exp[- integral_x^(x+1) mu_s ds]
#
# Substituting mu_s = B*C^s:
#
#       integral_x^(x+1) B*C^s ds
#       = B*C^x*(C-1)/ln(C)
#
# Therefore:
#
#       p_x = exp[-B*C^x*(C-1)/ln(C)]
#
# and:
#
#       q_x = 1-p_x
#
# 3. UNIFORM DISTRIBUTION OF DEATHS (UDD)
#
# Under UDD, deaths are assumed to be uniformly distributed
# throughout each year of age.
#
# For 0 <= r <= 1:
#
#       _r p_x = 1-r*q_x
#
# Thus, for a fractional duration r within the year:
#
#       _r p_x = 1-r*q_x
#
# For a duration j+r, where j is an integer and 0<=r<1:
#
#       _(j+r)p_x = _j p_x * (1-r*q_(x+j))
#
# where:
#
#       _j p_x = product from h=0 to j-1 of p_(x+h)
#
# 4. DISCOUNT FUNCTION
#
# The force of interest is delta=0.05.
# The discount factor for a payment made at time t is:
#
#       v^t = exp(-delta*t)
#
# 5. M-THLY WHOLE LIFE ANNUITY-DUE
#
# An m-thly annuity-due pays 100/m at the beginning of
# each m-th of a year, provided the life is alive.
#
# Payments are made at times:
#
#       t = j + k/m,  k=0,1,...,m-1; j=0,1,2,...
#
# Its actuarial present value is:
#
#       a-double-dot_x^(m)
#       = (100/m) * sum over j>=0 and k=0,...,m-1 of
#         exp[-delta*(j+k/m)] * _(j+k/m)p_x
#
# 6. M-THLY WHOLE LIFE ANNUITY-IMMEDIATE
#
# An m-thly annuity-immediate pays 100/m at the end of
# each m-th of a year, provided the life is alive.
#
# Payments are made at times:
#
#       t = j + k/m,  k=1,2,...,m; j=0,1,2,...
#
# Its actuarial present value is:
#
#       a_x^(m)
#       = (100/m) * sum over j>=0 and k=1,...,m of
#         exp[-delta*(j+k/m)] * _(j+k/m)p_x
#
# The sums are evaluated numerically until the remaining
# survival probability becomes negligible.

# R FUNCTIONS USED:
#
# exp()    : exponential function.
# log()    : natural logarithm.
# for()    : repeats calculations over ages and payment
#            frequencies.
# data.frame(): stores the calculated results.
# matplot(): plots the annuity values.
# legend() : adds a legend to the graph.
# cat()    : displays results.

# CODE AND OUTPUT:

# Given parameters
B <- 0.0001151
C <- 1.096
delta <- 0.05
payment <- 100

# Calculate one-year probability of death q_x
qx <- function(x) {
  1-exp(-B*(C^x)*(C-1)/log(C))
}

# Calculate the m-thly whole life annuity-due
annuity_due <- function(age,m) {
  
  value <- 0
  survival <- 1
  
  for(j in 0:200) {
    
    q <- qx(age+j)
    
    for(k in 0:(m-1)) {
      
      r <- k/m
      prob_survival <- survival*(1-r*q)
      time <- j+r
      
      value <- value +
        (payment/m)*exp(-delta*time)*prob_survival
    }
    
    survival <- survival*(1-q)
    
    if(survival < 1e-12) break
  }
  
  return(value)
}

# Calculate the m-thly whole life annuity-immediate
annuity_immediate <- function(age,m) {
  
  value <- 0
  survival <- 1
  
  for(j in 0:200) {
    
    q <- qx(age+j)
    
    for(k in 1:m) {
      
      r <- k/m
      prob_survival <- survival*(1-r*q)
      time <- j+r
      
      value <- value +
        (payment/m)*exp(-delta*time)*prob_survival
    }
    
    survival <- survival*(1-q)
    
    if(survival < 1e-12) break
  }
  
  return(value)
}

# Required ages and payment frequencies
ages <- c(25,30)
m_values <- c(2,4,12)

# Calculate all actuarial present values
results <- data.frame()

for(age in ages) {
  for(m in m_values) {
    
    due <- annuity_due(age,m)
    immediate <- annuity_immediate(age,m)
    
    results <- rbind(results,
                     data.frame(Age=age,m=m,
                                Annuity_Due=due,
                                Annuity_Immediate=immediate))
  }
}

# Display results
cat("Actuarial Present Values:\n")
print(results)

# Plot annuity-due and annuity-immediate values
# for each age
par(mfrow=c(1,2))

for(age in ages) {
  
  age_results <- results[results$Age==age,]
  
  plot(m_values,age_results$Annuity_Due,
       type="b",pch=16,lwd=2,
       ylim=range(age_results$Annuity_Due,
                  age_results$Annuity_Immediate),
       xlab="Payment frequency (m)",
       ylab="Actuarial Present Value",
       main=paste("Whole Life Annuities: Age",age))
  
  lines(m_values,age_results$Annuity_Immediate,
        type="b",pch=17,lwd=2,lty=2)
  
  legend("topright",
         legend=c("Annuity-due","Annuity-immediate"),
         pch=c(16,17),lty=c(1,2),lwd=2,
         cex=0.8)
}

par(mfrow=c(1,1))

# RESULT AND CONCLUSION:
#
# The R program calculates the actuarial present values
# of m-thly whole life annuity-due and annuity-immediate
# for ages 25 and 30, for m=2, 4 and 12.
#
# The results are displayed in the table above.

# INTERPRETATION:
#
# 1. Annuity-due versus annuity-immediate:
# The annuity-due has a higher actuarial present value
# because each instalment is paid at the beginning of
# the payment period, whereas an annuity-immediate pays
# at the end. The first annuity-due payment is immediate
# at time 0.
#
# 2. Effect of payment frequency m:
# As m increases, payments are made more frequently and
# each instalment becomes smaller. The annuity-due value
# decreases towards the corresponding continuous-payment
# limit, while the annuity-immediate value increases
# towards that same limit.
#
# 3. Effect of age:
# The actuarial present values for age 30 are lower than
# those for age 25 because the older life has a higher
# force of mortality under Gompertz's law and is expected
# to receive fewer future payments.
#
# 4. Fractional ages:
# The UDD assumption is used to calculate survival
# probabilities between integer ages.
#
# Thus, payment timing, payment frequency and the age
# of the life all affect the actuarial present value.

############################################################
#                    END OF PRACTICAL 11
############################################################



















































############################################################
#                         PRACTICAL 12
#       MONTHLY WHOLE LIFE ANNUITY-DUE AND IMMEDIATE
############################################################

# AIM:
# To calculate the actuarial present value of whole-life
# annuity-due and annuity-immediate payable monthly for
# ages 40 to 49, using the given specific mortality rates
# and the UDD assumption for fractional ages.

# PROBLEM:
#
# The specific mortality probabilities q_x are given for
# ages 40 to 49. The annual effective rate of interest is
# i = 0.05 and the annual whole-life assurance value at
# age 50 is A_50 = 0.251.
#
# Find the actuarial present values of whole-life annuity-
# due and annuity-immediate payable monthly, with a monthly
# payment of Rs. 1000, for ages 40 to 49.
#
# The given mortality probabilities are:
#
# Age (x):  40     41     42     43     44
# q_x:     0.0027 0.0029 0.0032 0.0034 0.0037
#
# Age (x):  45     46     47     48     49
# q_x:     0.0039 0.0043 0.0046 0.0050 0.0054
#
# Use the Uniform Distribution of Deaths (UDD) assumption
# for fractional ages.

# THEORY:
#
# 1. ONE-YEAR SURVIVAL PROBABILITY
#
# The probability of surviving one year from age x is:
#
#       p_x = 1 - q_x
#
# where q_x is the probability of death between ages x
# and x+1.
#
# 2. ANNUAL WHOLE-LIFE ASSURANCE
#
# The annual whole-life assurance value is calculated
# recursively:
#
#       A_x = v(q_x + p_x*A_(x+1))
#
# where:
#
#       v = 1/(1+i)
#
# The recursion is started with the given value A_50=0.251
# and calculated backwards for ages 49 to 40.
#
# 3. ANNUAL WHOLE-LIFE ANNUITY-DUE
#
# The annual whole-life annuity-due is:
#
#       ä_x = (1-A_x)/d
#
# where:
#
#       d = i/(1+i)
#
# The annual whole-life annuity-immediate is:
#
#       a_x = ä_x - 1
#
# 4. UDD ASSUMPTION FOR FRACTIONAL AGES
#
# Under the Uniform Distribution of Deaths assumption,
# deaths are uniformly distributed throughout each year.
# This assumption is used to convert the annual annuity
# values into m-thly annuity values.
#
# 5. M-THLY ANNUITY-DUE
#
# For payments m times per year, define:
#
#       i^(m) = m[(1+i)^(1/m)-1]
#
#       d^(m) = m[1-(1+i)^(-1/m)]
#
#       alpha_m = i*d/[i^(m)*d^(m)]
#
#       beta_m = [i-i^(m)]/[i^(m)*d^(m)]
#
# Under UDD, the m-thly annuity-due is:
#
#       ä_x^(m) = alpha_m*ä_x - beta_m
#
# For monthly payments, m=12.
#
# 6. M-THLY ANNUITY-IMMEDIATE
#
# The m-thly annuity-immediate is:
#
#       a_x^(m) = ä_x^(m) - 1/m
#
# For monthly payments:
#
#       a_x^(12) = ä_x^(12) - 1/12
#
# 7. MONTHLY PAYMENT AMOUNT
#
# The monthly payment is Rs. 1000. Therefore, the annual
# payment rate is:
#
#       1000*12 = Rs. 12000
#
# Hence:
#
#       APV(Due)        = 12000*ä_x^(12)
#
#       APV(Immediate)  = 12000*a_x^(12)
#
# These formulae give the present values of the complete
# streams of monthly payments, conditional on being alive
# at the respective payment dates.

# R FUNCTIONS USED:
#
# 1:length() : creates the age index.
# for()      : calculates the assurance values recursively.
# data.frame(): stores the results in tabular form.
# round()    : rounds the results.
# matplot()  : plots both annuity values against age.
# legend()   : displays the graph legend.

# CODE AND OUTPUT:

# Given values
i <- 0.05
A50 <- 0.251
m <- 12
monthly_payment <- 1000
annual_payment <- monthly_payment*m

# Given qx values for ages 40 to 49
age <- 40:49
qx <- c(0.0027,0.0029,0.0032,0.0034,0.0037,
        0.0039,0.0043,0.0046,0.0050,0.0054)

# Calculate px
px <- 1-qx

# Discount and annual discount factors
v <- 1/(1+i)
d <- i/(1+i)

# Annual whole-life assurance values
Ax <- numeric(11)
names(Ax) <- 40:50
Ax["50"] <- A50

for(j in 10:1) {
  Ax[j] <- v*(qx[j]+px[j]*Ax[j+1])
}

# Annual whole-life annuity values
annuity_due <- (1-Ax[1:10])/d
annuity_immediate <- annuity_due-1

# UDD conversion factors for monthly payments
i_m <- m*((1+i)^(1/m)-1)
d_m <- m*(1-(1+i)^(-1/m))

alpha_m <- (i*d)/(i_m*d_m)
beta_m <- (i-i_m)/(i_m*d_m)

# Monthly annuity-due and annuity-immediate
annuity_due_m <- alpha_m*annuity_due-beta_m
annuity_immediate_m <- annuity_due_m-1/m

# Actuarial present values
APV_due <- annual_payment*annuity_due_m
APV_immediate <- annual_payment*annuity_immediate_m

# Prepare and display the result table
results <- data.frame(
  Age=age,
  qx=qx,
  Ax=Ax[1:10],
  Annual_Due=annuity_due,
  Annual_Immediate=annuity_immediate,
  Monthly_Due=annuity_due_m,
  Monthly_Immediate=annuity_immediate_m,
  APV_Due=APV_due,
  APV_Immediate=APV_immediate
)

cat("Actuarial Present Values of Monthly Whole-Life Annuities:\n")
print(round(results,4))

# Graph: Actuarial present values by age
matplot(age,cbind(APV_due,APV_immediate),
        type="b",pch=c(16,17),lty=c(1,2),lwd=2,
        xlab="Age",
        ylab="Actuarial Present Value (Rs.)",
        main="Monthly Whole-Life Annuities by Age")

legend("topright",
       legend=c("Annuity-Due","Annuity-Immediate"),
       pch=c(16,17),lty=c(1,2),lwd=2)

# RESULT AND CONCLUSION:
#
# The annual whole-life assurance values were first
# calculated recursively, starting from A_50=0.251.
# The annual annuity-due and annuity-immediate values
# were then obtained using the given formulae.
#
# The UDD conversion formula was applied to obtain the
# monthly annuity values. Finally, these values were
# multiplied by Rs. 12000 to obtain the actuarial present
# values for monthly payments of Rs. 1000.
#
# The complete numerical results are displayed in the
# output table and the graph.

# INTERPRETATION:
#
# 1. Annuity-due and annuity-immediate:
# The APV of the annuity-due is greater than that of the
# annuity-immediate because annuity-due payments are made
# at the beginning of each month, whereas annuity-
# immediate payments are made at the end of each month.
#
# 2. Effect of age:
# The APVs generally decrease as age increases. An older
# individual has higher mortality probabilities and is
# therefore expected to receive fewer future payments.
#
# 3. Monthly payment frequency:
# The UDD formula adjusts the annual annuity values to
# monthly payment frequency. The monthly annuity-due and
# immediate values differ by 1/12 in annuity units.
#
# Thus, the annuity-due has the higher APV, and the APVs
# decrease with increasing age for the given data.

############################################################
#                    END OF PRACTICAL 12
############################################################




















































############################################################
#                         PRACTICAL 13
#       CONTINUOUS INSURANCE AND ANNUITY PREMIUMS
############################################################

# AIM:
# To calculate the continuous level premium rates for
# different life insurance and annuity contracts under
# Gompertz's law of mortality, and compare the premiums
# for different ages, policy terms and premium-paying terms.

# PROBLEM:
#
# The force of mortality follows Gompertz's law:
#
#             mu_(x+t) = B*C^(x+t)
#
# where B = 0.0001151 and C = 1.096.
#
# The force of interest is delta = 0.05 and the benefit
# amount is 1000.
#
# 1. n-year endowment insurance, n=1,2,...,10 and
#    x=25,30; premiums paid for at most n years.
#
# 2. 5-year endowment insurance, x=25; premiums paid
#    for at most 3 years.
#
# 3. Whole-life insurance, x=25,30; premiums paid as
#    a whole-life continuous annuity.
#
# 4. Whole-life insurance, x=25; premiums paid for at
#    most 20 years.
#
# 5. 30-year deferred whole-life annuity, x=30;
#    premiums paid for at most 30 years.
#
# Compare the premium as age changes in (1) and (3).
# Compare premiums in (1) and (2), and compare premiums
# in (3) and (4).

# THEORY:
#
# 1. GOMPERTZ'S LAW OF MORTALITY
#
# Under Gompertz's law, the force of mortality at age y is:
#
#       mu_y = B*C^y
#
# Therefore, for a life aged x at issue, the force of
# mortality after t years is:
#
#       mu_(x+t) = B*C^(x+t)
#
# where B=0.0001151 and C=1.096.
#
# 2. SURVIVAL PROBABILITY
#
# The probability that a life aged x survives for a
# further t years is:
#
#       _t p_x = exp[- integral_0^t mu_(x+s) ds]
#
# Substituting Gompertz's law:
#
#       _t p_x = exp[- integral_0^t B*C^(x+s) ds]
#
# Integrating:
#
#       _t p_x = exp[-(B*C^x/ln(C))*(C^t-1)]
#
# This survival function is used in all five parts.
#
# 3. FORCE OF INTEREST AND DISCOUNT FUNCTION
#
# The force of interest is delta=0.05.
# The discount factor for a payment at time t is:
#
#       exp(-delta*t)
#
# 4. CONTINUOUS PREMIUMS
#
# In a fully continuous insurance, premiums are paid
# continuously while the life is alive and while the
# premium-paying period has not ended.
#
# If P is the continuous premium rate, its expected
# present value is:
#
#       P * integral_0^k exp(-delta*t)*_t p_x dt
#
# where k is the maximum premium-paying period.
#
# By the equivalence principle:
#
#       EPV(Premiums) = EPV(Benefits)
#
# Hence:
#
#       P = EPV(Benefits) / EPV(Premium annuity)
#
# 5. CONTINUOUS N-YEAR ENDOWMENT INSURANCE
#
# An n-year endowment insurance pays 1000:
# (i) at the moment of death if death occurs during
#     the n-year term; or
# (ii) at time n if the life survives to the end.
#
# EPV of benefits:
#
#       1000 * {
#         integral_0^n exp(-delta*t)*_t p_x*mu_(x+t) dt
#         + exp(-delta*n)*_n p_x
#       }
#
# Premiums are paid continuously while alive for at
# most n years. Therefore:
#
#       EPV of premiums per unit premium rate
#       = integral_0^n exp(-delta*t)*_t p_x dt
#
# Thus:
#
#       P = EPV(Endowment benefits) /
#           integral_0^n exp(-delta*t)*_t p_x dt
#
# 6. FIVE-YEAR ENDOWMENT WITH PREMIUMS LIMITED TO 3 YEARS
#
# The benefit is payable on death during the first five
# years or at the end of five years if the life survives.
#
# However, premiums are paid only while alive and for
# at most three years.
#
# Therefore:
#
#       P = EPV(5-year endowment benefits) /
#           integral_0^3 exp(-delta*t)*_t p_25 dt
#
# 7. WHOLE-LIFE INSURANCE
#
# A whole-life insurance pays 1000 immediately at death.
#
# EPV of benefits:
#
#       1000 * integral_0^infinity
#         exp(-delta*t)*_t p_x*mu_(x+t) dt
#
# If premiums are paid throughout life:
#
#       P = EPV(Whole-life benefit) /
#           integral_0^infinity exp(-delta*t)*_t p_x dt
#
# If premiums are paid for at most 20 years:
#
#       P = EPV(Whole-life benefit) /
#           integral_0^20 exp(-delta*t)*_t p_x dt
#
# 8. 30-YEAR DEFERRED WHOLE-LIFE ANNUITY
#
# A 30-year deferred whole-life annuity pays a continuous
# annuity at rate 1000 per year after 30 years, provided
# the life is alive.
#
# Its EPV of benefits is:
#
#       1000 * integral_30^infinity
#         exp(-delta*t)*_t p_30 dt
#
# Premiums are paid continuously while alive for at
# most 30 years. Their EPV per unit premium rate is:
#
#       integral_0^30 exp(-delta*t)*_t p_30 dt
#
# Therefore:
#
#       P = 1000 * integral_30^infinity
#                 exp(-delta*t)*_t p_30 dt
#           /
#           integral_0^30 exp(-delta*t)*_t p_30 dt
#
# The premium rate is calculated using the equivalence
# principle in each part.

# R FUNCTIONS USED:
#
# integrate() : numerical integration.
# exp()       : exponential function.
# log()       : natural logarithm.
# data.frame(): stores results in tables.
# for()       : repeats calculations.
# matplot()   : plots premium rates.
# legend()    : adds a legend.
# round()     : rounds displayed values.

# CODE AND OUTPUT:

# Given values
B <- 0.0001151
C <- 1.096
delta <- 0.05
Benefit <- 1000

# Survival probability for a life aged x after t years
survival <- function(x,t) {
  exp(-(B*C^x/log(C))*(C^t-1))
}

# Force of mortality at attained age x+t
mortality <- function(x,t) {
  B*C^(x+t)
}

# Present value of a continuous premium stream
# paid for at most k years, while the life is alive
premium_annuity <- function(x,k) {
  integrate(function(t) {
    exp(-delta*t)*survival(x,t)
  },lower=0,upper=k)$value
}

# EPV of n-year endowment insurance benefits
endowment_benefit <- function(x,n) {
  
  death_value <- integrate(function(t) {
    exp(-delta*t)*survival(x,t)*mortality(x,t)
  },lower=0,upper=n)$value
  
  maturity_value <- exp(-delta*n)*survival(x,n)
  
  Benefit*(death_value+maturity_value)
}

# Continuous level premium for n-year endowment insurance
endowment_premium <- function(x,n,k=n) {
  endowment_benefit(x,n)/premium_annuity(x,k)
}

# EPV of whole-life insurance benefits
whole_life_benefit <- function(x) {
  Benefit*integrate(function(t) {
    exp(-delta*t)*survival(x,t)*mortality(x,t)
  },lower=0,upper=100)$value
}

# PART 1: n-year endowment insurance, x=25 and x=30
n_values <- 1:10

part1 <- data.frame(n=n_values,
                    Premium_Age_25=sapply(n_values,
                                          function(n) endowment_premium(25,n)),
                    Premium_Age_30=sapply(n_values,
                                          function(n) endowment_premium(30,n)))

cat("Part 1: n-year Endowment Insurance Premium Rates\n")
print(round(part1,4))

# Graph for Part 1
matplot(n_values,part1[,2:3],
        type="b",pch=c(16,17),lty=c(1,2),lwd=2,
        xlab="Policy term (n)",
        ylab="Continuous Premium Rate",
        main="n-year Endowment Insurance")

legend("topright",
       legend=c("Age 25","Age 30"),
       pch=c(16,17),lty=c(1,2),lwd=2)

# PART 2: 5-year endowment, premiums paid for at most 3 years
part2 <- endowment_premium(x=25,n=5,k=3)

cat("\nPart 2: 5-year Endowment, 3-year Premium Period\n")
cat("Continuous Premium Rate =",round(part2,4),"\n")

# PART 3: Whole-life insurance, premiums paid throughout life
part3 <- data.frame(
  Age=c(25,30),
  Premium=sapply(c(25,30),function(x) {
    whole_life_benefit(x)/premium_annuity(x,100)
  })
)

cat("\nPart 3: Whole-life Insurance, Whole-life Premiums\n")
print(round(part3,4))

# PART 4: Whole-life insurance, premiums paid for at most 20 years
part4 <- whole_life_benefit(25)/premium_annuity(25,20)

cat("\nPart 4: Whole-life Insurance, Premiums Limited to 20 Years\n")
cat("Continuous Premium Rate =",round(part4,4),"\n")

# PART 5: 30-year deferred whole-life annuity, age 30
deferred_benefit <- Benefit*integrate(function(t) {
  exp(-delta*t)*survival(30,t)
},lower=30,upper=100)$value

part5 <- deferred_benefit/premium_annuity(30,30)

cat("\nPart 5: 30-year Deferred Whole-life Annuity\n")
cat("EPV of deferred annuity =",round(deferred_benefit,4),"\n")
cat("Continuous Premium Rate =",round(part5,4),"\n")

# RESULT AND CONCLUSION:
#
# The continuous level premium rates are calculated by
# dividing the expected present value of benefits by the
# expected present value of a unit continuous premium
# stream. The results for each part are displayed above.
#
# INTERPRETATION:
#
# 1. Comparison of ages in Part 1:
# For the same endowment term, the premium rate for age 30
# is slightly higher than for age 25 because mortality
# rates are higher at the older age.
#
# 2. Comparison of ages in Part 3:
# The whole-life insurance premium rate is higher for age
# 30 than for age 25 because the older life has a greater
# probability of death at earlier durations.
#
# 3. Comparison of Parts 1 and 2:
# In Part 2, the five-year endowment benefit is unchanged,
# but premiums are payable for at most three years rather
# than five years. The shorter premium-paying period gives
# a higher level premium rate.
#
# 4. Comparison of Parts 3 and 4:
# The benefit is the same whole-life insurance, but in
# Part 4 premiums are limited to 20 years. A shorter
# premium-paying period increases the continuous premium
# rate compared with premiums payable throughout life.
#
# 5. Part 5:
# The deferred annuity benefit starts only after 30 years.
# Premiums are collected during the first 30 years while
# the life is alive. The premium rate balances the EPV
# of the deferred annuity payments against the EPV of
# the limited premium stream.
#
# Note: The continuous premium rate is expressed per year.
# The benefit amount of 1000 in Part 5 is interpreted as
# an annual continuous annuity payment rate of 1000 after
# the 30-year deferment period.

############################################################
#                    END OF PRACTICAL 13
############################################################




















































############################################################
#                         PRACTICAL 14
#       PROSPECTIVE RESERVE UNDER MAKEHAM'S LAW
############################################################

# AIM:
# To calculate the prospective reserve for a whole-life
# insurance issued to a life aged 30, using Makeham's law
# of mortality, when premiums are payable as a whole-life
# annuity-due.

# PROBLEM:
#
# The force of mortality follows Makeham's law:
#
#       mu_x = A + B*C^x
#
# where:
#       A = 0.0007
#       B = 0.0001151
#       C = 1.096
#
# The annual effective rate of interest is i = 0.05 and:
#
#       A_40 = 0.27975
#
# Calculate the prospective reserve for a whole-life
# insurance issued to (30), with a benefit of 1000 units,
# for k = 1,2,...,10, when premiums are payable as a
# whole-life annuity-due.

# THEORY:
#
# 1. MAKEHAM'S LAW OF MORTALITY
#
# Under Makeham's law, the force of mortality at age x is:
#
#       mu_x = A + B*C^x
#
# The constant A represents the age-independent component
# of mortality, while B*C^x represents the age-dependent
# component.
#
# For a life aged x, the force of mortality t years later is:
#
#       mu_(x+t) = A + B*C^(x+t)
#
# 2. ONE-YEAR SURVIVAL PROBABILITY
#
# The probability of surviving one year from age x is:
#
#       p_x = exp[- integral_0^1 mu_(x+t) dt]
#
# Substituting Makeham's law:
#
#       integral_0^1 [A+B*C^(x+t)] dt
#       = A + B*C^x*(C-1)/ln(C)
#
# Therefore:
#
#       p_x = exp[-A-B*C^x*(C-1)/ln(C)]
#
# and:
#
#       q_x = 1-p_x
#
# 3. ANNUAL WHOLE-LIFE ASSURANCE
#
# Let A_x denote the actuarial present value of a
# whole-life assurance of 1, payable at the end of the
# year of death.
#
# Its recursive formula is:
#
#       A_x = v*(q_x + p_x*A_(x+1))
#
# where:
#
#       v = 1/(1+i)
#
# The value A_40 = 0.27975 is given. We use it as the
# starting value and calculate A_39, A_38, ..., A_30
# recursively backwards.
#
# 4. WHOLE-LIFE ANNUITY-DUE
#
# The actuarial present value of a whole-life annuity-due
# of 1 per year is:
#
#       ä_x = (1-A_x)/d
#
# where:
#
#       d = i/(1+i)
#
# 5. NET ANNUAL PREMIUM
#
# The benefit amount is 1000.
# Premiums are payable annually in advance while the
# policyholder is alive, throughout the whole of life.
#
# By the equivalence principle:
#
#       EPV(Benefits) = EPV(Premiums)
#
#       1000*A_30 = P*ä_30
#
# Hence, the annual level premium is:
#
#       P = 1000*A_30/ä_30
#
# 6. PROSPECTIVE RESERVE
#
# The prospective reserve at time k is the expected
# present value of future benefits minus the expected
# present value of future premiums, conditional on the
# policyholder being alive at time k.
#
# At time k, the attained age is 30+k.
#
# Therefore:
#
#       V_k = 1000*A_(30+k) - P*ä_(30+k)
#
# where P is the annual premium calculated at issue.
#
# This formula is used for k=1,2,...,10.
#
# Note: The reserve is calculated immediately after k
# complete years have elapsed, conditional on survival
# to that time. Future premiums are valued as a whole-life
# annuity-due at the attained age.

# R FUNCTIONS / COMMANDS USED:
#
# exp()       : exponential function.
# log()       : natural logarithm.
# for()       : calculates values recursively.
# data.frame(): creates the result table.
# plot()      : plots the prospective reserves.
# round()     : rounds displayed results.
# cat()       : displays results.

# CODE AND OUTPUT:

# Given values
A <- 0.0007
B <- 0.0001151
C <- 1.096
i <- 0.05
A40 <- 0.27975
Benefit <- 1000

# Discount factors
v <- 1/(1+i)
d <- i/(1+i)

# Calculate one-year probability of death qx
qx <- function(x) {
  1-exp(-(A+B*C^x*(C-1)/log(C)))
}

# Calculate annual whole-life assurance values
# A40 is given; calculate A39 down to A30
Ax <- numeric(11)
names(Ax) <- 30:40
Ax["40"] <- A40

for(x in 39:30) {
  q <- qx(x)
  p <- 1-q
  Ax[as.character(x)] <-
    v*(q+p*Ax[as.character(x+1)])
}

# Calculate whole-life annuity-due values
annuity_due <- (1-Ax)/d

# Calculate annual net premium at age 30
P <- Benefit*Ax["30"]/annuity_due["30"]

cat("A30 =",Ax["30"],"\n")
cat("Whole-life annuity-due at age 30 =",annuity_due["30"],"\n")
cat("Annual net premium =",P,"\n")

# Calculate prospective reserves for k = 1 to 10
k <- 1:10
attained_age <- 30+k

reserve <- numeric(length(k))

for(j in 1:length(k)) {
  x <- attained_age[j]
  reserve[j] <- Benefit*Ax[as.character(x)] -
    P*annuity_due[as.character(x)]
}

# Display the reserve table
results <- data.frame(
  k=k,
  Age=attained_age,
  Ax=as.numeric(Ax[as.character(attained_age)]),
  Annuity_Due=as.numeric(annuity_due[as.character(attained_age)]),
  Prospective_Reserve=reserve
)

cat("\nProspective Reserve Table:\n")
print(round(results,4))

# Plot prospective reserve against policy duration
plot(k,reserve,
     type="b",pch=16,lwd=2,
     xlab="Policy duration (k)",
     ylab="Prospective Reserve",
     main="Prospective Reserve under Makeham's Law")

# RESULT AND CONCLUSION:
#
# The annual net premium is calculated using the
# equivalence principle. The annual whole-life assurance
# values for ages 30 to 39 are obtained recursively
# backwards from the given value A40=0.27975.
#
# The prospective reserve at each duration k is then
# calculated as the EPV of future benefits less the EPV
# of future premiums.
#
# The numerical values are displayed in the result table.

# INTERPRETATION:
#
# 1. The prospective reserve increases as the policy
#    duration k increases for the given data.
#
# 2. As the policyholder gets older, the force of mortality
#    under Makeham's law increases. Consequently, the
#    actuarial present value of the future whole-life
#    insurance benefit increases.
#
# 3. The value of the future premium annuity-due decreases
#    with age because fewer future premium payments are
#    expected to be made.
#
# 4. Both effects contribute to the increase in the
#    prospective reserve over the policy duration.
#
# Therefore, the reserve is positive and increases over
# the ten policy durations considered.

############################################################
#                    END OF PRACTICAL 14
############################################################
