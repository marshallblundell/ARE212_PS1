#############################################################
#     File Name           :     Documents/ARE/ARE212/PS1/Code/ps1functions.R
#     Created By          :     MBlundell
#     Creation Date       :     [2018-01-28 09:19]
#     Last Modified       :     [2018-02-06 10:56]
#     Description         :      
#     Stores useful functions for ps1.
#############################################################

#
# demean() takes a vector and returns demeaned vector.
#
demean <- function(x) x - mean(x)

#
# reg() takes a vector y and a matrix X and regresses y on X
# Return value is matrix of coefficients.
# Does not include an intercept by default. User must append
# a vector of ones to X in that case.
#
reg <- function(y, X) {
    solve(t(X) %*% X) %*% t(X) %*% y
}

#
# pred() takes a matrix X and a matrix beta
# Return value is vector of predicted values.
#
pred <- function(X, beta) {
    as.vector(X %*% beta)
}

#
# R.squared.uc() takes a vector e of residuals and a vector
# dependent variable y and returns uncentered R squared.
#
R.squared.uc <- function(e, y) {
    as.vector(1 - (t(e) %*% e) / (t(y) %*% y))
}

#
# R.squared() takes a vector e of residuals and a vector
# dependent variable y and returns centered R squared.
#
R.squared <- function(e, y) {
    y.demeaned <- demean(y)
    as.vector(1 - ((t(e) %*% e) / (t(y.demeaned) %*% y.demeaned)))
}

#
# adj.R.squared() takes a vector e of residuals and a vector
# dependent variable y, total degrees of freedom and residual degrees
# of freedom and calculates adjusted R-squared.
#
adj.R.squared <- function(e, y, df.t, df.e) {
    as.vector(1 - (1 - R.squared(e, y)) * (df.t / df.e))
}

# 
# AIC() takes a n, the number of obs, k, the number of estimated
# parameters (which should count estimate of variance), and e, a matrix of residuals, 
# and returns a scalar AIC.
# In the case of least squares this is
# n * log(SSR / n) + 2 * k
#
AIC <- function(n, k, e) {
    as.vector(n * log((t(e) %*% e) / n) + 2 * k)
}

# 
# SIC() takes a n, the number of obs, and e, a matrix of residuals, and returns
# a scalar AIC. In the case of least squares this is
# n * log(SSR / n) + 2 * log(n)
#
SIC <- function(n, k, e) {
    as.vector(n * log((t(e) %*% e) / n) + 2 * log(n))
}

# 
# s.squared() takes a matrix of residuals e and degrees of freedom
# and estimates standard error.
#
s.squared <- function(e, df) {
    as.vector((t(e) %*% e) / df)
}

# 
# report.stats() takes a vector dependent variable y, a matrix of regressors X,
# and fitted values beta and compiles summary stats in a list
#
report.stats <- function(y, X, beta) {
    # Set up output
    out <- list()

    # Get some intermediate values
    y_hat  <- pred(X, beta)
    e <- y - y_hat

    # Compile output
    out$n <- dim(X)[1]
    out$df <- dim(X)[1] - dim(X)[2]
    out$beta <- beta
    out$R2.uc <- R.squared.uc(e, y)
    out$adj.R2 <- adj.R.squared(e, y, out$n, out$df)
    out$R2 <- R.squared(e, y)
    out$AIC <- AIC(out$n, dim(X)[2] + 1, e) # Add 1 to estimated parameters to take into account variance estimate
    out$SIC <- SIC(out$n, dim(X)[2] + 1, e) # Add 1 to estimated parameters to take into account variance estimate
    out$s2 <- s.squared(e, out$df)

    out
}
