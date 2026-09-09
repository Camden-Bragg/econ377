############################################################
# STATISTICS QUIZ R SCRIPT
#
# ONLY CHANGE x AND y BELOW FOR EACH NEW QUESTION.
############################################################


# =========================
# CHANGE NUMBERS HERE ONLY
# =========================

x <- c(9,9,2)
y <- c(0.2,0.5,0.3)
Sxy<- -1
Sx<- 5
Sy<- 3

############################################################
# BASIC SAMPLE STATISTICS
############################################################

# SAMPLE SIZE (n)
# Use when you need to know how many observations are in x.
length(x)


# SAMPLE MEAN (x-bar)
# Use when asked for the mean or average of x.
round(mean(x), 2)


# SAMPLE VARIANCE (Sx^2)
# Use when asked for SAMPLE variance.
# R automatically divides by (n - 1).
round(var(x), 2)


# SAMPLE STANDARD DEVIATION (Sx)
# Use when asked for SAMPLE standard deviation.
# R automatically uses the sample formula with (n - 1).
round(sd(x), 2)



############################################################
# TWO-VARIABLE STATISTICS
############################################################

# SAMPLE COVARIANCE (Sxy)
# Use when given two data sets x and y and asked for covariance.
round(cov(x, y), 2)


# SAMPLE CORRELATION
# Use when given actual x and y data and asked for correlation.
# Correlation is always between -1 and +1.
round(cor(x, y), 2)



############################################################
# EXPECTED VALUE / PROBABILITY QUESTIONS
############################################################

# FOR THESE QUESTIONS:
# x = possible values
# y = probabilities
#
# Example:
# x <- c(8, 5, 7)
# y <- c(0.2, 0.5, 0.3)


# CHECK PROBABILITIES
# Probabilities should add up to 1.
sum(y)


# EXPECTED VALUE: E[X]
# Use when asked for the expected value.
# Formula: sum(value * probability)
round(sum(x * y), 2)


# EXPECTED VALUE OF X SQUARED: E[X^2]
# Use when asked for E[X^2].
# Square EACH x value first, then multiply by its probability.
round(sum((x^2) * y), 2)



############################################################
# CORRELATION WHEN Sxy, Sx, AND Sy ARE ALREADY GIVEN
############################################################

# If a question gives you Sxy, Sx, and Sy instead of raw data,
# enter them into x in this exact order:
#
# x <- c(Sxy, Sx, Sy)
#
# Example from your quiz:
# x <- c(2, 2, 4)
#
# x[1] = Sxy
# x[2] = Sx
# x[3] = Sy

round(Sxy / (Sx * Sy), 2)



############################################################
# QUICK REFERENCE
############################################################

# mean(x)       = sample mean
#
# var(x)        = sample variance
#
# sd(x)         = sample standard deviation
#
# cov(x, y)     = sample covariance
#
# cor(x, y)     = sample correlation
#
# sum(x * y)    = E[X] when y contains probabilities
#
# sum(x^2 * y)  = E[X^2] when y contains probabilities
#
# x[1]/(x[2]*x[3])
# = correlation when x contains c(Sxy, Sx, Sy)