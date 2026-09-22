############################################################
# ECONOMETRICS QUIZ - COMPACT R SCRIPT
# Change values ONLY in the INPUTS section.
############################################################

# =========================
# INPUTS - CHANGE THESE ONLY
# =========================

# Raw sample data / joint-distribution values
x <- c(6,3,1)
y <- c(0,4,1)
p <- c(0.2, 0.3, 0.5)       # probabilities ONLY when given

# Prediction / change values
x_value <- 40/10
change_x <- 3

# Conditional expectation
subgroup_y <- c(0, 1, 1, 5)
EY_given_X <- 5

# Given expected values
EX <- 3
EY <- 1
EX2 <- 41
EXY <- 26

# Given variance / covariance / SD
VarX <- 9
VarY <- 6
CovXY <- -3
Sx <- 2
Sy <- 3

# Constants / transformations
a <- 5
b <- 9
c0 <- 3
a1 <- 5
a2 <- 3
constant <- 7

# Given regression values
beta0_given <- 12/10
beta1_given <- 3/10
xbar_given <- 9
ybar_given <- 11


############################################################
# 1. BASIC SAMPLE STATISTICS
############################################################

length(x)                    # sample size
round(mean(x), 2)            # mean
round(var(x), 2)             # SAMPLE variance
round(sd(x), 2)              # sample SD
round(cov(x, y), 2)          # SAMPLE covariance
round(cor(x, y), 2)          # sample correlation


############################################################
# 2. PROBABILITY DISTRIBUTIONS
############################################################

sum(p)                       # probabilities should = 1
round(sum(x*p), 2)           # E[X]
round(sum(y*p), 2)           # E[Y]
round(sum(x^2*p), 2)         # E[X^2]
round(sum(x*y*p), 2)         # E[XY]

# Var(X) = E[X^2] - E[X]^2
round(sum(x^2*p) - sum(x*p)^2, 2)

# Cov(X,Y) = E[XY] - E[X]E[Y]
round(sum(x*y*p) - sum(x*p)*sum(y*p), 2)

# Population regression slope from probabilities
prob_CovXY <- sum(x*y*p) - sum(x*p)*sum(y*p)
prob_VarX <- sum(x^2*p) - sum(x*p)^2
round(prob_CovXY / prob_VarX, 2)


############################################################
# 3. EXPECTATION / VARIANCE RULES
############################################################

constant                         # E[c] = c
round(a*EX + b, 2)               # E[aX+b]
round(a*EX + b*EY, 2)            # E[aX+bY]
round(a*EX + b*EY - c0, 2)       # E[aX+bY-c]

round(EX2 - EX^2, 2)             # Var(X)
round(sqrt(VarX), 2)             # SD from variance
round(Sx^2, 2)                   # variance from SD
round(a^2*VarX, 2)               # Var(aX+b)
round(VarX+VarY+2*CovXY, 2)      # Var(X+Y), general
round(VarX+VarY, 2)              # Var(X+Y), if independent
round(a^2*VarX+b^2*VarY+2*a*b*CovXY, 2)  # Var(aX+bY)


############################################################
# 4. COVARIANCE / CORRELATION
############################################################

round(EXY - EX*EY, 2)            # Cov(X,Y)
round(a1*a2*CovXY, 2)            # Cov(a1X+b1,a2Y+b2)
round(mean(x*y)-mean(x)*mean(y), 2)  # equally likely pairs
round(CovXY/(Sx*Sy), 2)          # correlation

# Independent X,Y => Cov(X,Y)=0 and E[XY]=E[X]E[Y]
# Correlation is unitless and always between -1 and 1.


############################################################
# 5. CONDITIONAL EXPECTATION
############################################################

round(mean(y[x == x_value]), 2)  # E[Y|X=x] from paired data
round(mean(subgroup_y), 2)       # if subgroup Y values are given
round(sum(y*p), 2)               # if p = conditional probabilities
round(a*EY_given_X + b, 2)       # E[aY+b | X=x]


############################################################
# 6. SIMPLE LINEAR REGRESSION - MUST KNOW
############################################################

# Y = beta0 + beta1*X + U
# Y = dependent/outcome variable
# X = independent/explanatory variable
# beta0 = intercept
# beta1 = slope: change in Y for +1 X, holding other factors fixed
# U = other/unobserved factors affecting Y
#
# E[U|X] = 0:
# average U does not depend on X; DOES NOT mean every U = 0
#
# E[Y|X=x] = beta0 + beta1*x
# beta0 + beta1*X = explained part of Y
# U = unexplained part
#
# beta0, beta1 = POPULATION parameters; we observe only a sample
# beta0_hat, beta1_hat = SAMPLE estimates


############################################################
# 7. OLS FROM RAW SAMPLE x AND y
############################################################

# Slope: beta1_hat = Cov(x,y)/Var(x)
beta1_hat <- cov(x, y) / var(x)
round(beta1_hat, 2)

# Same slope using summation formula
round(sum((x-mean(x))*(y-mean(y))) / sum((x-mean(x))^2), 2)

# Intercept: beta0_hat = ybar - beta1_hat*xbar
beta0_hat <- mean(y) - beta1_hat*mean(x)
round(beta0_hat, 2)

# Prediction: y_hat = beta0_hat + beta1_hat*x
round(beta0_hat + beta1_hat*x_value, 2)

# Change in predicted Y = slope * change in X
round(beta1_hat * change_x, 2)


############################################################
# 8. OLS WHEN VALUES ARE GIVEN
############################################################

round(CovXY / VarX, 2)                        # given slope
round(ybar_given-beta1_given*xbar_given, 2)   # given intercept
round(beta0_given+beta1_given*x_value, 2)      # predicted Y
round(beta1_given*change_x, 2)                 # change in predicted Y


############################################################
# 9. FAST FORMULA CHECK
############################################################

# E[aX+b] = aE[X]+b
# E[aX+bY] = aE[X]+bE[Y]            independence NOT required
# Var(X) = E[X^2]-E[X]^2
# Var(aX+b) = a^2 Var(X)             b does not matter
# Var(X+Y) = VarX+VarY+2CovXY
# Independent => Var(X+Y)=VarX+VarY
# Cov(X,Y) = E[XY]-E[X]E[Y]
# Cor(X,Y) = CovXY/(Sx*Sy)
#
# OLS:
# beta1_hat = Cov(x,y)/Var(x)
# beta0_hat = ybar-beta1_hat*xbar
# y_hat = beta0_hat+beta1_hat*x
# change in y_hat = beta1_hat*change in x


############################################################
# 10. IMPORTANT QUIZ WARNING
############################################################

# SAMPLE / raw sample data:
# use cov(x,y) and var(x)  --> both divide by n-1
#
# PROBABILITY DISTRIBUTION with p:
# DO NOT use cov(x,y) or var(x)
# E[X]     = sum(x*p)
# E[Y]     = sum(y*p)
# E[XY]    = sum(x*y*p)
# Var(X)   = sum(x^2*p) - sum(x*p)^2
# Cov(X,Y) = sum(x*y*p) - sum(x*p)*sum(y*p)
# beta1    = Cov(X,Y)/Var(X)
############################################################
