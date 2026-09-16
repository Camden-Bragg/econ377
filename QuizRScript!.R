############################################################
# ECONOMETRICS QUIZ - COMPACT R SCRIPT
# Change values ONLY in the INPUTS section.
############################################################

# =========================
# INPUTS - CHANGE THESE ONLY
# =========================

# Raw data / possible values
x <- c(2,0,3)
y <- c(8,9,9)
p <- c(0.2,0.3,0.5)     # probabilities when needed
subgroup_y <- c(0,1,1,5)
x_value <- 1               # for E[Y | X = x]

# Given expected values
EX  <- 3                   # E[X]
EY  <- 1                   # E[Y]
EX2 <- 41                  # E[X^2]
EXY <- 26                   # E[XY]
EY_given_X <- 5            # E[Y | X = x]

# Given variance / covariance / SD
VarX  <- 5
VarY  <- 6
CovXY <- -1
Sx <- 2                    # sd(X)
Sy <- 3                    # sd(Y)

# Constants / coefficients
a <- 5
b <- 9
c0 <- 3
a1 <- 5
a2 <- 3
constant <- 7


############################################################
# 1. BASIC SAMPLE STATISTICS
############################################################

length(x)                   # sample size n
round(mean(x), 2)           # sample mean
round(var(x), 2)            # sample variance
round(sd(x), 2)             # sample standard deviation
round(cov(x, y), 2)         # SAMPLE covariance
round(cor(x, y), 2)         # SAMPLE correlation


############################################################
# 2. EXPECTED VALUE / PROBABILITY
############################################################

sum(y)                      # check probabilities add to 1
round(sum(x * y), 2)        # E[X]
round(sum((x^2) * y), 2)    # E[X^2]

# Var(X) from a probability distribution
round(sum((x^2)*y) - (sum(x*y))^2, 2)

# E[c] = c
constant

# E[aX + b]
round(a * EX + b, 2)

# E[aX + bY]
round(a * EX + b * EY, 2)

# E[aX + bY - c]
round(a * EX + b * EY - c0, 2)


############################################################
# 3. VARIANCE / STANDARD DEVIATION
############################################################

# Var(X) = E[X^2] - (E[X])^2
round(EX2 - EX^2, 2)

# sd(X) = sqrt(Var(X))
round(sqrt(VarX), 2)

# Var(X) = sd(X)^2
round(Sx^2, 2)

# Var(aX + b) = a^2 Var(X)
round(a^2 * VarX, 2)

# GENERAL: Var(X + Y)
round(VarX + VarY + 2*CovXY, 2)

# INDEPENDENT X,Y: Var(X + Y)
round(VarX + VarY, 2)

# GENERAL: Var(aX + bY)
round(a^2*VarX + b^2*VarY + 2*a*b*CovXY, 2)


############################################################
# 4. COVARIANCE
############################################################

# Cov(X,Y) = E[XY] - E[X]E[Y]
round(EXY - EX*EY, 2)

# If X and Y are independent:
# Cov(X,Y) = 0
# E[XY] = E[X]E[Y]

# Cov(a1X+b1, a2Y+b2) = a1*a2*Cov(X,Y)
round(a1 * a2 * CovXY, 2)

# If listed (X,Y) pairs are the ENTIRE distribution
# and each pair is equally likely:
round(mean(x*y) - mean(x)*mean(y), 2)


############################################################
# 5. CORRELATION
############################################################

# Cor(X,Y) = Cov(X,Y)/(sd(X)sd(Y))
round(CovXY / (Sx * Sy), 2)

# Correlation facts:
# always between -1 and 1
# unitless
# -1 = perfect negative, 0 = no linear relation, 1 = perfect positive


############################################################
# 6. CONDITIONAL EXPECTATION
############################################################

# E[Y | X=x] from full paired x,y data
round(mean(y[x == x_value]), 2)

# If subgroup Y values are already given
round(mean(subgroup_y), 2)

# E[Y | X=x] when y = values and p = conditional probabilities
round(sum(y * p), 2)

sum(p)                      # check conditional probabilities = 1

# E[aY+b | X=x]
round(a * EY_given_X + b, 2)


############################################################
# 7. FAST CONCEPT CHECKS
############################################################

# E[c] = c
# E[aX+b] = aE[X]+b
# E[aX+bY] = aE[X]+bE[Y]   (independence NOT required)
#
# Var(X) = E[X^2]-(E[X])^2
# sd(X) = sqrt(Var(X))
# Var(aX+b) = a^2 Var(X)    (b does NOT matter)
#
# Cov(X,Y) = E[XY]-E[X]E[Y]
# Independent => Cov(X,Y)=0
#
# Var(X+Y) = VarX+VarY+2CovXY
# Independent => Var(X+Y)=VarX+VarY
#
# Cov(a1X+b1,a2Y+b2) = a1*a2*CovXY
#
# Cor = CovXY/(Sx*Sy)
# Correlation is unitless and between -1 and 1
#
# E[Y|X=x] = average Y among observations with X=x


############################################################
# 8. IMPORTANT QUIZ WARNING
############################################################

# cov(x,y) = SAMPLE covariance, divides by n-1.
#
# If the question gives a probability distribution or says
# the listed outcomes are equally likely possible outcomes,
# use:
# E[XY] - E[X]E[Y]
# NOT cov(x,y).
############################################################