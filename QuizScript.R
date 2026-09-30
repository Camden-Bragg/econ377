############################################################
# ECONOMETRICS QUIZ - COMPACT MASTER SCRIPT
# Change values in INPUTS, then run the line you need.
############################################################

# ===================== 1. INPUTS ===========================
x <- c(5,3,7)
y <- c(7,13,3)
p <- c(0.2,0.3,0.5)       # ONLY if probabilities are given
x_value <- 8               # x used for prediction / condition
change_x <- 7              # change in x
y_actual <- 4             # actual y for a residual question

subgroup_y <- c(0,1,1,5)
EY_given_X <- 5

EX <- 3; EY <- 1; EX2 <- 41; EXY <- 26
VarX <- 9; VarY <- 6; CovXY <- -3
Sx <- 2; Sy <- 3
a <- 5; b <- 9; c0 <- 3; a1 <- 5; a2 <- 3; constant <- 7

beta0_given <- 1
beta1_given <- 3/10
xbar_given <- 9
ybar_given <- 11

# QUIZ notation: SST=total, SSE=explained, SSR=residual/unexplained
SST_given <- 302
SSE_given <- 87
SSR_given <- 80
R2_given <- 25/100


# ================= 2. BASIC SAMPLE STATS ==================
length(x)                         # sample size
round(mean(x),2); round(mean(y),2)
round(var(x),2)                   # sample variance
round(sd(x),2)                    # sample SD
round(cov(x,y),2)                 # sample covariance
round(cor(x,y),2)                 # sample correlation


# ============== 3. PROBABILITY DISTRIBUTIONS ==============
# Use ONLY when p is given.
sum(p)                            # should = 1
round(sum(x*p),2)                 # E[X]
round(sum(y*p),2)                 # E[Y]
round(sum(x^2*p),2)               # E[X^2]
round(sum(x*y*p),2)               # E[XY]
round(sum(x^2*p)-sum(x*p)^2,2)    # Var(X)
round(sum(x*y*p)-sum(x*p)*sum(y*p),2)  # Cov(X,Y)

prob_CovXY <- sum(x*y*p)-sum(x*p)*sum(y*p)
prob_VarX <- sum(x^2*p)-sum(x*p)^2
round(prob_CovXY/prob_VarX,2)     # population regression slope


# ============== 4. EXPECTATION / VARIANCE =================
constant                           # E[c]=c
round(a*EX+b,2)                    # E[aX+b]
round(a*EX+b*EY,2)                 # E[aX+bY]
round(a*EX+b*EY-c0,2)              # E[aX+bY-c]
round(EX2-EX^2,2)                  # Var(X)
round(sqrt(VarX),2)                # SD from variance
round(Sx^2,2)                      # variance from SD
round(a^2*VarX,2)                  # Var(aX+b)
round(VarX+VarY+2*CovXY,2)         # Var(X+Y), general
round(VarX+VarY,2)                 # Var(X+Y), independent
round(a^2*VarX+b^2*VarY+2*a*b*CovXY,2) # Var(aX+bY)


# ============== 5. COVARIANCE / CORRELATION ===============
round(EXY-EX*EY,2)                 # Cov(X,Y)
round(a1*a2*CovXY,2)               # Cov(a1X+b1,a2Y+b2)
round(mean(x*y)-mean(x)*mean(y),2) # equally likely pairs
round(CovXY/(Sx*Sy),2)             # correlation
# Independent => Cov=0 and E[XY]=E[X]E[Y]
# Correlation is unitless and between -1 and 1.


# ============== 6. CONDITIONAL EXPECTATION =================
round(mean(y[x==x_value]),2)       # E[Y|X=x] from paired data
round(mean(subgroup_y),2)          # if subgroup y values are given
round(sum(y*p),2)                  # if p are conditional probabilities
round(a*EY_given_X+b,2)            # E[aY+b | X=x]


# ================== 7. OLS QUICK CONCEPTS ===================
# Y = beta0 + beta1*X + U
# Y = actual; Yhat = fitted/predicted; U = unobserved error
# Yhat = beta0_hat + beta1_hat*X
# residual u_hat = Y - Yhat
# u_hat > 0 -> UNDER-predicted; u_hat < 0 -> OVER-predicted
# u_hat = 0 -> exact fit
# With an intercept: sum(residuals)=0
# OLS line always passes through (mean(x),mean(y))
# R^2 = fraction of sample variation in Y explained by X
# R^2 near 0 = little explained; does NOT automatically mean invalid


# ================= 8. OLS FROM RAW x AND y ==================
# If quiz gives x=(...) and y=(...), start here.
beta1_hat <- cov(x,y)/var(x)
round(beta1_hat,2)                 # slope

# Same slope using exact formula shown on quiz
round(sum((x-mean(x))*(y-mean(y))) / sum((x-mean(x))^2),2)

beta0_hat <- mean(y)-beta1_hat*mean(x)
round(beta0_hat,2)                 # intercept

round(beta0_hat+beta1_hat*x_value,2)  # predict Y at x_value
round(beta1_hat*change_x,2)           # predicted change in Y
round(beta0_hat+beta1_hat*mean(x),2)  # equals mean(y)


# ================ 9. GIVEN FITTED LINE ======================
# If beta0 and beta1 are already given, use this section.
y_hat_given <- beta0_given+beta1_given*x_value
round(y_hat_given,2)               # fitted/predicted value

residual_given <- y_actual-y_hat_given
round(residual_given,2)            # residual = actual - fitted

round(beta1_given*change_x,2)      # change in predicted Y
round(CovXY/VarX,2)                # slope if Cov and Var are given
round(ybar_given-beta1_given*xbar_given,2) # intercept from means


# ============ 10. OLS MODEL / RESIDUALS / R-SQUARED =========
model <- lm(y~x)
fitted_values <- fitted(model)
residuals_ols <- resid(model)

round(fitted_values,2)             # all fitted values
round(residuals_ols,2)             # all residuals
round(sum(residuals_ols),10)       # should be 0
round(summary(model)$r.squared,2)  # R^2


# =============== 11. SST / SSE / SSR FROM DATA ==============
# IMPORTANT: professor's quiz notation:
# SST = TOTAL; SSE = EXPLAINED; SSR = RESIDUAL/UNEXPLAINED
SST <- sum((y-mean(y))^2)
SSE <- sum((fitted_values-mean(y))^2)
SSR <- sum(residuals_ols^2)

round(SST,2)
round(SSE,2)
round(SSR,2)
round(SSE+SSR,2)                   # equals SST
round(SSE/SST,2)                   # R^2
round(1-SSR/SST,2)                 # same R^2
round(SSR/SST,2)                   # unexplained fraction


# ========== 12. GIVEN LINE + POINTS -> FIND SSR ==============
# Put the point coordinates into x and y at the top.
y_hat_line <- beta0_given+beta1_given*x
residuals_line <- y-y_hat_line
round(sum(residuals_line^2),2)     # SSR


# ============= 13. GIVEN SST / SSE / SSR / R^2 ===============
# SST = SSE + SSR
round(SST_given-SSR_given,2)       # find SSE
round(SST_given-SSE_given,2)       # find SSR

round(SSE_given/SST_given,2)       # R^2 from SSE and SST
round(1-SSR_given/SST_given,2)     # R^2 from SSR and SST
round(SSR_given/SST_given,2)       # unexplained fraction

round((1-R2_given)*SST_given,2)    # find SSR from SST and R^2
round(R2_given*SST_given,2)        # find SSE from SST and R^2


# ================== 14. FAST FORMULA CHECK ====================
# E[aX+b] = aE[X]+b
# E[aX+bY] = aE[X]+bE[Y]        independence NOT required
# Var(X) = E[X^2]-E[X]^2
# Var(aX+b) = a^2 Var(X)
# Var(X+Y) = VarX+VarY+2CovXY
# Independent => Var(X+Y)=VarX+VarY
# Cov(X,Y) = E[XY]-E[X]E[Y]
# Cor(X,Y) = CovXY/(Sx*Sy)
#
# OLS:
# slope = Cov(x,y)/Var(x)
# intercept = ybar-slope*xbar
# Yhat = intercept+slope*x
# residual = Y-Yhat
# change in Yhat = slope*change in x
# sum(residuals)=0
# line passes through (xbar,ybar)
#
# QUIZ SUM-OF-SQUARES:
# SST = SSE + SSR
# R^2 = SSE/SST = 1-SSR/SST
# unexplained fraction = SSR/SST


# ==================== 15. IMPORTANT WARNING ===================
# RAW SAMPLE DATA: use cov(x,y) and var(x).
# PROBABILITY DISTRIBUTION WITH p: do NOT use cov(x,y)/var(x).
# Use weighted formulas: sum(x*p), sum(x*y*p), etc.
############################################################
