# ============================================================================
# OLS REGRESSION & LOG TRANSFORMATIONS - CALCULATION SCRIPT
# ============================================================================
# Edit values, run, get answers. For actual data questions, load datasets first.
# ============================================================================

# ============================================================================
# CONCEPTUAL QUESTIONS (ANSWERS ONLY)
# ============================================================================

# Q1: ZCM assumption with confounding variable (family income in U)
# Correct answer: "The zero conditional mean assumption fails, because the
#                 average of U changes with class size."
# Explanation: If richer districts have smaller classes, and richer families
#              have higher test scores, then E[U|class size] ≠ 0

# Q2: Identifying OLS line from sums table
# OLS minimizes Σû_i² AND has Σû_i = 0
# Line C: Σû_i = 0.00, Σ|û_i| = 6.40, Σû_i² = 8.13 ← SMALLEST SSR
# Answer: Line C

# Q7: Sum of residuals always equals zero
# Residuals: 1.4, -0.3, -0.2, 0.1, -0.3, ?
# Answer: -0.7 (sum must = 0)

# Q13: Multiply all y by 1000
# New slope = 1000 × old slope
# New slope = 1000 × 0.05 = 50
# Answer: 50

# Q14: Divide all x by 1000
# New slope = old slope × 1000 = 0.59 × 1000 = 590
# Answer: 590

# Q15: Multiply all x by 10
# New slope = old slope ÷ 10
# Intercept does NOT change
# R² does NOT change
# Answer: "The slope is divided by 10, the intercept does not change, and R² does not change."

# Q20: Interpreting log-log coefficient
# log(rent) = 2.1 + 0.45*log(income)
# Coefficient 0.45 means: 1% rise in income → 0.45% rise in rent
# Answer: "A 1% rise in income is associated with about a 0.45% rise in rent."

# ============================================================================
# CALCULATION QUESTIONS
# ============================================================================

# --- Q3: Find SD of 3X - 7 ---
# Given: E[X] = 5, E[X²] = 61
E_X <- 4
E_X2 <- 32
Var_X <- E_X2 - E_X^2
# Var(3X - 7) = 3² * Var(X)
Var_3X_minus_7 <- 4^2 * Var_X
SD_3X_minus_7 <- sqrt(Var_3X_minus_7)
round(SD_3X_minus_7, 2)
# Expected: 18

# --- Q4: Find Var(2X - 2Y) ---
# Given: Var(X)=5, Var(Y)=3, Cov(X,Y)=-0.3
Var_X <- 9
Var_Y <- 11
Cov_XY <- -2.3
# Var(2X - 2Y) = 2²*Var(X) + (-2)²*Var(Y) + 2*2*(-2)*Cov(X,Y)
Var_2X_minus_2Y <- 2^2 * Var_X + (-3)^2 * Var_Y + 2*2*(-3)*Cov_XY
round(Var_2X_minus_2Y, 2)
# Expected: 34.4

# --- Q5: Find Cor(X, Y) from joint distribution ---
x_vals <- c(1, 5, 6, 7)
y_vals <- c(1, 11, 4, 8)
probs <- c(0.3, 0.2, 0.1, 0.4)

E_X <- sum(x_vals * probs)
E_Y <- sum(y_vals * probs)
E_XY <- sum(x_vals * y_vals * probs)
Cov_XY <- E_XY - E_X * E_Y

E_X2 <- sum(x_vals^2 * probs)
E_Y2 <- sum(y_vals^2 * probs)
Var_X <- E_X2 - E_X^2
Var_Y <- E_Y2 - E_Y^2

Cor_XY <- Cov_XY / (sqrt(Var_X) * sqrt(Var_Y))
round(Cor_XY, 2)
# Expected: -0.51

# --- Q6: OLS regression residual ---
# x = (2, 3, 5, 7, 10), y = (2, 5, 12, 7, 5)
# Find residual of observation 3: (x₃, y₃) = (5, 12)
x <- c(2,3,6,7,9)
y <- c(4,2,10,6,11)

x_bar <- mean(x)
y_bar <- mean(y)
beta_1 <- cov(x, y) / var(x)
beta_0 <- y_bar - beta_1 * x_bar

# Fitted value for x=5
x_3 <- 5
y_hat_3 <- beta_0 + beta_1 * x_3
# Residual
residual_3 <- 12 - y_hat_3
round(residual_3, 2)
# Expected: 5.9

# --- Q8: Sum of x*residuals (should be 0) ---
# x = (2, 3, 6, 9, 10, 12), y = (7, 4, 9, 3, 5, 2)
x <- c(2, 3, 6, 9, 10, 12)
y <- c(7, 4, 9, 3, 5, 2)

x_bar <- mean(x)
y_bar <- mean(y)
beta_1 <- cov(x, y) / var(x)
beta_0 <- y_bar - beta_1 * x_bar

y_hat <- beta_0 + beta_1 * x
residuals <- y - y_hat
sum_x_residuals <- sum(x * residuals)
round(sum_x_residuals, 2)
# Expected: 0 (always 0 by OLS property)

# --- Q9: Find SSR from SSE and R² ---
# Given: SSE = 128, R² = 0.33
SSE <- 186
R_squared <- 0.6
# R² = SSE / SST, so SST = SSE / R²
SST <- SSE / R_squared
# SST = SSE + SSR, so SSR = SST - SSE
SSR <- SST - SSE
round(SSR, 2)
# Expected: 259.88

# --- Q10: Find SST from sample variance ---
# Given: n = 32, S_y² = 11
n <- 11
S_y2 <- 17
# SST = (n-1) * S_y²
SST <- (n - 1) * S_y2
round(SST, 2)
# Expected: 341

# --- Q12: Find sample mean of y ---
# Given: β̂₀ = 2, β̂₁ = 1.8, x̄ = 12
# Since (x̄, ȳ) lies on OLS line: ȳ = β̂₀ + β̂₁*x̄
beta_0 <- 2
beta_1 <- 1.8
x_bar <- 12
y_bar <- beta_0 + beta_1 * x_bar
round(y_bar, 2)
# Expected: 23.6

# --- Q17: Log-level elasticity ---
# log(y) = 0.6 + 0.07x
# When x rises by 3: Δlog(y) = 0.07 * 3 = 0.21
# Percent change ≈ 0.21 * 100 = 21%
beta_1 <- 0.07
delta_x <- 3
delta_log_y <- beta_1 * delta_x
pct_change <- delta_log_y * 100
round(pct_change, 2)
# Expected: 21

# --- Q18: Log-log elasticity ---
# log(y) = 5.7 + 0.95*log(x)
# Elasticity = coefficient = 0.95
# When x rises by 3%: y rises by 0.95 * 3 = 2.85%
elasticity <- 0.95
pct_x_change <- 3
pct_y_change <- elasticity * pct_x_change
round(pct_y_change, 2)
# Expected: 2.85

# --- Q19: Level-log interpretation ---
# y = 129 + 35.7*log(x)
# When x rises by 13%: Δy = 35.7 * ln(1.13)
beta_1 <- 35.7
pct_x_change <- 13
# For small %, ln(1 + pct/100) ≈ pct/100
# ln(1.13) = 0.1222
delta_y <- beta_1 * log(1 + pct_x_change/100)
round(delta_y, 2)
# Expected: 4.64

# ============================================================================
# DATASET-DEPENDENT QUESTIONS
# ============================================================================

# Q11: Requires wage1 dataset
# Run: reg <- lm(wage ~ tenure, data = wage1)
# Then: residuals(reg)[12]
# Expected residual: -1.92

# Q16: Requires ceosal1 dataset
# Original regression: lm(salary ~ sales, data = ceosal1)
# If salary is in dollars instead of thousands, multiply slope by 1000
# Expected new slope: 15.47

# Q21: Requires wage1 dataset
# Run: lm(log(wage) ~ exper, data = wage1)
# Coefficient on exper * 100 = percent change per year
# For 10 years: coef * 10 * 100
# Expected: 4.66%

# ============================================================================
# KEY FORMULAS USED
# ============================================================================
# Var(aX + b) = a² * Var(X)
# Var(aX + bY) = a²Var(X) + b²Var(Y) + 2ab*Cov(X,Y)
# Cor(X,Y) = Cov(X,Y) / (sd(X) * sd(Y))
# OLS slope: β̂₁ = cov(x,y) / var(x)
# OLS intercept: β̂₀ = ȳ - β̂₁*x̄
# (x̄, ȳ) always on OLS line: ȳ = β̂₀ + β̂₁*x̄
# R² = SSE / SST
# SST = SSE + SSR
# SST = (n-1) * S_y²
# Log-level: %Δy ≈ 100 * β₁ * Δx
# Log-log: %Δy ≈ β₁ * %Δx
# Level-log: Δy ≈ (β₁/100) * %Δx

wage1

lm(log(wage) ~ tenure, data = wage1)
