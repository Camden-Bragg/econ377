# ============================================================================
# ECONOMETRICS & DATA ANALYTICS - QUIZ 9 (OCTOBER 28, 2026)
# 11 QUESTIONS: #1, #2, #3, #4, #6, #7, #9, #12, #13, #14, #15
# ============================================================================
# HOW TO USE THIS SCRIPT
# 1. Press Command + F and search "QUESTION 7" (or any question number).
# 2. Read the ORIGINAL PRACTICE QUESTION and its hint.
# 3. Change ONLY the values under "CHANGE THESE NUMBERS" for that question.
# 4. Highlight that question's changeable numbers AND "RUN THIS CODE" lines.
#    Press Command + Return in RStudio on your Mac (or click Run).
# 5. Look for "QUESTION __ ANSWER" in the Console. Enter the number only,
#    without a % sign, when the quiz asks for a numeric percentage.
# 6. Each question can be run separately. You do NOT have to run all 11.
# 7. Questions 3, 14, and 15 use Wooldridge datasets. If necessary, type
#    install.packages("wooldridge") ONCE in the RStudio Console beforehand.
# 8. Do NOT round intermediate regression coefficients. Round the final
#    answer to TWO decimal places, exactly as the quiz requests.
#
# CONCEPT CHECKS:
# LEVEL-LOG: y = b0 + b1*log(x)      -> change in y = b1*(% change in x)/100.
# LOG-LEVEL: log(y) = b0 + b1*x      -> % change in y = 100*b1*(change in x).
# LOG-LOG:   log(y) = b0 + b1*log(x) -> % change in y = b1*(% change in x).
# Use the APPROXIMATE percent formulas because these questions say "about."
# ============================================================================




# ============================================================================
# QUESTION 1 - LEVEL-LOG: INTERPRETING A COEFFICIENT
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A regression gives predicted test score = 12 + 8*log(income).
# What does the 8 mean?
#
# CHOICES:
# 1. A 1% rise in income is associated with a rise of about 8% in the
#    predicted test score.
# 2. A 1% rise in income is associated with a rise of 8 points in the
#    predicted test score.
# 3. A $1 rise in income is associated with a rise of 8 points in the
#    predicted test score.
# 4. A 1% rise in income is associated with a rise of about 0.08 points
#    in the predicted test score.
#
# HINT: The dependent variable is in LEVELS, but income is in LOGS.
#       The answer is test-score POINTS, NOT a percent of test score.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q1_slope    <- 8        # Number multiplying log(income), NOT intercept 12
q1_income_pct <- 1      # Percent change in income: use 1 for 1%; -1 for fall
#
# ------------------------ RUN THIS CODE -----------------------------------
q1_answer <- q1_slope * q1_income_pct / 100
cat("QUESTION 1 ANSWER (test-score POINTS):",
    sprintf("%.2f", q1_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# A 1% increase in income -> 8*(1/100) = 0.08 test-score POINTS.
# If the percent increase changes, update q1_income_pct, NOT q1_slope.
# Do not confuse a LEVEL-LOG model with a LOG-LEVEL model.
# PRACTICE ANSWER: OPTION 4 - about 0.08 points.




# ============================================================================
# QUESTION 2 - LOG-LEVEL: X FALLS BY 3
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A log-level regression gives log(y)_hat = 1.6 + 0.05*x.
# By about what percent does y change when x FALLS by 3?
# (Give a percent; it can be negative. Round to the hundredths.)
#
# HINT: % change in y ~= 100 * slope * change in x.
#       A FALL means the change in x MUST be NEGATIVE.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q2_slope    <- 0.05     # Coefficient on x (ignore intercept 1.6)
q2_change_x <- -3       # X FALLS by 3 -> -3; if it RISES by 3, use +3
#
# ------------------------ RUN THIS CODE -----------------------------------
q2_answer <- 100 * q2_slope * q2_change_x
cat("QUESTION 2 ANSWER (%):", sprintf("%.2f", q2_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# log(y) on the LEFT -> multiply slope * change in x * 100.
# 100 * 0.05 * (-3) = -15%; do not use the intercept.
# PRACTICE ANSWER: -15.00




# ============================================================================
# QUESTION 3 - WOOLDRIDGE bwght: LOG-LOG REGRESSION
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Data: bwght (bwght = baby's birth weight in ounces; faminc = family's
# income in thousands of dollars). Run the log-log regression
# lm(log(bwght) ~ log(faminc), data = bwght).
# By about what percent does predicted birth weight change when family
# income RISES by 20%? (Give a percent. Round to the hundredths.)
#
# HINT: LOG-LOG: % change in y ~= slope * % change in x.
#       The coefficient on log(faminc) is an ELASTICITY.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q3_income_pct <- 20     # RISES by 20% -> 20; a FALL of 20% -> -20
#
# ------------------------ RUN THIS CODE -----------------------------------
if (!requireNamespace("wooldridge", quietly = TRUE)) {
  stop('Install the package first: install.packages("wooldridge")')
}
data("bwght", package = "wooldridge")       # Loads birthweight dataset
q3_reg <- lm(log(bwght) ~ log(faminc), data = bwght) # Run EXACT regression
q3_slope <- unname(coef(q3_reg)["log(faminc)"])      # Take slope, not intercept
q3_answer <- q3_slope * q3_income_pct
cat("QUESTION 3 REGRESSION SLOPE:", q3_slope, "\n")
cat("QUESTION 3 ANSWER (%):", sprintf("%.2f", q3_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# Step 1: Load bwght. Step 2: Regress log(bwght) on log(faminc).
# Step 3: Find the slope on log(faminc), keeping ALL decimal places.
# Step 4: Multiply that slope by 20 for a 20% rise in family income.
# Do NOT multiply by 100 again (20 already means 20%, NOT 0.20).
# PRACTICE ANSWER: The script PRINTS the exact two-decimal value.
# IMPORTANT: Do NOT add cigs, male, etc. to the regression; the question
#            explicitly requests ONLY log(faminc).




# ============================================================================
# QUESTION 4 - UNBIASED OLS ESTIMATES (CONCEPT / MULTIPLE CHOICE)
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Suppose you could draw 1,000 different random samples from the same
# population and compute beta1_hat in each one. If OLS is unbiased
# (SLR1-SLR4 hold), which statement is correct?
#
# CHOICES:
# 1. Every one of the 1,000 estimates equals beta1.
# 2. The 1,000 estimates have no spread at all: Var(beta1_hat) = 0.
# 3. All 1,000 estimates are positive.
# 4. The average of the 1,000 estimates is close to beta1.
#
# ------------------------ RUN THIS CODE -----------------------------------
cat("QUESTION 4 ANSWER: OPTION 4 - The average of the estimates",
    "is close to the true beta1.\n")
#
# HOW TO THINK ABOUT IT:
# UNBIASED means E[beta1_hat] = beta1 (average across repeated samples).
# Individual beta1_hat values can be above or below the true beta1.
# Unbiased does NOT mean every estimate is perfect or has zero variance.
# PRACTICE ANSWER: OPTION 4 (match wording if options are rearranged).




# ============================================================================
# QUESTION 6 - WHICH CHANGES INCREASE OLS SLOPE VARIANCE? (SELECT ALL)
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Under SLR1-SLR5, Var(beta1_hat) = sigma^2 / SST_X.
# Holding everything else fixed, which changes make Var(beta1_hat)
# LARGER? SELECT ALL THAT APPLY.
#
# CHOICES:
# 1. A smaller sample (fewer observations with X spread the same way,
#    so a smaller SST_X).
# 2. A larger true slope beta1.
# 3. A larger sample (more observations with X spread the same way,
#    so a larger SST_X).
# 4. X values bunched more tightly together (a smaller SST_X).
# 5. A larger error variance sigma^2.
#
# ------------------------ RUN THIS CODE -----------------------------------
cat("QUESTION 6 ANSWER: SELECT OPTIONS 1, 4, AND 5.\n")
#
# HOW TO THINK ABOUT IT:
# Var(beta1_hat) = sigma^2 / SST_X.
# LARGER numerator sigma^2 -> LARGER slope variance.
# SMALLER denominator SST_X -> LARGER slope variance.
# Fewer observations or X values closer together -> smaller SST_X.
# A larger true beta1 is NOT in the formula, so it does not affect variance.
# PRACTICE ANSWER: OPTIONS 1, 4, 5.




# ============================================================================
# QUESTION 7 - STANDARD ERROR FROM N, SIGMA-HAT, AND SAMPLE SD OF X
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A simple regression on n = 8 observations has sigma_hat = 6.8, and
# the sample STANDARD DEVIATION of X is S_X = 2.8.
# Find the standard error se(beta1_hat). (Round to the hundredths.)
#
# HINT: SST_X = (n - 1) * S_X^2.
#       Then se(beta1_hat) = sigma_hat / sqrt(SST_X).
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q7_n         <- 8        # Number of observations
q7_sigma_hat <- 6.8      # ESTIMATED error STANDARD DEVIATION (not variance)
q7_sx        <- 2.8      # SAMPLE standard deviation of X
#
# ------------------------ RUN THIS CODE -----------------------------------
q7_sstx <- (q7_n - 1) * q7_sx^2
q7_answer <- q7_sigma_hat / sqrt(q7_sstx)
cat("QUESTION 7 SST_X:", sprintf("%.2f", q7_sstx), "\n")
cat("QUESTION 7 ANSWER (standard error):",
    sprintf("%.2f", q7_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# Do NOT use (n-2) here: sample SD S_X uses n-1 in its definition.
# Square S_X FIRST, multiply by (n-1), then square-root SST_X.
# Finally divide sigma_hat by that square root.
# PRACTICE ANSWER: 0.92




# ============================================================================
# QUESTION 9 - STANDARD DEVIATION WHEN SAMPLE SIZE IS MULTIPLIED
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Under SLR1-SLR5, a regression has sd(beta1_hat) = 1.84.
# If the sample were 10 times as large, with X spread out in the same
# way (so SST_X is also 10 times as large), what would sd(beta1_hat)
# be? (Round to the hundredths.)
#
# HINT: sd(beta1_hat) = sigma/sqrt(SST_X).
#       Increasing SST_X by factor k divides sd by sqrt(k), NOT by k.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q9_old_sd <- 1.84        # Original sd(beta1_hat)
q9_factor <- 10          # New SST_X is 10 times the old one
#
# ------------------------ RUN THIS CODE -----------------------------------
q9_answer <- q9_old_sd / sqrt(q9_factor)
cat("QUESTION 9 ANSWER (new standard deviation):",
    sprintf("%.2f", q9_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# 1.84 / sqrt(10) = 0.58, NOT 1.84/10.
# If the sample is 4 times as large, divide by sqrt(4) = 2.
# PRACTICE ANSWER: 0.58




# ============================================================================
# QUESTION 12 - MULTIPLE REGRESSION: PREDICTED VALUE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A multiple regression gives predicted y = 3 + 0.6*x1 + 1.2*x2.
# Find predicted y at x1 = 7 and x2 = 14.
# (Round to the hundredths.)
#
# HINT: predicted y = b0 + b1*x1 + b2*x2.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q12_b0 <- 3             # INTERCEPT (constant)
q12_b1 <- 0.6           # Coefficient on x1
q12_b2 <- 1.2           # Coefficient on x2
q12_x1 <- 7             # Value of x1
q12_x2 <- 14            # Value of x2
#
# ------------------------ RUN THIS CODE -----------------------------------
q12_answer <- q12_b0 + q12_b1*q12_x1 + q12_b2*q12_x2
cat("QUESTION 12 ANSWER (predicted y):",
    sprintf("%.2f", q12_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# INCLUDE the intercept because the question asks for predicted LEVEL of y.
# 3 + (0.6*7) + (1.2*14) = 24.
# PRACTICE ANSWER: 24.00




# ============================================================================
# QUESTION 13 - MULTIPLE REGRESSION: CHANGE IN PREDICTED Y
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A multiple regression gives predicted y = 2 + 3.7*x1 - 2.1*x2.
# If x1 RISES by 4 and x2 RISES by 3, by how much does predicted y
# change? (Round to the hundredths.)
#
# HINT: CHANGE in predicted y = b1*(change x1) + b2*(change x2).
#       The intercept does NOT matter for a CHANGE question.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q13_b1 <- 3.7           # Coefficient on x1
q13_b2 <- -2.1          # NEGATIVE coefficient on x2; keep the minus sign
q13_change_x1 <- 4      # x1 RISES by 4 -> +4 (falls -> -4)
q13_change_x2 <- 3      # x2 RISES by 3 -> +3 (falls -> -3)
#
# ------------------------ RUN THIS CODE -----------------------------------
q13_answer <- q13_b1*q13_change_x1 + q13_b2*q13_change_x2
cat("QUESTION 13 ANSWER (change in predicted y):",
    sprintf("%.2f", q13_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# No intercept: 3.7*(4) + (-2.1)*(3) = 14.8 - 6.3 = 8.50.
# Check BOTH the sign of each SLOPE and the sign of each X CHANGE.
# PRACTICE ANSWER: 8.50




# ============================================================================
# QUESTION 14 - WOOLDRIDGE wage1: PREDICTED HOURLY WAGE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Data: wage1 (wage = average hourly earnings in dollars;
# educ = years of education; exper = years of experience;
# tenure = years with current employer).
# Run reg <- lm(wage ~ educ + exper + tenure, data = wage1).
# What is the predicted hourly wage of a worker with 14 years of
# education, 10 years of experience, and 5 years of tenure?
# (Round to the hundredths.)
#
# HINT: Use b0 + b1*(14) + b2*(10) + b3*(5).
#       ALL coefficients must come from your actual regression.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q14_educ   <- 14         # Years of education
q14_exper  <- 10         # Years of experience
q14_tenure <- 5          # Years with current employer
#
# ------------------------ RUN THIS CODE -----------------------------------
if (!requireNamespace("wooldridge", quietly = TRUE)) {
  stop('Install the package first: install.packages("wooldridge")')
}
data("wage1", package = "wooldridge")             # Load wage1 dataset
q14_reg <- lm(wage ~ educ + exper + tenure, data = wage1) # EXACT regression
q14_b <- coef(q14_reg)                           # Save all 4 coefficients
q14_answer <- q14_b[1] + q14_b[2]*q14_educ +
              q14_b[3]*q14_exper + q14_b[4]*q14_tenure
cat("QUESTION 14 COEFFICIENTS:\n")
print(q14_b)
cat("QUESTION 14 ANSWER (dollars per hour):",
    sprintf("%.2f", q14_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# 1. Load wage1; regress WAGE (not log(wage)) on educ, exper, tenure.
# 2. Get the four UNROUNDED coefficients with coef(q14_reg).
# 3. The b[1] intercept is INCLUDED in a predicted-WAGE question.
# 4. Multiply the matching slope by its variable and add everything.
# Alternative check:
# predict(q14_reg, newdata=data.frame(educ=14, exper=10, tenure=5))
# PRACTICE ANSWER: 6.58 dollars per hour.




# ============================================================================
# QUESTION 15 - WOOLDRIDGE wage1: EFFECT OF 3 MORE YEARS OF TENURE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Data: wage1 (wage = average hourly earnings in dollars;
# educ = years of education; exper = years of experience;
# tenure = years with current employer).
# Using the SAME regression of wage on educ, exper, and tenure,
# by how much does predicted hourly wage change when tenure RISES by
# 3 years, HOLDING education and experience FIXED?
# (Round to the hundredths.)
#
# HINT: Change in predicted wage = slope_on_tenure * change_in_tenure.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
q15_change_tenure <- 3      # Tenure RISES by 3; a FALL of 3 -> -3
#
# ------------------------ RUN THIS CODE -----------------------------------
if (!requireNamespace("wooldridge", quietly = TRUE)) {
  stop('Install the package first: install.packages("wooldridge")')
}
data("wage1", package = "wooldridge")             # Load wage1 dataset
q15_reg <- lm(wage ~ educ + exper + tenure, data = wage1) # EXACT regression
q15_tenure_slope <- unname(coef(q15_reg)["tenure"])
q15_answer <- q15_tenure_slope * q15_change_tenure
cat("QUESTION 15 TENURE SLOPE:", q15_tenure_slope, "\n")
cat("QUESTION 15 ANSWER (change in dollars per hour):",
    sprintf("%.2f", q15_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# Because ONLY tenure changes, its slope is the ONLY coefficient used.
# Do NOT include intercept, education, or experience in the calculation.
# Keep full precision of the tenure slope until the FINAL rounding.
# PRACTICE ANSWER: 0.51 dollars per hour.




# ############################################################################
# END OF QUIZ 9 SCRIPT - ALL 11 QUESTIONS INCLUDED.
# ############################################################################
