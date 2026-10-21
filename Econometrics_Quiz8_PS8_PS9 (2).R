# ============================================================================
# ECONOMETRICS & DATA ANALYTICS - QUIZ 8 (OCTOBER 21)
# 11 QUESTIONS: PS8 #14, #17, #19; PS9 #1, #5, #6, #9, #13, #14, #15, #16
# ============================================================================
# HOW TO USE THIS SCRIPT
# 1. Press Command + F and search, for example, "PS9 QUESTION 14".
# 2. Read the ORIGINAL PRACTICE QUESTION in that section.
# 3. Change ONLY the numbers under "CHANGE THESE NUMBERS" for that question.
# 4. Highlight/run that question's "RUN THIS CODE" lines in RStudio.
#    On a Mac, select the lines and press Command + Return (or click Run).
# 5. Read the line labeled ANSWER in your Console.
# 6. Sections are INDEPENDENT. You do not need to run the whole file.
# 7. PS9 Q6 is an EXCEPTION: it contains instructions in COMMENTS ONLY.
#    Follow them and type the commands yourself, as requested.
# ============================================================================



# ############################################################################
#                           PROBLEM SET 8
# ############################################################################



# ============================================================================
# PS8 QUESTION 14 - RESCALING AN OLS SLOPE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# In a regression of y on x, the OLS slope is beta1_hat = 0.03. Every value
# of x is then divided by 1,000 (for example, percent to decimal when the
# divisor is 100). What is the new OLS slope? (Round to the hundredths.)
#
# HINT: If x is multiplied by c, beta1_hat is multiplied by 1/c and beta0_hat
# does not change. Dividing by k is multiplying by c = 1/k.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps8_q14_slope   <- 0.03     # Original OLS slope
ps8_q14_divisor <- 1000     # Number that x is DIVIDED by
#
# ------------------------ RUN THIS CODE -----------------------------------
ps8_q14_answer <- ps8_q14_slope * ps8_q14_divisor
cat("PS8 QUESTION 14 ANSWER:", sprintf("%.2f", ps8_q14_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# Divide x by 1,000 -> multiply the slope by 1,000.
# If x is MULTIPLIED by a number instead, divide the slope by that number.
# PRACTICE ANSWER: 30.00



# ============================================================================
# PS8 QUESTION 17 - LOG-LEVEL: PERCENT CHANGE IN Y
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A log-level regression gives log(y)_hat = 1.1 + 0.01*x. By about what
# percent does y change when x rises by 2? (Give a percent, e.g. 12.34 for
# 12.34%. Round to the hundredths.)
#
# HINT: log-level: % change in y ~= 100 * beta1_hat * change in x.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps8_q17_slope    <- 0.01    # Coefficient multiplying x (NOT the intercept)
ps8_q17_x_change <- 2       # x rises by 2; use -2 if x FALLS by 2
#
# ------------------------ RUN THIS CODE -----------------------------------
ps8_q17_answer <- 100 * ps8_q17_slope * ps8_q17_x_change
cat("PS8 QUESTION 17 ANSWER (%):", sprintf("%.2f", ps8_q17_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# log(y) = intercept + slope*x -> 100*slope*(change in x).
# The intercept (1.1) does not matter here. Enter the number WITHOUT %.
# PRACTICE ANSWER: 2.00



# ============================================================================
# PS8 QUESTION 19 - LEVEL-LOG: CHANGE IN UNITS OF Y
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A level-log regression gives y_hat = 129 + 35.7*log(x). By about how many
# units does y change when x rises by 13%? (Round to the hundredths.)
#
# HINT: level-log: change in y ~= (beta1_hat / 100) * % change in x.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps8_q19_slope <- 35.7       # Coefficient multiplying log(x)
ps8_q19_x_pct <- 13         # 13 means 13% (NOT 0.13); use -13 for a fall
#
# ------------------------ RUN THIS CODE -----------------------------------
ps8_q19_answer <- ps8_q19_slope * ps8_q19_x_pct / 100
cat("PS8 QUESTION 19 ANSWER (units):", sprintf("%.2f", ps8_q19_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# y = intercept + slope*log(x) -> slope*(% change in x)/100.
# Ignore the intercept (129). Answer is in UNITS, not percent.
# PRACTICE ANSWER: 4.64



# ############################################################################
#                           PROBLEM SET 9
# ############################################################################



# ============================================================================
# PS9 QUESTION 1 - LOG-LOG: SALARY CHANGES FROM THOUSANDS TO DOLLARS
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A log-log regression of CEO salary on firm sales gives
# log(salary)_hat = beta0_hat + beta1_hat*log(sales), with salary in thousands
# of dollars. Salary is then measured in dollars instead. What happens?
#
# CHOICES:
# A. beta1_hat is divided by 1000.
# B. beta1_hat is multiplied by 1000.
# C. beta1_hat does not change; only beta0_hat changes.
# D. Neither beta0_hat nor beta1_hat changes.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps9_q1_multiplier <- 1000   # Thousands -> dollars = MULTIPLY salary by 1000
#
# ------------------------ RUN THIS CODE -----------------------------------
ps9_q1_intercept_change <- log(ps9_q1_multiplier)
cat("PS9 QUESTION 1 ANSWER: SLOPE UNCHANGED; ONLY INTERCEPT CHANGES.\n")
cat("Change in intercept:", round(ps9_q1_intercept_change, 4), "\n")
#
# HOW TO THINK ABOUT IT:
# log(1000*salary) = log(salary) + log(1000).
# Multiplying Y by a constant INSIDE a log adds a constant to the INTERCEPT.
# The SLOPE is unchanged. Select the WORDING if answer letters change.
# PRACTICE ANSWER: Choice C (third choice).



# ============================================================================
# PS9 QUESTION 5 - LOG-LOG: PERCENT CHANGE IN Y
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A log-log regression gives log(y)_hat = 1.6 + 0.55*log(x).
# By about what percent does y change when x FALLS by 13%?
# (Give a percent; it can be negative. Round to the hundredths.)
#
# HINT: log-log: % change in y ~= beta1_hat * % change in x.
# A fall means % change in x is negative.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps9_q5_slope <- 0.55        # Coefficient on log(x), NOT intercept 1.6
ps9_q5_x_pct <- -13         # FALLS 13% -> -13; RISES 13% -> +13
#
# ------------------------ RUN THIS CODE -----------------------------------
ps9_q5_answer <- ps9_q5_slope * ps9_q5_x_pct
cat("PS9 QUESTION 5 ANSWER (%):", sprintf("%.2f", ps9_q5_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# log(y) = intercept + slope*log(x) -> slope*(% change in x).
# Do NOT divide 13 by 100 for this approximate PERCENT answer.
# PRACTICE ANSWER: -7.15



# ============================================================================
# PS9 QUESTION 6 - WOOLDRIDGE ceosal1: TYPE THE REGRESSION YOURSELF
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars;
# roe = the firm's return on equity, in percent; sales = firm sales in
# millions of dollars). Run the log-level regression
# lm(log(salary) ~ roe, data = ceosal1). By about what percent does predicted
# salary change when return on equity rises by 5 points?
# (Give a percent. Use the UNROUNDED slope. Round to the hundredths.)
#
# HINT: Use the log-level formula with change in roe = 5.
#
# ------------------ READ / CHANGE AS NECESSARY ----------------------------
# This problem's given change is 5 ROE POINTS. If the quiz says 3 points,
# replace the final * 5 with * 3 in the command you personally type.
#
# ------------------ TYPE THESE YOURSELF IN RSTUDIO ------------------------
# STEP 1 (first time only): Install the package if it is not already installed.
#   Type this yourself: install.packages("wooldridge")
#
# STEP 2: Load the package.
#   Type this yourself: library(wooldridge)
#
# STEP 3: Load the dataset.
#   Type this yourself: data("ceosal1")
#
# STEP 4: Run the required regression and save it in an object called model.
#   Type this yourself: model <- lm(log(salary) ~ roe, data = ceosal1)
#
# STEP 5: Examine coefficients; use the coefficient named "roe", not intercept.
#   Type this yourself: coef(model)
#
# STEP 6: Calculate the approximate % change with the FULL slope (no early
# rounding). The 5 means the change in roe in PERCENTAGE POINTS.
#   Type this yourself: round(100 * coef(model)["roe"] * 5, 2)
#
# HOW TO THINK ABOUT IT:
# log-level: approximate % change in salary = 100 * slope_on_roe * change_roe.
# Use the unrounded slope straight from coef(model)["roe"].
# ENTER the final number without the % sign.
#
# IMPORTANT: This question has ONLY COMMENTS - no prewritten runnable code.



# ============================================================================
# PS9 QUESTION 9 - OMITTED-VARIABLE BIAS (MULTIPLE CHOICE)
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# In wage = beta0 + beta1*educ + U, which of the following is MOST likely to
# contribute omitted-variable bias to the OLS slope on educ?
#
# CHOICES:
# A. The day of the week on which the worker was surveyed.
# B. The worker's survey ID number.
# C. A random number the researcher assigned to each worker.
# D. The worker's ability, which raises wages and tends to be higher for
#    people with more education.
#
# ------------------------- HOW TO ANSWER ---------------------------------
# No numbers to change or R code to run: this is a CONCEPT question.
# Omitted-variable bias occurs when an omitted factor:
#   (1) affects the dependent variable (wage), AND
#   (2) is correlated with the included explanatory variable (education).
# Ability affects wages AND tends to be correlated with education.
# If ability is positively related to both, omitting it biases the slope UP.
# PRACTICE ANSWER: Choice D (fourth choice); use the wording if reordered.



# ============================================================================
# PS9 QUESTION 13 - WHAT MAKES VARIANCE OF THE OLS SLOPE SMALLER?
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Holding everything else fixed, which change makes Var(beta1_hat) SMALLER?
#
# CHOICES:
# A. More spread-out values of X (a larger SST_X).
# B. A larger error variance sigma^2.
# C. A larger intercept beta0.
# D. A smaller sample.
#
# ------------------------- HOW TO ANSWER ---------------------------------
# No numbers to change or R code to run: this is a CONCEPT question.
# Formula: Var(beta1_hat) = sigma^2 / SST_X
# LARGE denominator SST_X -> SMALLER variance (more precise slope).
# LARGE error variance sigma^2 -> LARGER variance.
# The intercept itself does not determine slope variance.
# PRACTICE ANSWER: Choice A (first choice); use wording if reordered.



# ============================================================================
# PS9 QUESTION 14 - STANDARD DEVIATION OF THE OLS SLOPE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# Under SLR1-SLR5, error standard deviation sigma = 2. The five X values
# are x = (2, 3, 4, 7, 9). Find sd(beta1_hat).
# (Round to the hundredths.)
#
# HINT: First SST_X = sum((x - mean(x))^2); then sd = sigma/sqrt(SST_X).
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps9_q14_sigma <- 2                      # Given error standard deviation
ps9_q14_x     <- c(2, 3, 4, 7, 9)      # ENTER ALL x values in c(...)
#
# ------------------------ RUN THIS CODE -----------------------------------
ps9_q14_sstx   <- sum((ps9_q14_x - mean(ps9_q14_x))^2)
ps9_q14_answer <- ps9_q14_sigma / sqrt(ps9_q14_sstx)
cat("PS9 QUESTION 14 SST_X:", ps9_q14_sstx, "\n")
cat("PS9 QUESTION 14 ANSWER:", sprintf("%.2f", ps9_q14_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# SST_X adds squared distances from the mean of x. Here SST_X = 34.
# Known population sigma -> sd(beta1_hat) = sigma / sqrt(SST_X).
# PRACTICE ANSWER: 0.34



# ============================================================================
# PS9 QUESTION 15 - ESTIMATED ERROR VARIANCE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A simple regression on n = 31 observations has SSR = 756.
# Find the estimate of the error VARIANCE, sigma_hat^2.
# (Round to the hundredths.)
#
# HINT: sigma_hat^2 = SSR/(n-2); use n-2, NOT n or n-1.
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps9_q15_n   <- 31            # Number of observations
ps9_q15_ssr <- 756           # Sum of squared residuals
#
# ------------------------ RUN THIS CODE -----------------------------------
ps9_q15_answer <- ps9_q15_ssr / (ps9_q15_n - 2)
cat("PS9 QUESTION 15 ANSWER (variance):", sprintf("%.2f", ps9_q15_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# Error variance estimate = SSR/(n-2) for a SIMPLE regression.
# Do NOT take a square root; the question asks for VARIANCE, not SD.
# PRACTICE ANSWER: 26.07



# ============================================================================
# PS9 QUESTION 16 - STANDARD ERROR OF THE OLS SLOPE
# ============================================================================
# ORIGINAL PRACTICE QUESTION:
# A simple regression on n = 10 observations has SSR = 299 and SST_X = 8.
# Find the standard error se(beta1_hat). (Round to the hundredths.)
#
# HINT: First sigma_hat = sqrt(SSR/(n-2)), then se = sigma_hat/sqrt(SST_X).
#
# --------------------- CHANGE THESE NUMBERS -------------------------------
ps9_q16_n    <- 10           # Number of observations
ps9_q16_ssr  <- 299          # Sum of squared residuals
ps9_q16_sstx <- 8            # Sum of squares of x (SST_X)
#
# ------------------------ RUN THIS CODE -----------------------------------
ps9_q16_sigma_hat <- sqrt(ps9_q16_ssr / (ps9_q16_n - 2))
ps9_q16_answer    <- ps9_q16_sigma_hat / sqrt(ps9_q16_sstx)
cat("PS9 QUESTION 16 ANSWER (standard error):", sprintf("%.2f", ps9_q16_answer), "\n")
#
# HOW TO THINK ABOUT IT:
# First estimate error standard deviation from SSR and n.
# Then divide by sqrt(SST_X) to get the SLOPE's standard error.
# Shortcut equivalent: sqrt(SSR/((n-2)*SST_X)).
# PRACTICE ANSWER: 2.16



# ############################################################################
# END OF SCRIPT. EACH QUESTION CAN BE RUN SEPARATELY.
# ############################################################################
