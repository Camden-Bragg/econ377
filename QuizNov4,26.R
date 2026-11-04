# ECONOMETRICS QUIZ 10 — PROBLEM SET 11
# Quiz questions: 1, 2, 3, 4, 6, 7, 9, 11, 14, 16, 17
# Run each question's code. Change only the numbers shown under CHANGE THESE.
# If needed, run install.packages("wooldridge") ONCE in the Console.
library(wooldridge)

# ====================================================================
# QUESTION 1
# A regression gives log(yhat) = 1.9 + 0.052*x1 - 0.013*x2.
# If x1 rises by 4 and x2 rises by 4, by about what percent does
# predicted y change? Give a percent; it can be negative.
# Round to the hundredths.
# CHANGE THESE (use the coefficients and changes on your quiz):
b1 <- 0.065
b2 <- -0.019
change_x1 <- 4
change_x2 <- 3
# ANSWER: approximate percent change (do NOT include the intercept).
round(100 * (b1*change_x1 + b2*change_x2), 2)

# ====================================================================
# QUESTION 2
# Data: wage1 (wage = average hourly earnings in dollars;
# educ = years of education; exper = years of experience;
# tenure = years with the current employer).
# Run reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1).
# By about what percent does predicted wage change when experience
# rises by 2 years and tenure rises by 1 year, holding education fixed?
# Give a percent, using the unrounded coefficients. Round to hundredths.
# CHANGE THESE if the quiz changes the two increases:
experience_change <- 2
tenure_change <- 1
# ANSWER:
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- coef(reg)
round(100 * (b["exper"]*experience_change + b["tenure"]*tenure_change), 2)

# ====================================================================
# QUESTION 3
# Data: wage1 (wage = average hourly earnings in dollars;
# educ = years of education; exper = years of experience;
# tenure = years with the current employer).
# Using the SAME regression of log(wage) on educ, exper, tenure,
# what is the predicted hourly wage, in dollars, of a worker with
# 12 years of education, 10 years of experience and 3 years of tenure?
# Round to the hundredths. Hint: predict log(wage) first, then exp().
# CHANGE THESE if the worker's characteristics are different:
education <- 12
experience <- 10
tenure <- 3
# ANSWER:
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
round(exp(predict(reg, newdata = data.frame(
  educ = education, exper = experience, tenure = tenure))), 2)

# ====================================================================
# QUESTION 4
# After an OLS multiple regression of Y on X1,...,Xk (with intercept)
# and residuals uhat_i = Y_i - Yhat_i, which statements are ALWAYS true?
# Select all that apply. In the notes list three properties.
# 1. The sample covariance between Y and the residuals is 0.
# 2. The sample average of the residuals is 0.
# 3. The sample covariance between each Xj and the residuals is 0.
# 4. The regression passes through the point of sample means:
#    Ybar = b0 + b1*X1bar + ... + bk*Xkbar.
# 5. Every residual is 0 when the sample is large enough.
# ANSWER: 2, 3, 4 (NOT 2, 4, 5).
# Statement 5 is false: large samples do not make every residual zero.
# REMEMBER: with an intercept, residuals average 0 and are uncorrelated
# with INCLUDED X variables (not generally with Y).

# ====================================================================
# QUESTION 6
# A multiple regression gives yhat = 5.8 + 0.9*x1 - 2.4*x2.
# The sample means are x1bar = 13.2 and x2bar = 2.5.
# What is the sample mean of y? Round to hundredths.
# CHANGE THESE:
intercept <- 3.6
b1 <- 0.8
b2 <- -1.2
mean_x1 <- 4.6
mean_x2 <- 2.9
# ANSWER: substitute the sample means into the fitted equation.
round(intercept + b1*mean_x1 + b2*mean_x2, 2)

# ====================================================================
# QUESTION 7
# Which regressions violate MLR3 (no perfect collinearity), so OLS
# cannot estimate them? Select all that apply.
# 1. A regression with 5 independent variables, estimated on 4 observations.
# 2. Wage on income in dollars and income in thousands of dollars.
# 3. colGPA on ACT and hsGPA, which are positively (but not perfectly)
#    correlated.
# 4. Wage on educ and exper in a sample of 526 workers, where both
#    educ and exper vary from worker to worker.
# 5. voteA on expendA, expendB and totexpend, where
#    totexpend = expendA + expendB.
# ANSWER: 1, 2, 5.
# REMEMBER: too few observations or an EXACT relationship among X's
# makes perfect collinearity; ordinary correlation does not.

# ====================================================================
# QUESTION 9
# Which situation makes MLR4, E(U | X1,...,Xk) = 0, fail (so that
# the X variables are endogenous)?
# 1. X1 and X2 are highly, but not perfectly, correlated.
# 2. The sample has only 50 observations.
# 3. Y is measured in thousands of dollars instead of dollars.
# 4. A variable that affects Y and is correlated with X1 is left
#    out of the regression.
# ANSWER: 4.
# REMEMBER: an omitted cause of Y that correlates with an included X
# can create omitted-variable bias.

# ====================================================================
# QUESTION 11
# Formula (3.23): b1_simple = b1_multiple + b2_multiple*delta1.
# The simple regression of Y on X1 gives b1_simple = -0.33.
# The multiple regression of Y on X1 and X2 gives b2_multiple = 1.47,
# and the slope from regressing X2 on X1 is delta1 = 0.89.
# Find the MULTIPLE-regression slope b1_multiple. Round to hundredths.
# CHANGE THESE NUMBERS:
b1_simple <- 0.85
b2_multiple <- 1.27
delta1 <- -0.16
# ANSWER: -1.64 with the original numbers.
round(b1_simple - b2_multiple * delta1, 2)

# ====================================================================
# QUESTION 14
# Data: bwght (bwght = baby's birth weight in ounces;
# cigs = cigarettes the mother smoked per day during pregnancy;
# faminc = family's income in thousands of dollars).
# Regress faminc (the variable left out of the simple regression in
# problem 12) on cigs: lm(faminc ~ cigs, data = bwght), to get delta1.
# Compute b2*delta1, where b2 is the faminc coefficient from the
# multiple regression in problem 13. Round to hundredths.
# Hint: b2*delta1 = b1(simple) - b1(multiple). Use that to check.
# ANSWER (run as is if datasets and variables are unchanged):
simple <- lm(bwght ~ cigs, data = bwght)
multiple <- lm(bwght ~ cigs + faminc, data = bwght)
extra <- lm(faminc ~ cigs, data = bwght)
round(coef(multiple)["faminc"] * coef(extra)["cigs"], 2)
# OPTIONAL CHECK (should match):
round(coef(simple)["cigs"] - coef(multiple)["cigs"], 2)

# ====================================================================
# QUESTION 16
# An omitted variable X2 lowers Y (b2 < 0), and X2 is NEGATIVELY
# correlated with included X1. The simple-regression slope on X1 is:
# 1. impossible to sign without the data
# 2. biased downward
# 3. biased upward
# 4. unbiased
# ANSWER: 3 (biased upward).
# RULE: bias sign = effect of omitted X2 on Y * correlation(X2, X1).
# Negative * negative = positive (UPWARD bias).
# If either sign changes on your quiz, multiply the two signs again.

# ====================================================================
# QUESTION 17
# A researcher regresses crop yield on fertilizer use across farms,
# leaving out land quality. Better land raises yield, and farmers
# tend to use more fertilizer on better land. The simple-regression
# fertilizer slope most likely:
# 1. is unbiased, because fertilizer and land quality are different variables
# 2. overstates the effect of fertilizer (upward bias)
# 3. is unbiased, because land quality is not in the regression
# 4. understates the effect of fertilizer (downward bias)
# ANSWER: 2 (upward bias).
# RULE: better land increases yield (+) and is associated with more
# fertilizer (+); positive * positive = positive (upward).

# ====================================================================
# QUESTION 15 (NOT ON THE DESIGNATED QUIZ; ADDED TO FIX PROBLEM SET)
# In the birth-weight regressions, the simple-regression cigs slope
# is about -0.51 and the multiple-regression slope (holding faminc fixed)
# is about -0.46. Which reading is correct?
# 1. Adding faminc makes the cigs slope biased, so simple is better.
# 2. About -0.46 per cigarette is smoking's association holding income
#    fixed; the extra -0.05 comes from income differences because heavier
#    smokers tend to have lower family incomes.
# 3. About -0.05 is the smoking effect; the other -0.46 is income.
# 4. The slopes differ only because of rounding.
# ANSWER: 2.
# The slope of cigs holding income fixed is -0.46, not -0.05.
