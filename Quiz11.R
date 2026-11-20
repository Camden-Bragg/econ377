# ECONOMETRICS QUIZ 11 - PROBLEM SET 13
# ONLY the 7 questions specified by your professor:
# 1, 3, 4, 7, 14, 17, 20
# Run the whole file once in RStudio (Source button).
# On quiz day, change the values below each CHANGE VALUES heading.
# These are concise paraphrases of the practice questions.

# Q1 - Why is the t statistic t-distributed rather than standard normal?
# Answer: estimated standard error uses sigma-hat, which varies by sample.
cat('\nQ1: Choose the answer saying estimated sigma-hat varies by sample.\n')

# Q3 - Residual degrees of freedom in multiple regression
# CHANGE VALUES: sample size n, number of regressors K (excluding intercept)
n3 <- 137
K3 <- 2
cat('\nQ3 degrees of freedom:', n3 - K3 - 1, '\n')

# Q4 - Compute t statistic for coefficient hypothesis H0: beta = beta0
# CHANGE VALUES
b4 <- -0.73
se4 <- 0.86
beta0_4 <- 0
cat('\nQ4 t statistic:', round((b4 - beta0_4) / se4, 2), '\n')

# Q7 - One-sided right-tail t test, H1: beta > beta0
# CHANGE VALUES: use the critical t given in the question, if supplied
observed_t7 <- 1.52
critical_t7 <- 1.71
cat('\nQ7:', if (observed_t7 > critical_t7) 'REJECT H0' else 'FAIL TO REJECT H0', '\n')
# If the quiz instead gives significance level and degrees of freedom:
alpha7 <- 0.05
df7 <- 25
cat('Q7 calculated right-tail critical t:', round(qt(1 - alpha7, df7), 2), '\n')

# Q14 - Wooldridge bwght: t statistic for cigarettes in a regression
# Installs the data package only if missing. Requires internet for first install.
if (!requireNamespace('wooldridge', quietly = TRUE)) {
  install.packages('wooldridge')
}
data('bwght', package = 'wooldridge')
# CHANGE REGRESSION if quiz specifies different variables or dataset
fit14 <- lm(bwght ~ cigs + faminc, data = bwght)
print(summary(fit14)$coefficients)
cat('\nQ14 cigs t statistic:', round(coef(summary(fit14))['cigs', 't value'], 2), '\n')
# For one-sided test H1: cigs < 0 at 1%:
alpha14 <- 0.01
t14 <- coef(summary(fit14))['cigs', 't value']
cutoff14 <- qt(alpha14, df.residual(fit14))
cat('Q14 left-tail cutoff:', round(cutoff14, 2), '\n')
cat('Q14 decision:', if (t14 < cutoff14) 'REJECT H0' else 'FAIL TO REJECT H0', '\n')

# Q17 - Rescale coefficient when X changes from percent (40) to decimal (0.40)
# CHANGE VALUE: original coefficient on percent-scale X
slope17 <- 0.251
cat('\nQ17 slope after converting X to decimal:', round(slope17 * 100, 2), '\n')
# If converting the OTHER direction (decimal X to percent X), divide by 100.

# Q20 - Wooldridge hprice1: predicted house price from log(price) model
# CHANGE REGRESSION if quiz specifies a different model.
data('hprice1', package = 'wooldridge')
fit20 <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1)
print(coef(fit20))
# CHANGE VALUES: house square footage and bedrooms
sqrft20 <- 2000
bdrms20 <- 3
log_prediction20 <- predict(fit20, newdata = data.frame(sqrft = sqrft20, bdrms = bdrms20))
price_thousands20 <- as.numeric(exp(log_prediction20))
cat('\nQ20 predicted price ($1,000s):', round(price_thousands20, 2), '\n')
cat('Q20 predicted price ($):', round(price_thousands20 * 1000, 2), '\n')
