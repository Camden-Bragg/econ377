###############################################################################
# ECN 377 -- EXAM II | RELIABLE SEARCHABLE NOTES (NO AI / NO INTERNET)
# Study-guide coverage: Chapters/days through 21; Chapter 4 t-tests excluded.
# Built around Exam I style, PS8-12 and the PS12 five highlighted question types.
#
# QUICK START (Mac RStudio): OPEN THIS FILE -> CLICK SOURCE ONCE.
# At the > CONSOLE prompt:
#    ask("find the standard error")
#    ask("Compute SSTj for Xj=bdrms using hprice1")
#    ask("Which regression gives Rj2 for the exper coefficient?")
#    ask()       # type/paste a one-line question when prompted
#    ask_paste() # multiline question + choices, finish with DONE
#    topics()    # all note sections
#    note("COEF_SE") # open an exact note by ID
#    test_search()  # built-in routing tests
#
# IMPORTANT: This is an OFFLINE NOTES INDEX, not an AI model.
# It does not choose multiple-choice letters or guarantee correct routing
# for arbitrary unfamiliar wording. Broad requests show a decision guide.
# Full calculation methods and variable meanings are printed together.
# No packages needed for notes. Only Wooldridge dataset helpers need wooldridge.
###############################################################################

.exam_notes <- list()
.add <- function(id, title, clues, meaning, facts, code = character(), symbols = character()) {
  .exam_notes[[id]] <<- list(id=id, title=title, clues=clues,
                             meaning=meaning, facts=facts, code=code,
                             symbols=symbols)
}

# =================== 01. BASIC PROBABILITY ================================
.add("PROB", "Expected value / population variance / probability table",
     c("expected value", "expected outcome", "probability table", "population variance", "expected payoff", "variance of x", "var x plus", "var a times"),
     "Use when a table assigns PROBABILITIES to possible values of X (not raw sample observations).",
     c("Expected value E(X) = sum of each value times its probability.",
       "Population variance Var(X) = sum[p * (x - E(X))^2]. SD(X) = sqrt(Var(X)).",
       "E(aX+b) = a E(X)+b; Var(X+b)=Var(X); Var(aX)=a^2 Var(X).",
       "If X and Y independent: Var(X+Y)=Var(X)+Var(Y). Otherwise include 2Cov(X,Y).",
       "The population mean is a FIXED but usually unknown number; the sample mean is RANDOM across samples."),
     c("x <- c(0, 1, 4); p <- c(.25, .50, .25)",
       "sum(x*p)                                # E(X)",
       "sum(p*(x-sum(x*p))^2)                  # population Var(X)",
       "sqrt(sum(p*(x-sum(x*p))^2))            # population SD"),
     c("x: possible outcomes; p: corresponding probabilities (must sum to 1)."))

# =================== 02. SAMPLE STATISTICS ================================
.add("SAMPLE", "Sample mean, sample SD, variance, group averages",
     c("sample mean", "sample standard deviation", "sample variance", "average wage", "within a group", "mean within", "sample statistic", "standard deviation of observations", "sample size"),
     "Use for observed data, sample statistics, and group-specific averages.",
     c("Sample mean = sum(x)/n. Sample variance = sum((x-xbar)^2)/(n-1). Sample SD is its square root.",
       "R's sd() and var() use the SAMPLE denominator (n-1).",
       "A sample statistic (xbar, s) changes across random samples. A population parameter (mu, sigma) is fixed.",
       "For groups, use a logical filter; missing values may need na.rm=TRUE."),
     c("x <- c(3,5,8,8); mean(x); sd(x); var(x); length(x)",
       "# After loading wage1: mean(wage1$wage[wage1$female==1], na.rm=TRUE)",
       "# Or for any df: mean(df$variable[df$group==1], na.rm=TRUE)"),
     c("n: number of observations; xbar: sample average; s: sample SD."))

# =================== 03. PERCENT VS POINTS ================================
.add("PERCENT", "Percent change versus percentage-point change",
     c("percentage point", "percent change", "rate rises", "rate falls", "from percent to", "percent versus", "percentage change"),
     "A change from 4% to 6% is +2 percentage POINTS, but a +50% RELATIVE change.",
     c("Percentage-point change = new percentage - old percentage.",
       "Percent change = 100*(new-old)/old, with old and new measured in the same units.",
       "A variable measured in percentage points: a one-unit rise = one percentage point, NOT 1% relative change."),
     c("old <- 4; new <- 6", "new-old                     # percentage points",
       "100*(new-old)/old           # relative percent change"))

# =================== 04. RESCALING ========================================
.add("RESCALE", "Changing units of Y or X: slope and intercept",
     c("rescale", "rescaling", "change units", "dollars to thousands", "thousands to dollars", "years to months", "percent to decimal", "decimal instead", "unit of measurement"),
     "Use when a question actually CHANGES units of a variable, not merely describes its units.",
     c("If new_Y=a*old_Y and new_X=b*old_X: new slope=(a/b)*old slope.",
       "Multiply Y by a: all coefficients (including intercept) multiply by a.",
       "Multiply one X by b: ONLY its slope divides by b; intercept unchanged (if only a pure rescaling).",
       "R-squared and predictions expressed in the SAME ORIGINAL units do not change from simple rescaling.",
       "Example: annual earnings in dollars -> thousands (a=.001); years education -> months (b=12)."),
     c("old_slope <- 3000; a <- .001; b <- 12", "old_slope*a/b           # 0.25 thousand dollars per month"),
     c("a = new_Y / old_Y; b = new_X / old_X; slope units = units of Y per unit of X."))

# =================== 05. LEVEL-LEVEL ======================================
.add("LEVEL_LEVEL", "Level-level model: values and changes in Y units",
     c("level level", "y equals b0", "slope in units", "one unit increase", "predicted value", "constant term", "intercept"),
     "Neither Y nor the relevant X is logged. Coefficients describe changes in Y UNITS.",
     c("yhat=b0+b1*x1+... . Substitute ALL given X values to predict Y.",
       "If X1 rises by d1 and X2 falls by d2, predicted change = b1*d1 + b2*(-d2).",
       "For a predicted CHANGE, intercept cancels, and held-fixed variables contribute zero.",
       "Ceteris paribus: the slope b1 is the change in predicted Y for +1 X1, holding other X fixed."),
     c("b0 <- 7; b1 <- 2.9; b2 <- -.5", "x1 <- 3; x2 <- 6",
       "b0+b1*x1+b2*x2                # predicted value",
       "dx1 <- 1; dx2 <- -6", "b1*dx1+b2*dx2                 # predicted change"),
     c("b0: intercept; bj: slope for Xj; dxj: new value minus old value of Xj."))

# =================== 06. LOG-LEVEL + MULTIPLE CHANGES =====================
.add("LOG_LEVEL", "LOG-LEVEL: log(Y), one or MANY X changes; percent wage change",
     c("log level", "log wage", "log salary", "log price", "percent does predicted", "predicted percent change", "percentage change in predicted", "log y changes", "log wage changes", "multiple variables changing", "education increases experience decreases", "wage increase percent"),
     "Y is logged, X variables are in levels: combine ALL changing slopes, then convert to percent.",
     c("log(yhat) = b0+b1*x1+b2*x2+... . Define dlogY=sum(bj*changeXj).",
       "Approximate PERCENT change in Y = 100*dlogY. This is what an 'approximately' question usually wants.",
       "Exact percent change in fitted exponentiated Y = 100*(exp(dlogY)-1).",
       "If tenure is fixed, change tenure=0. Negative X change multiplied by negative coefficient becomes positive.",
       "A single +1 unit X raises Y about 100*bj percent; not bj units.",
       "Predict level from a fitted log value with exp(log_yhat). Remember exp(fitted log Y) is not automatically E(Y|X)."),
     c("# Example: log(wage)=0.75+.065*educ-.012*exper+.018*tenure",
       "b <- c(educ=.065, exper=-.012, tenure=.018)",
       "change <- c(educ=3, exper=5, tenure=0)",
       "dlogY <- sum(b*change)",
       "100*dlogY                         # approx percentage change: 13.50%",
       "100*(exp(dlogY)-1)              # exact finite fitted-percent change",
       "# Predict wage level at specified X values:",
       "exp(.75+.065*12-.012*5+.018*2)"),
     c("dlogY: change in predicted log(Y); bj: coefficient of Xj; changeXj: new minus old Xj."))

# =================== 07. LEVEL-LOG ========================================
.add("LEVEL_LOG", "LEVEL-LOG: Y in levels, log(X); change Y in UNITS",
     c("level log", "log income", "log sqrft", "log sales as x", "percent increase in income", "increase in x percent", "income rises percent", "points from income"),
     "Y is NOT logged; X is inside log(): a percentage change in X changes Y in UNITS.",
     c("Model: Y=b0+b1*log(X). A 1% increase in X predicts ~b1/100 units of Y.",
       "Approximate change Y = b1*(percentage change in X / 100).",
       "Exact finite change Y = b1*log(newX/oldX).",
       "If other level variables also change, add their b*deltaX terms.",
       "Do not call the answer a % of Y unless Y itself is logged."),
     c("b_logx <- 8; pct_x <- 10", "b_logx*pct_x/100             # approx change in Y units",
       "old_x <- 100; new_x <- 110", "b_logx*log(new_x/old_x)     # exact change in Y units",
       "# If classsize rises by 2 and coefficient is -.7:",
       "b_logx*.10 + (-.7)*2"),
     c("pct_x: percent number like 10 (not .10); b_logx: slope multiplying log(X)."))

# =================== 08. LOG-LOG ==========================================
.add("LOG_LOG", "LOG-LOG: elasticity; percent of Y for percent of X",
     c("log log", "elasticity", "log sales", "log income and log", "both variables logged", "percent of y from percent of x"),
     "Both Y and X are logged; b1 is an ELASTICITY.",
     c("A 1% change in X predicts approximately b1% change in Y.",
       "Approx percent change Y = b1*(percent change X).",
       "Exact finite percent change = 100*((newX/oldX)^b1 - 1).",
       "Holding other X fixed is required for partial effects in multiple regression."),
     c("elasticity <- .8; pct_x <- 5", "elasticity*pct_x                 # approx: 4%",
       "100*((1+pct_x/100)^elasticity-1) # exact finite %"),
     c("elasticity=b1: percent change in Y for a 1% change in X, approximately."))

# =================== 09. EXP / LOG UNDO ===================================
.add("EXP", "Undo logarithm: exp() to predict original Y",
     c("undo log", "antilog", "exponentiate", "back to y", "predict wage level", "predict salary", "exp", "predicted log", "from log to wage"),
     "When the regression predicts log(Y) and the question asks for the level Y, apply exp().",
     c("If predicted log(wage)=z, predicted wage on the exponentiated-fit scale = exp(z).",
       "Plug all X values into the fitted equation FIRST; then use exp() ONCE.",
       "Strictly, exp(E[log Y|X]) need not equal E[Y|X] because of retransformation bias.",
       "Don't take exp() if the question requests the predicted LOG of Y."),
     c("z <- .58+.083*16", "exp(z)                      # exponentiated fitted log(wage)"),
     c("exp(): exponential function e^z; log(): natural logarithm in R."))

# =================== 10. GENERAL PREDICTIONS ==============================
.add("PREDICT", "Recognize predicted value vs predicted change, ANY model",
     c("predicted change", "prediction", "predict change", "how much does y change", "rises by", "falls by", "holding fixed", "ceteris paribus", "from x to", "increases from", "decreases from"),
     "Look at whether the question asks a NEW VALUE or HOW MUCH THE VALUE CHANGES.",
     c("Predicted VALUE: use intercept and every stated X level.",
       "Predicted CHANGE: use slopes multiplied by CHANGES; intercept is irrelevant.",
       "Level Y and level X: change Y=sum(bj*changeXj) Y units.",
       "Log Y and level X: approximate percent Y=100*sum(bj*changeXj).",
       "Level Y and log X: add bj*log(newX/oldX) to level-regressor terms.",
       "Log Y and log X: log change Y = bj*log(newX/oldX); approximate % rule for small changes.",
       "Always count EACH variable that changes, including ones with negative coefficients.",
       "See LOG_LEVEL / LEVEL_LOG / LOG_LOG for more detailed formulas and ready examples."),
     c("# Several LEVEL explanatory variables change:", "b <- c(x1=2.9,x2=-.5); dx <- c(x1=1,x2=-6)",
       "sum(b*dx)                    # change in Y units",
       "# If outcome is LOG(Y), instead use 100*sum(b*dx)"),
     c("delta or dX: NEW minus OLD; b: estimated slope; Yhat: fitted/predicted Y."))

# =================== 11. OLS SLOPE AND INTERCEPT ==========================
.add("SLOPE_SLR", "Simple OLS slope, intercept, sample covariance and variance",
     c("simple regression slope", "sxy", "sx2", "slope estimate", "ols slope formula", "covariance x y", "estimate beta1", "find intercept"),
     "Simple regression: one X. OLS chooses the fitted line that minimizes sum of squared residuals.",
     c("Slope b1 = Cov_sample(X,Y) / Var_sample(X), equivalently sum centered X*centered Y / sum centered X^2.",
       "Intercept b0 = mean(Y) - b1*mean(X).",
       "Positive residual means actual Y greater than fitted Y; negative means overprediction.",
       "Avoid confusing POPULATION coefficients beta with their SAMPLE estimates beta_hat."),
     c("x <- c(1,2,3,4); y <- c(2,4,4,7)",
       "b1 <- cov(x,y)/var(x)", "b0 <- mean(y)-b1*mean(x)",
       "c(intercept=b0,slope=b1)", "coef(lm(y~x))         # cross-check"),
     c("Cov(X,Y): sample covariance; Var(X): sample variance; b0: intercept; b1: slope."))

# =================== 12. RESIDUALS ========================================
.add("RESID", "Fitted values, residuals, and OLS sample properties",
     c("residual", "fitted value", "actual minus predicted", "overpredict", "underpredict", "residuals average", "covariance with residual", "passes through mean", "point of means", "sum residuals"),
     "Residual = observed Y - predicted Y. With an intercept, OLS has exact SAMPLE identities.",
     c("ui_hat=Yi-Yi_hat. Positive: model UNDERpredicted; negative: model OVERpredicted.",
       "OLS (with an intercept): sum residuals = mean residuals = 0.",
       "OLS residuals have zero sample covariance with EVERY included X variable.",
       "At the sample means of all X variables, fitted Y equals sample mean of Y.",
       "These identities do NOT imply true error U has conditional mean zero or that every residual is zero.",
       "OLS minimizes SSR=sum(residual^2), NOT sum(abs(residual))."),
     c("actual <- 10; fitted <- 12; actual-fitted  # -2: overprediction",
       "# With fitted model reg:", "head(fitted(reg)); head(residuals(reg)); sum(residuals(reg))",
       "sum(residuals(reg)^2)              # SSR"),
     c("Y: observed outcome; Yhat: fitted outcome; uhat: fitted residual; U: unobserved population error."))

# =================== 13. GOODNESS OF FIT ==================================
.add("R2_FIT", "Overall R-squared, SST = SSE + SSR, goodness of fit",
     c("goodness of fit", "what does r squared", "overall r squared", "explained variation", "unexplained variation", "sse", "sst equals", "proportion explained", "high r squared", "low r squared"),
     "Overall R^2 is how much SAMPLE variation in Y the fitted regression explains.",
     c("SST=sum((Y-mean(Y))^2): total variation in Y.",
       "SSE=sum((Yhat-mean(Y))^2): explained variation (lecture notation).",
       "SSR=sum((Y-Yhat)^2): unexplained residual variation (lecture notation).",
       "With intercept: SST=SSE+SSR and R^2=SSE/SST=1-SSR/SST.",
       "R^2=.30 means 30% explained, 70% unexplained, in that sample.",
       "High R^2 alone does NOT prove causation, unbiasedness, or a valid model.",
       "Lecture uses SSR for sum of SQUARED RESIDUALS; some other texts use different initials."),
     c("SST <- 1000; SSR <- 350; 1-SSR/SST",
       "# If reg already exists: summary(reg)$r.squared"),
     c("SST: total in Y; SSE: explained in fitted Y; SSR: residual unexplained."))

# =================== 14. UNBIASEDNESS =====================================
.add("UNBIASED", "Expected values of OLS estimators; unbiasedness",
     c("unbiased", "unbiasedness", "expected value of beta", "averages across samples", "repeated sampling", "estimator random", "sampling distribution"),
     "An estimator is unbiased if its average over repeated samples equals the true population parameter.",
     c("E(beta_hat_j)=beta_j under SLR1-4 / MLR1-4, respectively.",
       "Unbiased does NOT mean the estimate equals the truth in every sample.",
       "Estimator beta_hat varies randomly across samples; true population beta is fixed.",
       "Violating zero conditional mean (SLR4/MLR4) threatens unbiasedness.",
       "Homoskedasticity (MLR5) is NOT required for the unbiasedness conclusion."),
     c("# A conceptual question needs a rule, not R code."),
     c("E(): expected value across random samples; beta_hat: sample estimator; beta: true population coefficient."))

# =================== 15. SLR VARIANCE =====================================
.add("SLR_VAR", "Simple regression slope variance; sample size; precision",
     c("variance of slope in simple", "variance of beta1", "variance of ols slope", "sigma squared over sst", "spread in x", "larger sample", "sample k times", "sampling standard deviation"),
     "In SIMPLE regression under SLR1-5: Var(beta1_hat|X)=sigma^2/SST_X.",
     c("SST_X=sum((Xi-xbar)^2)=(n-1)*sX^2.",
       "More error variance sigma^2 -> less precise slope / larger variance.",
       "More spread in X / larger SST_X -> more precise slope / smaller variance.",
       "For otherwise comparable data, sample size multiplied by k approximately multiplies SST_X by k; SD(beta1_hat) divides by sqrt(k).",
       "Variance has squared units. Standard deviation/standard error is square root of variance."),
     c("sigma2 <- 25; n <- 51; sx <- 2", "sstx <- (n-1)*sx^2",
       "var_beta <- sigma2/sstx", "sqrt(var_beta)"),
     c("sigma2: population error variance; SST_X: variation in X; n: sample count; sX: sample SD."))

# =================== 16. MULTIPLE REGRESSION ==============================
.add("MLR", "Multiple regression ceteris paribus; predictions and assumptions",
     c("multiple regression", "mlr", "holding others", "control for", "controlling for", "ceteris paribus", "partial effect", "one independent variable", "interpret coefficient"),
     "Multiple linear regression models one outcome Y using K independent variables.",
     c("Y=b0+b1X1+...+bK XK+U; holding all OTHER Xs fixed, +1 in Xj changes fitted Y by bj units (for level Y).",
       "The fitted model is linear in PARAMETERS even if X includes squares/logs.",
       "Adding a relevant control can reduce omitted-variable bias if remaining exogeneity holds.",
       "MLR1: linear in parameters. MLR2: random sample. MLR3: no perfect collinearity.",
       "MLR4: E(U|X1,...,XK)=0 (zero conditional mean / exogeneity).",
       "MLR5: constant error variance given X (homoskedasticity).",
       "MLR1-4 -> unbiasedness; MLR1-5 -> usual variance formula and Gauss-Markov BLUE."),
     c("# Using loaded dataset df:", "reg <- lm(y ~ x1 + x2, data=df)",
       "coef(reg); summary(reg)", "# A changed X affects prediction through its own slope and any OTHER X changes."),
     c("Y: dependent/outcome; Xj: individual predictor; K: number of predictors excluding intercept; U: population error."))

# =================== 17. PERFECT COLLINEARITY =============================
.add("MLR3", "MLR3: no perfect collinearity versus high correlation",
     c("mlr3", "perfect collinearity", "dummy variable trap", "all dummies", "exact linear relationship", "highly correlated predictors", "high correlation", "linearly dependent", "duplicate regressor", "multicollinearity"),
     "MLR3 fails when one included regressor is an EXACT linear combination of others (including intercept).",
     c("Examples: income in dollars AND income in thousands; a total equal to sum of included components.",
       "With an intercept, including a dummy for EVERY mutually exclusive exhaustive group causes the dummy-variable trap.",
       "If n < K+1, no full column rank; in practice need n > K+1 for positive residual df.",
       "Two predictors correlation .95 is NOT perfect collinearity unless exact +/-1 relationship or other exact combination.",
       "Perfect collinearity prevents unique estimation of all slopes; high-but-imperfect collinearity increases SEs.",
       "Fix perfect collinearity by removing redundant predictor or one reference-category dummy."),
     c("# Check model when fitting: alias(reg); coef(reg)",
       "# NA coefficients may indicate perfect collinearity."),
     c("MLR3: no exact linear relationships among included explanatory variables."))

# =================== 18. ZERO CONDITIONAL MEAN ============================
.add("MLR4", "MLR4: zero conditional mean, exogeneity, endogeneity",
     c("mlr4", "zero conditional mean", "endogenous", "endogeneity", "exogenous", "error correlated", "omitted factor correlated", "wrong functional form", "omitted variable"),
     "MLR4 requires E(U|X1,...,XK)=0; unobserved determinants must have zero mean conditional on regressors.",
     c("If higher unobserved ability tends to accompany higher education and affects wages, MLR4 may fail.",
       "An omitted factor creates OVB when it (1) affects Y and (2) is associated with an included X.",
       "Wrong functional form and certain measurement/reverse-causality problems can also threaten exogeneity.",
       "A variable correlated with the model's error is endogenous; exogenous means zero conditional mean holds.",
       "MLR4 failure can bias OLS; adding controls does not magically guarantee causality."),
     c("# Conceptual rule: E(U | X1,...,XK)=0. No calculation automatically tests this."),
     c("U: unobserved error; Xs: included regressors; E(U|Xs): average error among units with specified X values."))

# =================== 19. OMITTED VARIABLE BIAS SIGNS =======================
.add("OVB_SIGN", "Omitted variable bias: two-legged SIGN table and stories",
     c("upward bias", "downward bias", "direction of bias", "omitted variable bias", "positive bias", "negative bias", "two legged", "omitted ability", "ability bias", "omitted income", "confounding"),
     "Ask TWO independent sign questions: omitted Z's effect on Y, and Z's correlation with included X.",
     c("Bias sign = sign(effect of Z on Y) TIMES sign(correlation of Z with X), in the simple two-regressor setup.",
       "Effect Z->Y +, Corr(X,Z) + => UPWARD bias in X slope.",
       "Effect Z->Y +, Corr(X,Z) - => DOWNWARD bias.",
       "Effect Z->Y -, Corr(X,Z) + => DOWNWARD bias.",
       "Effect Z->Y -, Corr(X,Z) - => UPWARD bias.",
       "'Upward' means simple estimate is MORE POSITIVE than target true slope, not necessarily positive.",
       "No OVB from that Z if its true partial effect is 0 OR it is uncorrelated with included X.",
       "Example from lecture: ability raises wage and likely correlates positively with education => upward bias on education.",
       "Low-income lunch-program enrollment may proxy poverty; a negative association with scores does not alone prove causality."),
     c("# Numerical sign checks, if given coefficients:", "effect_z <- -2; slope_z_on_x <- .3",
       "effect_z*slope_z_on_x       # negative: downward bias"),
     c("Z: omitted factor; X: included predictor; Y: outcome; bias: E(simple b1)-true b1."))

# =================== 20. OMITTED VARIABLE IDENTITY ========================
.add("OVB_FORMULA", "Formula (3.23): simple vs multiple regression coefficient",
     c("formula 3.23", "3 23", "simple versus multiple slope", "simple and multiple coefficients", "restricted coefficient", "slope with omitted", "recover slope", "bias formula", "delta tilde", "simple regression versus multiple"),
     "Exact SAMPLE identity when model Y~X1+X2 is compared with simple Y~X1.",
     c("simple_slope_X1 = multiple_slope_X1 + (multiple_coefficient_X2)*(slope from X2~X1).",
       "In lecture notation: beta1_tilde = beta1_hat + beta2_hat*delta1_tilde.",
       "To FIND MULTIPLE slope: beta1_hat = beta1_tilde - beta2_hat*delta1_tilde.",
       "To FIND SIMPLE slope: beta1_tilde = beta1_hat + beta2_hat*delta1_tilde.",
       "delta1_tilde comes from REGRESSING OMITTED X2 on INCLUDED X1 (not Y).",
       "Population bias expression under assumptions: Bias(simple b1) = beta2*delta1."),
     c("simple <- .40; b2 <- -.60; delta <- .25",
       "multiple <- simple - b2*delta; multiple   # 0.55",
       "# Reverse: multiple <- .55; multiple+b2*delta   # 0.40"),
     c("tilde: coefficient in simple/restricted model; hat: multiple-model coefficient; delta: X2-on-X1 slope."))

# =================== 21. SIGMA / ERROR SD =================================
.add("REG_ERR", "PS12 #3/#4/#9: regression error SD sigma-hat; SSR and df",
     c("standard error of the regression", "residual standard error", "estimated error standard deviation", "sigma hat", "sigmahat", "error sd", "regression error sd", "ssr divided by df", "sum of squared residuals", "regression has k", "degrees of freedom"),
     "Requested quantity sigma-hat is the RESIDUAL standard deviation of Y around the fitted regression, not a specific coefficient SE.",
     c("For a regression WITH intercept and K explanatory variables: df=n-K-1.",
       "SSR=sum(residuals^2) (this course's notation). Error variance estimate sigmahat^2=SSR/df.",
       "Regression residual standard error sigmahat=sqrt(SSR/df).",
       "Example: n=44, K=3, SSR=1498 => sqrt(1498/40)=6.12.",
       "PS12 Q9: use residuals from the FULL MODEL, NOT auxiliary model.",
       "RStudio summary(reg) reports 'Residual standard error ... on df degrees of freedom'.",
       "If asked what sigmahat estimates: true error-term SD sigma, NOT R2 or SD of beta-hat."),
     c("n <- 44; K <- 3; SSR <- 1498",
       "df <- n-K-1", "sigmahat <- sqrt(SSR/df); round(sigmahat,2)",
       "# If reg exists: sqrt(sum(residuals(reg)^2)/df.residual(reg))",
       "# Same result: sigma(reg)"),
     c("n: USED observations; K: # independent variables (exclude intercept); SSR: sum residual^2; df: n-K-1; sigmahat: residual SD."))

# =================== 22. DEGREES OF FREEDOM ================================
.add("DF", "PS12 #8: degrees of freedom n-K-1",
     c("how many degrees of freedom", "residual degrees of freedom", "df equal", "df n k", "number of independent variables", "on 84 degrees"),
     "Residual degrees of freedom subtract one per estimated parameter, INCLUDING intercept.",
     c("With intercept, K slopes, n usable observations: df=n-K-1.",
       "Example hprice1: price on sqrft+bdrms+lotsize has n=88, K=3, so df=84.",
       "If some variables contain NA, use nobs(reg) instead of nrow(full dataset)."),
     c("n <- 88; K <- 3; n-K-1", "# If model reg exists: df.residual(reg); nobs(reg)"),
     c("K: predictors excluding the intercept; n: complete observations used in fit."))

# =================== 23. SST OF REGRESSOR =================================
.add("SSTJ", "PS12 #5/#10: SSTj, variation of the TARGET Xj",
     c("compute sstj", "find sstj", "sst of bdrms", "sstj for", "total variation of", "variation around mean", "sum squared deviations", "sum of squared deviations", "sst of x", "sample standard deviation of xj"),
     "SSTj measures total sample variation in the chosen EXPLANATORY variable Xj, NOT Y.",
     c("SSTj=sum((Xj-mean(Xj))^2)=(n-1)*sampleSD(Xj)^2.",
       "Example with n=43 and sample SD 5.9: (42)*(5.9^2)=1462.02.",
       "If Xj=bdrms, compute using BEDROOM counts, never house price.",
       "Use observations included in the FULL REGRESSION when computing a coefficient SE; missing Y/other X values can change this sample.",
       "No regression is needed to find SSTj from a complete X vector."),
     c("n <- 43; sx <- 5.9; (n-1)*sx^2",
       "# For hprice1 (package must already be installed):",
       "data('hprice1', package='wooldridge')",
       "sum((hprice1$bdrms - mean(hprice1$bdrms))^2)",
       "# OR the built-in helper: sst_data('hprice1','bdrms')",
       "# General with clean observations xj: sum((xj-mean(xj))^2)"),
     c("Xj: independent variable of interest; n: number of included observations; sx: SAMPLE standard deviation of Xj."))

# =================== 24. AUXILIARY R-SQUARED ===============================
.add("AUX_R2", "PS12 #2/#11: auxiliary Rj2; WHICH regression?",
     c("which regression gives", "regression gives rj2", "auxiliary regression", "auxiliary r squared", "rj2", "r j squared", "regressing xj on", "other independent variables explain", "regress exper on", "r square for coefficient"),
     "Rj2 comes from a SECOND regression to measure how predictable Xj is from the OTHER independent variables.",
     c("Original: wage ~ educ + exper + tenure. If target is exper, auxiliary: exper ~ educ + tenure.",
       "Put TARGET Xj on LEFT; all OTHER original Xs on RIGHT. Exclude original Y (wage).",
       "Use summary(aux)$r.squared; do NOT use overall R-squared from wage regression.",
       "If target is bdrms in price~sqrft+bdrms+lotsize: auxiliary bdrms~sqrft+lotsize.",
       "Rj2 close to one means severe near-collinearity -> bigger SE of Xj coefficient.",
       "For a correctly matched hand calculation, use EXACT SAME ROWS as full fit."),
     c("# Original model: wage ~ educ + exper + tenure",
       "data('wage1', package='wooldridge')",
       "aux <- lm(exper ~ educ + tenure, data=wage1)", "summary(aux)$r.squared",
       "# Other example with hprice1 (load that dataset first):",
       "# summary(lm(bdrms ~ sqrft+lotsize, data=hprice1))$r.squared"),
     c("Xj: regressor whose coefficient SE we want; Rj2: auxiliary regression fit, NOT original Y model fit."))

# =================== 25. COEFFICIENT STANDARD ERROR ========================
.add("COEF_SE", "PS12 #6/#12/#13: standard error of ONE coefficient",
     c("standard error of the coefficient", "standard error of a coefficient", "standard error of beta", "se of beta", "se betaj", "se beta", "std error column", "standard error for bdrms", "precision of a slope", "standard error of exper", "put pieces together", "compute se"),
     "This SE measures how precisely ONE SLOPE is estimated. It is not sigmahat and not the original model R-squared.",
     c("Formula: se(beta_j_hat)=sigmahat/sqrt(SSTj*(1-Rj2)).",
       "Step 1: fit full model and get sigmahat=sqrt(SSR/(n-K-1)).",
       "Step 2: SSTj=sum((Xj-mean(Xj))^2) on SAME full-model rows.",
       "Step 3: regress target Xj on remaining Xs to get auxiliary Rj2.",
       "Step 4: combine all UNROUNDED quantities; round at END to 2 decimals.",
       "Given sigmahat=6.5,SSTj=836,Rj2=.77 => SE approximately 0.47.",
       "When fitted reg exists, summary(reg)$coefficients[\"bdrms\",\"Std. Error\"] gives same conventional SE.",
       "MLR1-5 support the usual homoskedastic formula; robust standard errors differ under heteroskedasticity."),
     c("sigmahat <- 6.5; SSTj <- 836; Rj2 <- .77",
       "sigmahat/sqrt(SSTj*(1-Rj2))",
       "# OR if n,K,SSR given rather than sigmahat:",
       "n <- 68; K <- 3; SSR <- 2560; SSTj <- 120; Rj2 <- .25",
       "sqrt((SSR/(n-K-1))/(SSTj*(1-Rj2)))",
       "# Dataset example when wooldridge is installed:",
       "model_parts('hprice1','price',c('sqrft','bdrms','lotsize'),'bdrms')"),
     c("beta_j_hat: estimated slope for target Xj; sigmahat: regression error SD; SSTj: spread of Xj; Rj2: auxiliary R^2."))

# =================== 26. VARIANCE OF MLR ESTIMATOR =========================
.add("VAR_MLR", "MLR variance / collinearity / what increases slope SE?",
     c("variance of beta j", "variance of an ols estimator", "how variance changes", "rj2 approaches one", "r squared gets close to 1", "variance larger smaller", "multicollinearity and variance", "variance formula"),
     "Var(beta_j_hat|X)=sigma^2/[SSTj*(1-Rj2)], under MLR1-5.",
     c("Greater population error variance sigma^2 -> larger coefficient variance and SE.",
       "Greater variation in Xj (SSTj) -> smaller variance and SE.",
       "Greater auxiliary Rj2 -> denominator smaller -> larger variance and SE.",
       "As auxiliary Rj2 approaches 1, coefficient variance becomes arbitrarily large (given other pieces fixed).",
       "This is the MULTIPLE regression extension of Var(beta1_hat)=sigma^2/SST_X in simple regression.",
       "This is about the Rj2 of Xj explained by OTHER Xs, NOT the overall model R2 for Y."),
     c("sigma2 <- 36; SSTj <- 250; Rj2 <- .4",
       "var_bj <- sigma2/(SSTj*(1-Rj2))", "sqrt(var_bj)"),
     c("sigma2: population error variance; SSTj: variation of Xj; Rj2: collinearity measure from auxiliary regression."))

# =================== 27. SUMMARY OUTPUT ==================================
.add("R_SUMMARY", "Read summary(reg), Std. Error, df, residual standard error",
     c("summary reg", "summary(reg)", "std error column", "coefficients table", "matches problem", "row column", "reg coefficients", "residual standard error on", "standard error reported"),
     "A fitted regression's summary contains different outputs. Read the correct row AND column.",
     c("summary(reg)$coefficients: rows are intercept, x1, x2, ...; columns Estimate, Std. Error, t value, Pr(>|t|).",
       "For a given variable, index by NAME rather than fragile row number.",
       "sigma(reg) gives residual standard error of entire regression (PS12 Q9/Q14).",
       "summary(reg)$r.squared is overall Y-model R^2, NOT auxiliary Rj2.",
       "coef(reg) yields estimated coefficients; reg$residuals or residuals(reg) yields residuals.",
       "Chapter 4 t-tests are not part of this Exam II guide."),
     c("summary(reg)",
       "summary(reg)$coefficients[\"bdrms\",\"Std. Error\"]",
       "sigma(reg); df.residual(reg); summary(reg)$r.squared",
       "coef(reg); residuals(reg)"),
     c("Std. Error beside a predictor: SE of its estimated SLOPE; Residual standard error: sigmahat."))

# =================== 28. MISSING OBSERVATIONS =============================
.add("MISSING", "Missing data / NA / use same fitted sample for every calculation",
     c("missing data", "missing values", "na", "dropped rows", "complete cases", "different sample", "same rows", "all observations versus regression", "why manual does not match"),
     "lm() typically omits observations missing any variable used by the model.",
     c("nobs(reg) gives count USED by fitted model, which may be below nrow(dataset).",
       "If Y or another X has NA but target Xj does not, raw SSTj on ALL Xj rows may differ from full model's SSTj.",
       "Do ALL hand computations on SAME rows used by the full model: model.frame(reg).",
       "Auxiliary regression must also use the full fit's rows for matching standard errors.",
       "Missing data can change sample mean, SD, SST, and coefficients; not just R-squared."),
     c("reg <- lm(y ~ x1+x2, data=df)", "used <- model.frame(reg)",
       "nobs(reg); nrow(df)",
       "xj <- used$x1", "sum((xj-mean(xj))^2)"),
     c("NA: missing value; model.frame(reg): data columns on used observations."))

# =================== 29. GAUSS-MARKOV =====================================
.add("BLUE", "Gauss-Markov: BLUE / best linear unbiased",
     c("gauss markov", "blue", "best linear unbiased", "smallest variance among", "minimum variance", "most efficient", "linear unbiased estimators"),
     "Under assumptions MLR1-MLR5, OLS is BLUE (Best Linear Unbiased Estimator).",
     c("Best = smallest variance within the class of LINEAR UNBIASED estimators.",
       "Linear means estimator is linear in observed Y outcomes, not necessarily that X can only enter unsquared.",
       "Unbiased means expected estimate equals population coefficient, not correct in each sample.",
       "Gauss-Markov does NOT say OLS beats every nonlinear unbiased estimator.",
       "Homoskedasticity MLR5 is needed for the usual BLUE result; MLR1-4 suffice for unbiasedness."),
     c("# Conceptual: no R calculation required."),
     c("B=best (minimum variance in class); L=linear in Y; U=unbiased; E=estimator."))

# =================== 30. HOMOSKEDASTICITY ================================
.add("MLR5", "Homoskedasticity versus heteroskedasticity; MLR5",
     c("mlr5", "homoskedasticity", "heteroskedasticity", "constant variance", "error spread", "spread changes with x", "wage variation by education"),
     "MLR5: conditional error variance is the same at every configuration of the explanatory variables.",
     c("Var(U | X1,...,XK)=sigma^2 is constant (homoskedasticity).",
       "If error spread widens as X increases, errors are heteroskedastic: MLR5 fails.",
       "Heteroskedasticity does NOT by itself violate MLR4 or imply OLS slopes are biased.",
       "Usual standard errors and the ordinary BLUE statement rely on MLR5; heteroskedastic-robust SE may be required otherwise.",
       "Lecture example: wages may vary more among workers with 20 education years than workers with 12."),
     c("# Conceptual distinction; no code necessary on this exam guide."),
     c("Var(U|X): error variance for individuals with same regressors; sigma^2: constant error variance if MLR5 holds."))

# =================== 31. SYMBOLS / WHAT IS Xj? ============================
.add("SYMBOLS", "Formula symbols: Xj, beta-hat, sigma-hat, SSTj, Rj2, SSR, K, n",
     c("what is xj", "what does xj", "what is x j", "what does x j", "what does sigma", "what is sigma hat", "what is sstj", "what is rj2", "what does j index", "meaning of xj", "symbol means", "what does estimate"),
     "Use for MULTIPLE-CHOICE DEFINITION questions. Define the symbol being asked about; no calculation is needed.",
     c("Xj = whichever INCLUDED independent variable has the slope beta_j_hat whose SE/variance we want.",
       "j is an INDEX, not 'the predictor with largest coefficient.'",
       "Y = dependent/outcome variable (not Xj).",
       "beta_j_hat = sample OLS estimated coefficient on Xj.",
       "sigma = true population SD of U; sigmahat = estimated SD of errors using full-model residuals.",
       "SSTj = sum((Xij - mean(Xj))^2), spread of target predictor Xj.",
       "Rj2 = auxiliary R^2 from REGRESSING Xj ON THE OTHER X variables, not from Y regression.",
       "SSR = sum of squared OLS residuals in this course.",
       "n = observations actually used; K = independent variables excluding intercept; df=n-K-1.",
       "Residual uhat = actual Y minus fitted Yhat; unobserved U is not the observed residual.",
       "When the question asks 'what is' or 'what does ... represent,' do not jump to computing the full formula."),
     c("# No R required for definitions. To inspect a fitted model: names(coef(reg))"),
     c("se(beta_j_hat) = sigmahat / sqrt(SSTj*(1-Rj2))."))

# =================== 32. WOOLDRIDGE WORKFLOW ==============================
.add("WOOLDRIDGE", "Wooldridge datasets: load, inspect, fit, coefficients, SST, SE",
     c("wooldridge", "wage1", "hprice1", "gpa1", "bwght", "meap93", "ceosal1", "dataset", "data frame", "run regression in r", "regression using data"),
     "A dataset name does NOT determine which method is needed: follow what the question asks to FIND.",
     c("Install wooldridge ONCE before exam, not during it: install.packages('wooldridge').",
       "For new dataset name: data('hprice1', package='wooldridge'); names(hprice1); head(hprice1).",
       "Full model: lm(Y ~ X1+X2+X3, data=hprice1). Change dependent and regressors as specified.",
       "Use nobs(reg) to count used rows; residuals(reg) to find SSR; sigma(reg) for regression error SD.",
       "For SE of Xj: fit full model; compute its Xj SST and auxiliary Rj2 on the same rows, then combine.",
       "Use model_parts() helper below for FULL fitted model and every piece.",
       "For standalone dataset SST without a regression: sst_data('hprice1','bdrms').",
       "Variable names vary across datasets. Never assume bdrms exists in wage1 or educ in hprice1."),
     c("data('hprice1', package='wooldridge')", "names(hprice1); head(hprice1)",
       "reg <- lm(price ~ sqrft+bdrms+lotsize, data=hprice1)",
       "summary(reg); nobs(reg); sigma(reg)",
       "# One-call comprehensive component breakdown:",
       "model_parts('hprice1','price',c('sqrft','bdrms','lotsize'),'bdrms')",
       "# For other model, CHANGE dataset, dependent, X list, target."),
     c("dataset: data frame (e.g. wage1); y: outcome; xs: predictor names; target: particular Xj of interest."))

# =================== 33. SELECT ALL / CONCEPTUAL ==========================
.add("CONCEPT", "Conceptual / select-all elimination checklist",
     c("which statements", "select all", "which statement", "which is true", "which is false", "not correct", "not necessarily", "always", "never", "must hold", "can conclude"),
     "Do NOT choose by a familiar keyword. For EACH option, check its full claim against a precise rule.",
     c("True SAMPLE identities: residual average 0, zero residual-X covariance, OLS fitted point of means (with intercept).",
       "Not guaranteed: every residual 0; high R2 means unbiased; correlation implies causation; unbiased means exactly right this sample.",
       "MLR3: exact collinearity fails; merely high correlation is not exact collinearity.",
       "MLR4: zero conditional mean; if omitted factor affects Y AND is related to an included X, bias is plausible.",
       "MLR5: constant error variance; failing MLR5 alone does not imply biased OLS slopes.",
       "Gauss-Markov BLUE means least variance among LINEAR UNBIASED estimators only.",
       "Coefficient SE is not regression residual SE; overall model R2 is not auxiliary Rj2.",
       "Look carefully at EXCEPT, NOT, ALL, ONLY, MUST, ALWAYS. Qualifiers change truth values.",
       "If a claim is not covered, open MLR3, MLR4, RESID, R2_FIT, UNBIASED, or BLUE for complete rules."),
     c("# Conceptual questions typically need reasoning, not R."))



# =================== 34. BROAD STANDARD-ERROR DECISION GUIDE ================
.add("SE_MAP", "STANDARD ERROR: two meanings; choose the requested result",
     c("standard error", "find the standard error", "compute a standard error", "standard error formula"),
     "A question that only says 'standard error' is AMBIGUOUS. Use these clues to choose the correct calculation.",
     c("Standard error OF THE REGRESSION / residual standard error / sigma-hat: describes the spread of regression errors. Use sqrt(SSR/(n-K-1)).",
       "Standard error OF A COEFFICIENT / slope / beta-j: describes precision of one estimated slope. Use sigma-hat/sqrt(SSTj*(1-Rj2)).",
       "If n, K and SSR are supplied but no target slope, likely regression error SD.",
       "If sigma-hat, SSTj and auxiliary Rj2 are supplied, likely individual coefficient SE.",
       "If a model is fitted in R: sigma(reg) gives regression error SD; summary(reg)$coefficients[\"xj\",\"Std. Error\"] gives that coefficient's SE.",
       "DO NOT confuse main regression R^2 with auxiliary Rj^2. See note('AUX_R2').",
       "If asked 'what makes a standard error larger?', more error variance or more collinearity increases a coefficient SE; more independent Xj spread decreases it."),
     c("# CASE 1: regression / residual standard error", "n <- 44; K <- 3; SSR <- 1498; sqrt(SSR/(n-K-1))  # 6.12",
       "# CASE 2: a particular coefficient's standard error", "sigmahat <- 6.5; SSTj <- 836; Rj2 <- .77", "sigmahat/sqrt(SSTj*(1-Rj2))  # 0.47",
       "# Fitted model: sigma(reg) versus summary(reg)$coefficients[\"bdrms\",\"Std. Error\"]"),
     c("n = observations USED by regression; K = number of independent variables; SSR = sum of SQUARED residuals.",
       "sigmahat = estimated error SD; SSTj = variation in the target independent variable; Rj2 = its auxiliary R^2."))

# =================== 35. QUICK 'WHAT IS IT ASKING?' MAP =====================
.add("EXAM_MAP", "Exam wording decoder: what the question is asking",
     c("exam question", "what should i do", "how to solve", "help", "notes", "conceptual"),
     "If the question is vague, scan the following distinctions, then open the matching section with note('ID').",
     c("'What is Xj, sigma-hat, SSTj, Rj2?' -> SYMBOLS. For a requested statistic use SSTJ, AUX_R2, REG_ERR, or COEF_SE.",
       "'How precise / Std. Error beside [variable]?' -> COEF_SE. 'Residual standard error?' -> REG_ERR.",
       "'Which regression gives Rj2?' -> AUX_R2. 'Total variation of Xj around mean?' -> SSTJ.",
       "'Rises, falls, how much does fitted Y change?' -> PREDICT, LOG_LEVEL, LEVEL_LOG, LOG_LOG depending on logs.",
       "'What is the bias direction?' -> OVB_SIGN. 'Recover simple/multiple slope?' -> OVB_FORMULA.",
       "'MLR3, MLR4, MLR5?' -> MLR3, MLR4, MLR5. 'BLUE?' -> BLUE.",
       "'What must be true about fitted residuals?' -> RESID. 'High R2?' -> R2_FIT.",
       "'Wooldridge, wage1, hprice1, gpa1, ceosal1, ...?' -> WOOLDRIDGE; then use the SPECIFIC requested-statistic note if named.",
       "Review before Exam I: population E/Var -> PROB, sample mean/SD -> SAMPLE.",
       "If you see 'NOT', 'EXCEPT', 'ALWAYS', 'ONLY', or 'SELECT ALL', inspect each statement separately: don't treat a familiar word as proof."),
     c("topics()  # list available sections", "note('SE_MAP')  # broad standard-error guide", "note('WOOLDRIDGE')  # dataset workflow"))

# =================== 36. DATA PREDICTIONS/CHANGED DATASET ===================
.add("DATA_PRED", "Wooldridge predictions and changes when dataset/model changes",
     c("predicted using data", "predict from dataset", "wooldridge prediction", "estimated wage model", "run model", "change dataset", "predict wage", "using the wage1 data"),
     "Find Y, the listed X variables and any log transformation. Fit EXACTLY the specified model, then predict values/changes using its UNROUNDED coefficients.",
     c("Dataset and regressor names can change. Never reuse a coefficient from a different dataset or fit.",
       "For fitted predicted values in original units from log(Y), use exp(predict(reg, newdata=...)). This back-transform is the exponential of predicted log(Y), not generally an unbiased estimate of E(Y|X).",
       "For predicted log(Y) percentage changes: approx 100*sum(beta_j*changeXj); exact 100*(exp(sum(beta_j*changeXj))-1).",
       "If the model is Y in levels, changes in Y are sum(beta_j*changeXj), in Y UNITS.",
       "For a binary dummy switching 0 to 1, change in X is 1; switching 1 to 0 gives -1.",
       "Use model.frame(reg) for rows actually used. Never round the coefficients before the final answer."),
     c("data('wage1', package='wooldridge')",
       "reg <- lm(log(wage) ~ educ + exper + tenure, data=wage1)",
       "coef(reg)  # full-precision estimated coefficients",
       "# Suppose educ increases by 2, exper decreases by 4, tenure unchanged:",
       "dlog <- coef(reg)['educ']*2 + coef(reg)['exper']*(-4)",
       "100*dlog                   # approximate percent predicted change",
       "100*(exp(dlog)-1)          # exact percentage change",
       "# New value example, supplied X values:",
       "exp(predict(reg, newdata=data.frame(educ=12,exper=5,tenure=3)))"),
     c("coef(reg) = unrounded OLS slopes; dlog = predicted change in log(Y); exp() reverses natural log."))




# =================== EXTRA QUICK-ACCESS NOTES ==============================
.add("SSR", "SSR: sum of squared residuals, not the regression standard error",
     c("ssr", "sum of squared residuals", "residual sum of squares", "unexplained variation"),
     "SSR is the sum of the SQUARED fitted residuals from the original regression.",
     c("In this class SSR = sum((Y - fitted Y)^2).",
       "If sigmahat, n, and K are given: SSR = sigmahat^2 * (n-K-1).",
       "With an intercept, SSR=(1-R_squared)*SST_Y, where SST_Y is total variation of Y.",
       "SSR and sigmahat are different: sigmahat=sqrt(SSR/(n-K-1)).",
       "Some textbooks call SSR the explained sum of squares; use your professor's notation."),
     c("# After fitting model reg:",
       "SSR <- sum(residuals(reg)^2); SSR",
       "# Given sigmahat, n and K:",
       "sigmahat <- 6.4; n <- 62; K <- 3; SSR <- sigmahat^2*(n-K-1); SSR",
       "# Given SST of Y and model R^2:",
       "SST_Y <- 1200; R2 <- .25; SSR <- (1-R2)*SST_Y; SSR"),
     c("SSR: residual squared sum; sigmahat: fitted error standard deviation; K: number of explanatory variables, excluding intercept."))

.add("MLR1", "MLR1: linear in parameters / multiple population regression",
     c("mlr1", "linear in parameters", "linear regression model"),
     "MLR1 specifies Y=beta0+beta1*X1+...+betaK*XK+U: linear in unknown beta coefficients.",
     c("Regressors may be transformed, e.g. X^2 or log(X), while the model remains linear in beta.",
       "Interpret beta_j holding the other included explanatory variables fixed.",
       "The unobserved U contains factors affecting Y that the model has not explicitly included."),
     c("# Example: fit a model linear in beta, even with squared X:",
       "# reg <- lm(y ~ x + I(x^2), data=d)"))

.add("MLR2", "MLR2: random sampling",
     c("mlr2", "random sampling", "iid sample", "random observations"),
     "MLR2 assumes the observations are a random sample from the population.",
     c("A random sample is a sampling-design assumption, distinct from a zero conditional mean.",
       "MLR2 alone does not make OLS unbiased; zero conditional mean also matters.",
       "Do not confuse random sampling of observations with random assignment of explanatory variables."),
     c("# A dataset's number of used observations after lm():", "# nobs(reg)"))

###############################################################################
# QUESTION SEARCH -- FINAL RELIABLE VERSION
# Focus FIRST on the requested quantity. Never reuse values from prior questions.
###############################################################################

.clean <- function(x) {
  s <- tolower(paste(x, collapse=" "))
  s <- gsub("β", "beta", s, fixed=TRUE)
  s <- gsub("σ", "sigma", s, fixed=TRUE)
  s <- gsub("²", "2", s, fixed=TRUE)
  s <- gsub("−", "-", s, fixed=TRUE)
  s <- gsub("̂", "", s, fixed=TRUE)
  s <- gsub("r[ _]*j[ ]*\\^?[ ]*2", "rj2", s, perl=TRUE)
  s <- gsub("sst[ _]*j", "sstj", s, perl=TRUE)
  s <- gsub("sigma[ _]*hat", "sigmahat", s, perl=TRUE)
  s <- gsub("[^a-z0-9]+", " ", s, perl=TRUE)
  trimws(gsub("[[:space:]]+", " ", s))
}

.stem <- function(x) {
  z <- strsplit(paste(x, collapse="\n"), "\n", fixed=TRUE)[[1]]
  choices <- grepl("^\\s*(?:[A-Da-d][.)]|[1-4][.)])\\s+",z,perl=TRUE)
  if (any(choices) && which(choices)[1]>1) z <- z[seq_len(which(choices)[1]-1)]
  paste(z,collapse=" ")
}

.focus <- function(x) {
  s <- .clean(.stem(x))
  starts <- gregexpr("\\b(?:what|which|find|compute|calculate|determine|obtain|identify|report|give|evaluate|how|interpret)\\b",s,perl=TRUE)[[1]]
  starts <- starts[starts>0]
  if (!length(starts)) return(s)
  substr(s,tail(starts,1),nchar(s))
}

.contains <- function(x,p) isTRUE(grepl(p,x,perl=TRUE))

.route <- function(question) {
  s <- .clean(.stem(question)); f <- .focus(question)
  q <- function(p) .contains(f,p)
  h <- function(p) .contains(s,p)
  # A definition of the SYMBOL is different from computing a number.
  if (q("^(?:what is|what does|define|meaning of)\\s+(?:the\\s+)?(?:xj|rj2|sstj|sigmahat|beta j|beta hat)\\b") ||
      q("\\b(?:xj|rj2|sstj|sigmahat)\\b.{0,25}(?:represent|refer to|mean)")) return("SYMBOLS")
  # Distinct numerical answers. The QUESTION FOCUS beats background facts.
  if (q("(?:standard|std)\\s*error.{0,100}(?:coefficient|beta|slope|bedroom|bdrms|exper|educ|tenure)|(?:coefficient|beta|slope|bedroom|bdrms|exper|educ|tenure).{0,70}(?:standard|std)\\s*error|\\bse\\s*(?:of\\s+)?(?:beta|coefficient|slope)|precision.{0,50}(?:coefficient|slope)") ||
      q("std error column") ||
      (h("(?:coefficient|slope|beta.{0,8}hat|bedrooms coefficient)") && q("(?:standard|std) error|\\bse\\b") && !q("error term"))) return("COEF_SE")
  if (q("(?:sum of (?:the )?squared deviations|squared deviations|total (?:sample )?variation|\\bsstj\\b|\\bsst of\\b|sum of squared (?:differences|distances).{0,35}mean)") &&
      !q("squared residual")) return("SSTJ")
  if (q("(?:estimated )?(?:standard deviation|standard devaiation|std deviation|standard error).{0,70}(?:error term|residuals|regression errors|regression|errors)|(?:error term|residuals).{0,40}(?:standard deviation|std error)|\\bsigmahat\\b")) return("REG_ERR")
  if (q("(?:which|what).{0,40}regression.{0,90}(?:rj2|auxiliary|r squared|r2)|(?:auxiliary|rj2).{0,80}(?:which regression|which model)|(?:obtain|compute|find|calculate).{0,35}(?:auxiliary r|rj2)")) return("AUX_R2")
  if (q("\\bssr\\b|sum of squared residuals|residual sum of squares") &&
      !q("(?:standard error|standard deviation|std error)")) return("SSR")
  if (q("degrees of freedom|\\bdf\\b")) return("DF")
  if (q("(?:standard|std) error|\\bse\\b")) return("SE_MAP")
  # Concept searches and shorter questions.
  if (h("\\bsstj\\b|sum of squared deviations|squared deviations around|squared differences from.{0,25}mean|total variation.{0,45}(?:regressor|independent|bdrms|xj)|\\bsst of x")) return("SSTJ")
  if (h("\\bse\\s*\\(?beta|standard error.{0,70}(?:coefficient|beta|slope|bedrooms)|std error.{0,70}(?:coefficient|beta|slope)")) return("COEF_SE")
  if (h("estimated standard deviat|error term.{0,25}standard deviation|regression error standard|regression residual standard|residual standard error")) return("REG_ERR")
  if (h("which regression.{0,90}(?:rj2|r squared)|\\brj2\\b.{0,30}(?:regression|calculate|compute)")) return("AUX_R2")
  if (s %in% c("ssr","sum of squared residuals","residual sum of squares")) return("SSR")
  if (s %in% c("rj2","auxiliary r2")) return("AUX_R2")
  if (h("standard error|std error")) return("SE_MAP")
  if (h("\\bmlr1\\b|linear in parameters")) return("MLR1")
  if (h("\\bmlr2\\b|random sampling|iid sample")) return("MLR2")
  if (h("\\bmlr3\\b|perfect collinearity|perfectly collinear|dummy variable trap")) return("MLR3")
  if (h("\\bmlr4\\b|zero conditional mean|endogene|exogene")) return("MLR4")
  if (h("\\bmlr5\\b|homoskedastic|heteroskedastic|constant error variance")) return("MLR5")
  if (h("gauss.markov|best linear unbiased|\\bblue\\b")) return("BLUE")
  if (h("omitted.{0,50}(?:bias|variable)|bias.{0,35}(?:upward|downward|positive|negative|direction)")) {
    if (h("\\bdelta1\\b|recover.{0,20}slope|simple.{0,30}multiple.{0,30}slope")) return("OVB_FORMULA")
    return("OVB_SIGN")
  }
  if (h("unbiased|unbiasedness|repeated sampl")) return("UNBIASED")
  if (h("\\brj2\\b.{0,40}(?:increase|rise|approach|close to 1)|collinearity.{0,50}(?:standard error|variance)")) return("VAR_MLR")
  if (h("residual.{0,40}(?:sum|mean|zero)|fitted residual|orthogonal|overpredict|underpredict")) return("RESID")
  if (h("overall.{0,20}r squared|goodness of fit|explained variation|high r squared|proportion of variation")) return("R2_FIT")
  if (h("\\bssr\\b|sum of squared residuals")) return("SSR")
  if (h("\\brj2\\b|auxiliary r squared")) return("AUX_R2")
  if (h("\\bdf\\b|degrees of freedom")) return("DF")
  if (h("elasticity|log log")) return("LOG_LOG")
  if (h("exponentiate|\\bexp\\b|antilog|undo.{0,10}log")) return("EXP")
  if (h("\\b(?:log|ln)\\s*\\(?\\s*(?:wage|salary|price|earnings|y)\\b") &&
      h("\\b(?:log|ln)\\s*\\(?\\s*(?:income|x|sqrft|sales|educ|lotsize)\\b")) return("LOG_LOG")
  if (h("\\b(?:log|ln)\\s*\\(?\\s*(?:wage|salary|price|earnings|y)\\b|log wage")) return("LOG_LEVEL")
  if (h("\\b(?:log|ln)\\s*\\(?\\s*(?:income|x|sqrft|sales|educ|lotsize)\\b|log income")) return("LEVEL_LOG")
  if (h("rescal|change.{0,20}units|dollars to thousands|years to months")) return("RESCALE")
  if (h("sample standard deviation|sample variance|sample mean|group average|average within")) return("SAMPLE")
  if (h("expected value|probability table|population variance|probability distribution")) return("PROB")
  if (h("\\bsst\\b|\\bsse\\b|\\br2\\b")) return("R2_FIT")
  if (h("variance of.{0,25}(?:beta|slope)|spread of x|sampling variance")) return("SLR_VAR")
  if (h("simple regression slope|covariance.{0,30}variance|least squares slope")) return("SLOPE_SLR")
  if (h("predicted|prediction|increases by|decreases by|rises by|falls by")) return("PREDICT")
  if (h("wooldridge|hprice1|wage1|gpa1|ceosal1|bwght|data set|dataset|using the data|fit the regression|estimate the regression")) return("WOOLDRIDGE")
  if (h("multiple regression|ceteris paribus|holding other variables|partial effect")) return("MLR")
  if (h("select all|which statements|which of the following|true or false")) return("CONCEPT")
  "EXAM_MAP"
}

# Parse the actual supplied numbers, including commas and leading-decimal numbers.
.num_regex <- "([+-]?(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+))"
.capture <- function(s,pattern,mode="after") {
  if (mode=="after") {
    p <- paste0("(?:",pattern,")\\s*(?:(?:is|was|equals|equal to|of|gives|=|:|approximately|about)\\s*)?",.num_regex)
  } else {
    p <- paste0(.num_regex,"\\s*(?:",pattern,")")
  }
  z <- regmatches(s,regexec(p,s,perl=TRUE))[[1]]
  if(length(z)<2) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",","",z[2],fixed=TRUE)))
}

.model <- function(question) {
  s <- tolower(.stem(question))
  # Treat explicit lm() formula first.
  z <- regmatches(s,regexec("\\blm\\(\\s*([a-z][a-z0-9_]*)\\s*~\\s*([a-z0-9_ +]+)",s,perl=TRUE))[[1]]
  if(length(z)>2) {
    xs <- unlist(strsplit(z[3],"\\s*\\+\\s*",perl=TRUE))
    xs <- trimws(xs)
    xs <- xs[grepl("^[a-z][a-z0-9_]*$",xs)]
    return(list(y=z[2],xs=unique(xs)))
  }
  # "estimates price on sqrft, bdrms, and lotsize with 88 observations"
  z <- regmatches(s,regexec("\\b(?:estimates|estimate|regresses|regress|regression of|fits|fit|model)\\s+(?:the\\s+)?([a-z][a-z0-9_]*)\\s+on\\s+([^.;?]+)",s,perl=TRUE))[[1]]
  if(length(z)<3) return(NULL)
  rhs <- strsplit(z[3],"\\s+(?:with|using|suppose|where|when|given|in\\s+a\\s+sample)\\b",perl=TRUE)[[1]][1]
  rhs <- gsub("\\b(?:and|the|independent|explanatory|variables|variable|regressors)\\b"," ",rhs,perl=TRUE)
  rhs <- gsub("[,+]"," ",rhs)
  xs <- unlist(strsplit(trimws(rhs),"\\s+"))
  xs <- xs[grepl("^[a-z][a-z0-9_]*$",xs) & nchar(xs)>1]
  if(!length(xs)) return(NULL)
  list(y=z[2],xs=unique(xs))
}

.values <- function(question) {
  s <- tolower(.stem(question))
  s <- gsub("²","2",s,fixed=TRUE)
  s <- gsub("σ","sigma",s,fixed=TRUE)
  model <- .model(question)
  n <- .capture(s,"\\bn\\b|sample size")
  if(is.na(n)) n <- .capture(s,"(?:complete\\s+)?observations\\b","before")
  K <- .capture(s,"\\bk\\b|number of (?:explanatory|independent) variables")
  if(is.na(K)) K <- .capture(s,"(?:explanatory|independent|predictor|regressor)\\s+variables\\b","before")
  if(is.na(K)) {
    for(i in seq_len(9)) {
      word <- c("one","two","three","four","five","six","seven","eight","nine")[i]
      if (.contains(s,paste0("\\b",word,"\\s+(?:explanatory|independent|predictor|regressor)\\s+variables\\b"))) {K<-i;break}
    }
  }
  if(is.na(K) && !is.null(model)) K <- length(model$xs)
  sx <- .capture(s,"\\bs_?xj\\b|\\bsxj\\b|sample\\s+standard\\s+deviat\\w*|sample\\s+sd")
  SSR <- .capture(s,"\\bssr\\b|sum of (?:the )?squared residuals|residual sum of squares")
  SSTj <- .capture(s,"\\bsst_?j\\b|\\bsstj\\b|total variation(?:\\s+(?:in|of|for)\\s+[a-z][a-z0-9_]*)?|sum of squared deviations(?:\\s+(?:in|of|for)\\s+[a-z][a-z0-9_]*)?")
  Rj2 <- .capture(s,"\\br_?j\\s*\\^?2\\b|\\brj2\\b|auxiliary\\s+r\\s*\\^?2(?:\\s+(?:for|of)\\s+[a-z][a-z0-9_]*)?|auxiliary\\s+r.squared(?:\\s+(?:for|of)\\s+[a-z][a-z0-9_]*)?")
  sigmahat <- .capture(s,"\\bsigma_?hat\\b|\\bsigmahat\\b|sigma\\s+hat|estimated\\s+(?:regression\\s+)?error\\s+(?:standard deviation|sd)")
  list(n=n,K=K,sx=sx,SSR=SSR,SSTj=SSTj,Rj2=Rj2,sigmahat=sigmahat,model=model)
}

.dataset <- function(question) {
  q <- tolower(.stem(question))
  datasets <- c("hprice1","hprice2","wage1","wage2","gpa1","ceosal1","bwght","vote1","meap93","lawsch85","sleep75","mroz","attend","htv","crime1","401k","401ksubs")
  for (d in datasets) if (.contains(q,paste0("\\b",d,"\\b"))) return(d)
  NULL
}

.target <- function(question) {
  s <- tolower(.stem(question))
  pats <- c("\\bx_?j\\s*=\\s*([a-z][a-z0-9_]*)",
            "\\b(?:for|of|on)\\s+(?:the\\s+)?([a-z][a-z0-9_]*)\\s+coefficient",
            "\\bcoefficient\\s+(?:on|for|of)\\s+([a-z][a-z0-9_]*)",
            "\\b(?:sstj|sst_?j)\\s+(?:for|of)\\s+([a-z][a-z0-9_]*)")
  for(p in pats) {
    z <- regmatches(s,regexec(p,s,perl=TRUE))[[1]]
    if(length(z)>1 && !z[2] %in% c("the","this","a","estimated","standard","individual")) return(z[2])
  }
  if (.contains(s,"\\bbedrooms? coefficient\\b")) return("bdrms")
  if (.contains(s,"\\bexperience coefficient\\b")) return("exper")
  NULL
}

# Print formulas using THIS question's values. Missing values are marked NA,
# never quietly replaced with random numbers from a different practice problem.
.print_code <- function(id,question) {
  v <- .values(question); d <- .dataset(question); target <- .target(question)
  n <- v$n; K <- v$K; ssr <- v$SSR; sj <- v$SSTj; r2 <- v$Rj2; sig <- v$sigmahat
  out <- function(...) cat(...,"\n",sep="")
  num <- function(x) if(is.na(x)) "NA_real_" else format(x,scientific=FALSE,trim=TRUE,digits=12)
  out("\n========== CODE FOR THIS QUESTION (copy or run these lines) ==========")
  if(id=="SSTJ") {
    if(!is.na(n) && !is.na(v$sx)) {
      out("n <- ",num(n),"; sx <- ",num(v$sx))
      out("round((n-1)*sx^2,2)")
      out(sprintf("ANSWER FROM YOUR NUMBERS: %.2f", (n-1)*v$sx^2))
    } else if(!is.null(d) && !is.null(target)) {
      out("d <- load_dataset('",d,"')")
      out("xj <- d[['",target,"']]")
      out("round(sum((xj-mean(xj,na.rm=TRUE))^2,na.rm=TRUE),2)")
    } else {
      out("# Given n and SAMPLE SD: (n - 1) * (sample_SD)^2")
      out("n <- ",num(n),"; sx <- ",num(v$sx),"  # replace NA values with data from question")
      out("round((n-1)*sx^2,2)")
      out("# Given a vector of observed Xj instead:")
      out("# xj <- c(...) ; sum((xj-mean(xj))^2)")
    }
    return(invisible(NULL))
  }
  if(id=="REG_ERR" || id=="DF") {
    out("n <- ",num(n),"; K <- ",num(K),"; SSR <- ",num(ssr),"   # NA = missing input")
    out("df <- n-K-1")
    if(id=="DF") out("df  # degrees of freedom")
    else out("round(sqrt(SSR/df),2)  # estimated regression error SD")
    if(!is.na(n) && !is.na(K) && !is.na(ssr) && n>K+1 && ssr>=0 && id=="REG_ERR")
      out(sprintf("ANSWER FROM YOUR NUMBERS: %.2f",sqrt(ssr/(n-K-1))))
    return(invisible(NULL))
  }
  if(id=="COEF_SE") {
    if(!is.na(sig) && !is.na(sj) && !is.na(r2)) {
      out("sigmahat <- ",num(sig),"; SSTj <- ",num(sj),"; Rj2 <- ",num(r2))
      out("round(sigmahat/sqrt(SSTj*(1-Rj2)),2)")
      if(sj>0 && r2>=0 && r2<1) out(sprintf("ANSWER FROM YOUR NUMBERS: %.2f",sig/sqrt(sj*(1-r2))))
    } else if (!is.na(ssr) && !is.na(n) && !is.na(K) && !is.na(sj) && !is.na(r2)) {
      out("n <- ",num(n),"; K <- ",num(K),"; SSR <- ",num(ssr),"; SSTj <- ",num(sj),"; Rj2 <- ",num(r2))
      out("sigmahat <- sqrt(SSR/(n-K-1))")
      out("round(sigmahat/sqrt(SSTj*(1-Rj2)),2)")
      if(n>K+1 && sj>0 && r2>=0 && r2<1) out(sprintf("ANSWER FROM YOUR NUMBERS: %.2f",sqrt((ssr/(n-K-1))/(sj*(1-r2)))))
    } else {
      out("# An individual coefficient SE needs sigmahat, SSTj, and auxiliary Rj2.")
      out("# Or compute sigmahat from SSR, n, K. NA means the input was not found.")
      out("n <- ",num(n),"; K <- ",num(K),"; SSR <- ",num(ssr))
      out("sigmahat <- ",num(sig),"; SSTj <- ",num(sj),"; Rj2 <- ",num(r2))
      out("if (is.na(sigmahat)) sigmahat <- sqrt(SSR/(n-K-1))")
      out("round(sigmahat/sqrt(SSTj*(1-Rj2)),2)")
    }
    if (!is.null(d) && !is.null(target)) out("# If fitting data instead: model_parts('",d,"', 'Y', c('X1','",target,"','X3'), '",target,"') # edit model")
    return(invisible(NULL))
  }
  if(id=="SSR") {
    out("# After running a regression and saving it as reg:")
    out("SSR <- sum(residuals(reg)^2); SSR")
    out("# From sigmahat and df: SSR <- sigmahat^2*(n-K-1)")
    out("# From the dependent variable SST and overall R2: SSR <- (1-R2)*SST_Y")
    return(invisible(NULL))
  }
  if(id=="AUX_R2") {
    m <- v$model
    if(!is.null(m) && !is.null(target) && target %in% m$xs) {
      xs <- setdiff(m$xs,target)
      if(length(xs)) {
        out("# Original model Y = ",m$y,"; target Xj = ",target)
        if(!is.null(d)) out("d <- load_dataset('",d,"')")
        out("aux <- lm(",target," ~ ",paste(xs,collapse=" + "),", data=",if(is.null(d)) "d" else "d",")")
        out("summary(aux)$r.squared")
      } else out("Only one independent variable: auxiliary Rj2 = 0 by convention.")
    } else {
      out("# Original: wage ~ educ + exper + tenure; target is exper")
      out("# Regress target Xj on the OTHER original X variables, never on wage.")
      out("# aux <- lm(exper ~ educ + tenure, data=wage1)")
      out("# summary(aux)$r.squared")
    }
    return(invisible(NULL))
  }
  if(id=="WOOLDRIDGE" || id=="DATA_PRED" || id=="R_SUMMARY") {
    if(is.null(d)) {
      out("# No dataset NAME detected. Change 'hprice1' to the dataset specified.")
      out("d <- load_dataset('hprice1')")
    } else out("d <- load_dataset('",d,"')")
    out("names(d)")
    if(!is.null(v$model)) {
      out("reg <- lm(",v$model$y," ~ ",paste(v$model$xs,collapse=" + "),", data=d)")
      out("summary(reg); sigma(reg); nobs(reg)")
    } else {
      out("# Replace Y, X1, X2 with the variables in the question:")
      out("# reg <- lm(Y ~ X1 + X2, data=d)")
      out("# summary(reg); summary(reg)$coefficients; summary(reg)$r.squared")
    }
    return(invisible(NULL))
  }
  if(id=="SE_MAP") {
    out("# REGRESSION error SD: sqrt(SSR/(n-K-1))")
    out("# COEFFICIENT SE: sigmahat/sqrt(SSTj*(1-Rj2))")
    out("note('REG_ERR')   # specific regression error SD instructions")
    out("note('COEF_SE')   # coefficient-specific SE instructions")
    return(invisible(NULL))
  }
  # The original full note contains its own executable R examples.
  t <- .exam_notes[[id]]
  if(length(t$code)) for(line in t$code) out(line)
  else out("# This is a conceptual question; no calculation is required.")
  invisible(NULL)
}

.print_note <- function(id) {
  t <- .exam_notes[[id]]
  if(is.null(t)) return(invisible(NULL))
  cat("\n",strrep("-",68),"\n",sep="")
  cat("NOTES: ",t$title," [",id,"]\n",sep="")
  cat("WHEN TO USE: ",t$meaning,"\n",sep="")
  cat("CONCEPTS / MULTIPLE-CHOICE AND SELECT-ALL RULES:\n")
  for(z in t$facts) cat("  - ",z,"\n",sep="")
  if(length(t$symbols)) {
    cat("SYMBOLS:\n")
    for(z in t$symbols) cat("  - ",z,"\n",sep="")
  }
  if(length(t$code)) {
    cat("R EXAMPLES / TEMPLATES (values below are EXAMPLES, not your question):\n")
    for(z in t$code) cat("  ",z,"\n",sep="")
  }
  invisible(t)
}

.note_hits <- function(text) {
  q <- .clean(text)
  ids <- names(.exam_notes)
  if(!nzchar(q)) return(character())
  score <- vapply(ids,function(id) {
    t <- .exam_notes[[id]]
    title <- .clean(t$title)
    clues <- .clean(t$clues)
    points <- 0
    if(.contains(.clean(id),paste0("^",q,"$"))) points <- points+20
    if(grepl(q,title,fixed=TRUE)) points <- points+10
    if(grepl(q,clues,fixed=TRUE)) points <- points+8
    words <- strsplit(q," ",fixed=TRUE)[[1]]
    for(w in words[nchar(words)>2]) if(grepl(w,title,fixed=TRUE)) points <- points+1
    points
  },numeric(1))
  ids[order(-score)][score[order(-score)]>0]
}

note <- function(id) {
  id <- as.character(id)[1]
  exact <- toupper(trimws(id))
  if(exact %in% names(.exam_notes)) return(.print_note(exact))
  # Natural-language note search, including 'squared deviations'.
  routed <- .route(id)
  if(routed!="EXAM_MAP") {
    cat("SEARCH MATCH: ",routed,"\n",sep="")
    return(.print_note(routed))
  }
  hits <- .note_hits(id)
  if(length(hits)) {
    cat("NEAREST NOTE SEARCH RESULTS:\n")
    for(z in head(hits,5)) cat(sprintf("%-14s %s\n",z,.exam_notes[[z]]$title))
    return(.print_note(hits[1]))
  }
  cat("No exact match. Opening the exam search guide.\n")
  .print_note("EXAM_MAP")
}

topics <- function(search=NULL) {
  ids <- if(is.null(search)) names(.exam_notes) else .note_hits(search)
  if(!length(ids) && !is.null(search)) {
    routed <- .route(search)
    if(routed!="EXAM_MAP") ids <- routed
  }
  if(!length(ids)) {
    cat("No topic-title match. Try ask('your phrase') instead.\n")
    return(invisible(character()))
  }
  for(id in ids) cat(sprintf("%-14s %s\n",id,.exam_notes[[id]]$title))
  invisible(ids)
}

ask <- function(question=NULL) {
  if(is.null(question) || !nzchar(trimws(paste(question,collapse=""))))
    question <- readline("PASTE QUESTION, then press Return: ")
  question <- paste(question,collapse="\n")
  if(!nzchar(trimws(question))) {cat("No question was entered.\n"); return(invisible(NULL))}
  id <- .route(question)
  cat("\n",strrep("=",68),"\nQUESTION -> CODE + NOTES\n",strrep("=",68),"\n",sep="")
  cat("YOU ASKED: ",.stem(question),"\n",sep="")
  cat("QUESTION FOCUS: ",.focus(question),"\n",sep="")
  cat("BEST MATCH: ",id," / ",.exam_notes[[id]]$title,"\n",sep="")
  .print_code(id,question)
  .print_note(id)
  related <- switch(id,
                    SSTJ=c("SAMPLE","COEF_SE","WOOLDRIDGE"),
                    COEF_SE=c("REG_ERR","SSTJ","AUX_R2","WOOLDRIDGE"),
                    REG_ERR=c("DF","COEF_SE","SSR"),
                    AUX_R2=c("COEF_SE","VAR_MLR"),
                    SE_MAP=c("COEF_SE","REG_ERR"),
                    WOOLDRIDGE=c("SSTJ","COEF_SE","R_SUMMARY"),
                    c("EXAM_MAP","CONCEPT"))
  cat("\nRELATED: ",paste(sprintf("note('%s')",related),collapse=" | "),"\n",sep="")
  if(id=="EXAM_MAP") {
    cat("Try a short search with the exact requested outcome, e.g. ask('sum of squared deviations').\n")
    matches <- .note_hits(question)
    if(length(matches)) cat("OTHER POSSIBLE NOTES: ",paste(head(matches,3),collapse=", "),"\n",sep="")
  }
  invisible(list(topic=id,values=.values(question)))
}

ask_paste <- function() {
  cat("Paste full question and choices, one line at a time; type DONE alone to finish.\n")
  z <- character()
  repeat {
    line <- readline(if(length(z)) "MORE / DONE: " else "QUESTION: ")
    if(toupper(trimws(line))=="DONE") break
    z <- c(z,line)
    if(length(z)>=70) break
  }
  if(length(z)) ask(paste(z,collapse="\n"))
}

find_topic <- function(keyword) topics(keyword)
symbols <- function() note("SYMBOLS")
wooldridge_guide <- function() note("WOOLDRIDGE")

###############################################################################
# DATA HELPERS -- BASE R EXCEPT FOR THE OPTIONAL 'wooldridge' PACKAGE
###############################################################################
load_dataset <- function(name) {
  if(!requireNamespace("wooldridge",quietly=TRUE)) stop("Install wooldridge once (when allowed): install.packages('wooldridge')",call.=FALSE)
  e <- new.env(parent=globalenv())
  utils::data(list=name,package="wooldridge",envir=e)
  if(!exists(name,envir=e,inherits=FALSE)) stop("Dataset not found in wooldridge; check the exact name.",call.=FALSE)
  get(name,envir=e,inherits=FALSE)
}

sst <- function(x) {
  x <- x[!is.na(x)]
  if(length(x)<2) stop("At least two nonmissing values are required")
  sum((x-mean(x))^2)
}

sst_data <- function(dataset,variable) {
  d <- load_dataset(dataset)
  if(!variable %in% names(d)) stop("Variable is absent. Check names(load_dataset('dataset')).")
  ans <- sst(d[[variable]])
  cat(sprintf("SST(%s) in %s = %.2f\n",variable,dataset,ans))
  invisible(ans)
}

reg_error <- function(n,K,SSR) {
  if(n<=K+1 || SSR<0) stop("Require n>K+1 and SSR>=0")
  ans <- sqrt(SSR/(n-K-1))
  cat(sprintf("Regression error SD = %.2f\n",ans))
  invisible(ans)
}

coef_se <- function(sigmahat,SSTj,Rj2) {
  if(sigmahat<0 || SSTj<=0 || Rj2<0 || Rj2>=1) stop("Require valid sigmahat, SSTj, auxiliary Rj2")
  ans <- sigmahat/sqrt(SSTj*(1-Rj2))
  cat(sprintf("Coefficient SE = %.2f\n",ans))
  invisible(ans)
}

model_parts <- function(dataset,y,xs,target) {
  d <- load_dataset(dataset)
  if(!target %in% xs) stop("Target must be one of the explanatory variables")
  if(!all(c(y,xs) %in% names(d))) stop("Check variable names with names(load_dataset('dataset'))")
  f <- stats::reformulate(xs,response=y)
  reg <- stats::lm(f,data=d)
  used <- stats::model.frame(reg)
  n <- stats::nobs(reg); K <- length(xs)
  df <- stats::df.residual(reg)
  SSR <- sum(stats::residuals(reg)^2)
  sigmahat <- sqrt(SSR/df)
  Xj <- used[[target]]
  SSTj <- sum((Xj-mean(Xj))^2)
  others <- setdiff(xs,target)
  Rj2 <- if(length(others)) summary(stats::lm(stats::reformulate(others,response=target),data=used))$r.squared else 0
  se <- sigmahat/sqrt(SSTj*(1-Rj2))
  table_se <- summary(reg)$coefficients[target,"Std. Error"]
  cat("Model: ",deparse(f),"; dataset: ",dataset,"\n",sep="")
  cat(sprintf("n=%d K=%d df=%d SSR=%.4f sigmahat=%.4f\n",n,K,df,SSR,sigmahat))
  cat(sprintf("target=%s SSTj=%.4f auxiliary Rj2=%.5f\n",target,SSTj,Rj2))
  cat(sprintf("Coefficient SE manual=%.4f regression table=%.4f\n",se,table_se))
  invisible(list(reg=reg,n=n,K=K,df=df,SSR=SSR,sigmahat=sigmahat,SSTj=SSTj,Rj2=Rj2,coef_se=se))
}

# User's exact failures + representative Exam 2 questions. Run in RStudio.
self_test <- function() {
  checks <- list(
    c("Using a housing dataset, an economist estimates price on sqrft, bdrms, and lotsize with 88 complete observations. Suppose SSR = 201,600, the total variation in bdrms is 80, and the auxiliary R² for bdrms is 0.30. What value should appear in the Std. Error column for the bedrooms coefficient? Round to the hundredths.","COEF_SE"),
    c("In a sample of 49 observations, an independent variable has a sample standard deviation of 4. What is the sum of squared deviations of that variable from its sample mean?","SSTJ"),
    c("what is the sum of squared deviations","SSTJ"),
    c("In a sample of n=148 observations, xj has sample standard deviation Sxj=3.4. Find SSTj.","SSTJ"),
    c("A multiple regression has sigma_hat=6.4, SSTj=842, Rj2=0.37. Find se(betaj_hat).","COEF_SE"),
    c("A researcher estimates a multiple regression using 62 observations and three explanatory variables. The sum of squared residuals is 1,450. What is the estimated standard deviation of the error term?","REG_ERR"),
    c("Which regression gives Rj2 for exper in wage ~ educ + exper + tenure?","AUX_R2"),
    c("In the formula se(betaj_hat)=sigmahat/sqrt(SSTj*(1-Rj2)), what is Xj?","SYMBOLS"),
    c("Compute SSTj for Xj=bdrms using hprice1","SSTJ"),
    c("Using hprice1, estimate the regression of price on sqrft, bdrms and lotsize","WOOLDRIDGE"),
    c("Which assumptions are needed for Gauss Markov BLUE?","BLUE"),
    c("What is MLR3 perfect collinearity?","MLR3"),
    c("An omitted ability variable causes upward bias in the schooling coefficient.","OVB_SIGN"),
    c("log(wage)=0.7+0.06educ-0.01exper; educ rises 3. How does wage change?","LOG_LEVEL"),
    c("testscore=15+8log(income); income increases by 10%. What happens?","LEVEL_LOG"),
    c("what is the standard error","SE_MAP"),
    c("SSR","SSR")
  )
  good <- logical(length(checks))
  for(i in seq_along(checks)) {
    got <- .route(checks[[i]][1]); exp <- checks[[i]][2]
    good[i] <- identical(got,exp)
    cat(sprintf("%s %02d: %-10s expected %-10s\n",if(good[i]) "PASS" else "FAIL",i,got,exp))
  }
  qs <- checks[[1]][1]
  vv <- .values(qs)
  v2 <- .values(checks[[2]][1])
  v3 <- .values(checks[[4]][1])
  math <- c(
    !is.na(vv$n) && identical(as.numeric(vv$n),88),
    !is.na(vv$K) && identical(as.numeric(vv$K),3),
    !is.na(vv$SSR) && identical(as.numeric(vv$SSR),201600),
    !is.na(vv$SSTj) && identical(as.numeric(vv$SSTj),80),
    !is.na(vv$Rj2) && abs(vv$Rj2-0.30)<1e-10,
    !is.na(v2$n) && v2$n==49,
    !is.na(v2$sx) && v2$sx==4,
    !is.na(v3$n) && v3$n==148,
    !is.na(v3$sx) && abs(v3$sx-3.4)<1e-10,
    abs(round(sqrt((201600/(88-3-1))/(80*(1-0.30))),2)-6.55)<1e-10,
    abs(round((49-1)*4^2,2)-768)<1e-10,
    abs(round((148-1)*3.4^2,2)-1699.32)<1e-10
  )
  cat(sprintf("ROUTING %d/%d; EXTRACTION/MATH %d/%d\n",sum(good),length(good),sum(math),length(math)))
  if(!all(c(good,math))) cat("Review FAIL items; the topic can always be opened using note('ID').\n")
  invisible(list(routing=good,math=math))
}

cat("\nEXAM 2 NOTES DATABASE LOADED: ",length(.exam_notes)," note sections.\n",sep="")
cat("Type ask() to paste a question; note('squared deviations') searches notes.\n")
cat("Run self_test() once. For Wooldridge: wooldridge_guide(), model_parts().\n")
