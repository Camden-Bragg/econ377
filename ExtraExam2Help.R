sigma_hat=6.5
SST_j=836
Rj2=0.77

sigma_hat/sqrt(SST_j*(1-Rj2))

reg <- lm(price ~ sqrft + bdrms +lotsize, data = hprice1)

sqrt(sum(residuals(reg)^2)/df.residual(reg))
SSR <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR / 84)

n= nobs(reg)
K= length(coef(reg))-1
df= n-K-1

xj <- hprice1$bdrms
sum((xj-mean(xj))^2)

aux <- lm(bdrms ~ sqrft + lotsize, data = hprice1)
summary(aux)$r.squared

summary(reg)$coefficients["bdrms", "Std. Error"]
summary(reg)$coefficients

summary(reg)

100 * (0.065 * 3 - 0.012 * 5 + 0.018)

n        = 42
k        = 3
ssr      = 1520

#EXACT R CODE EXAMPLE (replace numbers/names from your question):
  # Example: log(wage) = .75 + .065*educ - .012*exper + .018*tenure
  # Education +3, experience +5, tenure fixed:
  100 * (.075 * 2 + (-.013) * 4)  # 13.50 percent, approximately
# NEW numbers? Replace slopes and CHANGES in the line above.
# Or use one helper for ANY number of changing variables:
log_multi(c(educ=.065,exper=-.012,tenure=.018), c(educ=3,exper=5))
# Want EXACT percent only if question explicitly requests exact:
log_multi(c(educ=.065,exper=-.012),c(educ=3,exper=5),exact=TRUE)

#EXACT R CODE EXAMPLE (replace numbers/names from your question):
  n <- 56; k <- 3; ssr <- 2560; sstj <- 120; rj2 <- .25
sqrt((ssr/(n-k-1)) / (sstj*(1-rj2)))  # 0.67
# If sigma-hat (not SSR) is GIVEN:
sigmahat <- 8.0; sstj <- 400; rj2 <- .36
sigmahat/sqrt(sstj*(1-rj2))  # 0.47
# When a model is fitted: summary(reg)$coefficients['bdrms','Std. Error']

n <- 43; sd_x <- 5.9
(n-1)*sd_x^2

x <- c(2,3,4,7,9)
sum((x-mean(x))^2)

n=43
sd_x=3.4
(n-1)*sd_x^2

# If sigma-hat (not SSR) is GIVEN:
sigmahat <- 6.4; sstj <- 842; rj2 <- .37
sigmahat/sqrt(sstj*(1-rj2))  # 0.47

reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
summary(reg)
summary(reg)$sigma; summary(reg)$r.squared
summary(reg)$coefficients['bdrms','Std. Error']
df.residual(reg); sum(residuals(reg)^2)
summary(reg)

summary(reg)$sigma

aux <- lm(bdrms ~ sqrft + lotsize, data=hprice1)
summary(aux)$r.squared

xj <- model.frame(reg)[['VARIABLE']]
xj
round(sum((xj - mean(xj))^2), 2)


reg <- lm(price ~ sqrft + bdrms + lotsize,data=hprice1)
summary(reg)
summary(reg)$coefficients['bdrms','Std. Error']

reg <- lm(price ~ sqrft + bdrms + lotsize,data=hprice1)
summary(reg)
summary(reg)$coefficients['bdrms','Std. Error']

summary(reg)

summary(reg)$coefficients
(148-1)*3.4^2

n <- 62; K <- 3; SSR <- 1450
df <- n-K-1
sigmahat <- sqrt(SSR/df); round(sigmahat,2)
sigmahat

sigmahat <- 7.2; SSTj <- 225; Rj2 <- .64
sigmahat/sqrt(SSTj*(1-Rj2))

