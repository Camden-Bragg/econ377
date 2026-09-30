## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D12/D12_starter.R", "D12.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D12) into a new script
## SAVE your work -> commit + push D12.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ECN 377 - Day 12 STARTER  |  R^2;  units of measurement
## ------------------------------------------------------------------
## Each ______ comment gives the MATH + a hint at the code; you write the command.
## Fill each ______ as we go, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)
?bwght

## ---- SST = SSE + SSR, and R^2   (bwght ~ cigs) ----
data("bwght")
reg2 <- lm(bwght ~ cigs, data = bwght)   # regress bwght on cigs
reg2$coefficients
SST <- sum((bwght$bwght - mean(bwght$bwght))^2)    # total variation:   squared deviations of bwght from its mean, summed
SST <- (1388-1) * var(bwght$bwght)
SST <- (nrow(bwght) -1) * var(bwght$bwght)
SSR <- (nrow(bwght) -1) * var(reg2$residuals)    # unexplained:       squared residuals of reg2, summed
SSE <- SST - SSR    # explained:         SST - SSR
SSE <- (nrow(bwght) -1) * var(reg2$fitted.values)
R2  <- SSE / SST    # R^2 = SSE / SST    (~ 0.02: low is normal)
summary(reg2)$r.squared
summary(reg2)

## ---- Units of measurement   (Example 2.3: salary on roe, salary in $1000s) ----
data("ceosal1")
reg3 <- ______         # regress salary on roe                         (963.19 and 18.50)
ceosal1$salarydol <- ______   # salary in DOLLARS:  Y x 1000          (hint: 1000 * the salary column)
______                # regress salarydol on roe: both estimates x 1000?  (look at $coefficients)
ceosal1$roedec <- ______      # roe as a DECIMAL:   X x 1/100         (hint: the roe column / 100)
______                # regress salary on roedec: slope x 100, intercept unchanged?
______                # R^2 of reg3 -- same for all three regressions  (hint: summary(...)$r.squared)

## ================= PROBLEMS (your turn) =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- ______   # (a) explained sum of squares
R20  <- ______   # (b) R^2
unex <- ______   # (c) fraction of the variation UNEXPLAINED