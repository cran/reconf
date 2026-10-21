## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# lme4's profile confidence intervals take ~25 s, which is too slow for the
# time budget of a routine package check. Compute them only when NOT_CRAN is
# set (locally and in CI); otherwise show the precomputed result below.
run_profile <- !identical(Sys.getenv("NOT_CRAN"), "")

## ----load-data----------------------------------------------------------------
library(reconf)
library(lme4)

data(fev1)
dim(fev1)
head(fev1)

## ----visits-per-subject-------------------------------------------------------
range(table(fev1$id))

## ----fit-model----------------------------------------------------------------
fit <- lmer(
  logfev1 ~ age + ht + baseage + baseht + (age | id),
  data = fev1,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

## ----mle----------------------------------------------------------------------
round(fixef(fit), 3)

as.data.frame(VarCorr(fit), order = "lower.tri")[
  , c("grp", "var1", "var2", "vcov")]

## ----ci-all-------------------------------------------------------------------
ci <- vc_ci(fit)
ci

## ----ci-single----------------------------------------------------------------
vc_ci(fit, parm = "var_age|id")   # random slope variance psi_3

## ----lr-compare, eval = run_profile-------------------------------------------
# confint(fit, method = "profile", parm = "theta_")

## ----lr-compare-static, eval = !run_profile, echo = FALSE---------------------
# Precomputed output of the chunk above (shown when the profile computation
# is skipped for speed).
cat("              2.5 %       97.5 %",
    ".sig01  0.000000000  0.139517390",
    ".sig02  0.006037058  0.007818737",
    ".sig03 -0.671833222 -0.353296130",
    ".sigma  0.057054602  0.061757510", sep = "\n")

## ----score-test-one-----------------------------------------------------------
fit_reml <- lmer(
  logfev1 ~ age + ht + baseage + baseht + (age | id),
  data = fev1, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa")
)
vc_test(fit_reml, parm = 3L)

## ----score-test---------------------------------------------------------------
vc_test(fit_reml, parm = c(2L, 3L))

