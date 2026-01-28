library(conflicted)
library(gee)
# library(MASS)
library(geepack) # for the ohio dataset


# we aim to reproduce the results in Example 6.4 of F & T
?ohio # manual for the dataset
str(ohio)
# age = -2 is 7 yo,
# age = -1 is 8 yo
# age = 0 is 9 yo
# age = 1 is 10 yo
head(ohio, 20)

# first set the right classes for the variables in the dataset
class(ohio$resp)
class(ohio$age)
class(ohio$smoke)
ohio$age <- as.factor(ohio$age)
ohio$smoke <- as.factor(ohio$smoke)


contrasts(ohio$age)
contrasts(ohio$smoke)
# F and T uses effect coding in the example so we need to set the contrasts here
options(contrasts = c("contr.sum", "contr.poly")) # set the contrasts for the covariates to be
contrasts(ohio$age)
contrasts(ohio$smoke)
# contrast for smoke status needs to be further adjusted
contrasts(ohio$smoke) <- matrix(c(-1, 1), 2, 1)

contrasts(ohio$smoke)

geefit_exch <- gee(
  formula = resp ~ age * smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "exchangeable"
)

geefit_indep <- gee(
  formula = resp ~ age * smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "independence"
)

geefit_unstruct <- gee(
  formula = resp ~ age * smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "unstructured"
)

# same number as Table 6.8 in F & T
summary(geefit_indep)$coeff
summary(geefit_exch)$coeff
summary(geefit_unstruct)$coeff


geefit_indep$robust.variance
geefit_exch$robust.variance
geefit_unstruct$robust.variance

# the three robust variances happen to look exactly the same here, probably just a numerical coincidence
#  however if I take a difference then R says they differ by very small numbers
geefit_indep$robust.variance - geefit_exch$robust.variance


summary(geefit_indep)$scale

# Now I first tease out the Pearson residuals and then compute the method of moment dispersion estimator by hand
pearson_resid <- (ohio$resp - geefit_indep$fitted) /
  sqrt(
    geefit_indep$fitted.values * (1 - geefit_indep$fitted.values)
  )

scale_by_hand <- sum(pearson_resid^2) /
  (geefit_indep$nobs - length(geefit_indep$coeff))
scale_by_hand
# same number as the one reported in summary

###################################################################################################
# this is to verify how they compute the naive se

# We first regress by fixing the scale to be 1
geefit_indep_scale_fix <- gee(
  formula = resp ~ age * smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "independence",
  scale.fix = TRUE
)

summary(geefit_indep_scale_fix)$coeff[, "Naive S.E."] *
  sqrt(summary(geefit_indep)$scale)
# is same as
summary(geefit_indep)$coeff[, "Naive S.E."]
#### so it only differs by the dispersion parameter, which makes sense.

################################ The beta estimate for beta_a4  (age=  10)#####
beta_age_10 <- -sum(summary(geefit_indep)$coeff[
  c("age1", "age2", "age3"),
  1
])
beta_age_10
# same as the book
robust_se_age_10 <- sqrt(
  rep(1, 3) %*%
    geefit_indep$robust.variance[
      c("age1", "age2", "age3"),
      c("age1", "age2", "age3")
    ] %*%
    rep(1, 3)
)

robust_se_age_10
# same as reported on p.273 in the book

#################################### (Table 6.9)
### fit without interactions

geefit_indep_no_interaction <- gee(
  formula = resp ~ age + smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "independence"
)

geefit_exch_no_interaction <- gee(
  formula = resp ~ age + smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "exchangeable"
)


geefit_unstruct_no_interaction <- gee(
  formula = resp ~ age + smoke,
  data = ohio,
  family = binomial,
  id = id,
  corstr = "unstructured"
)

summary(geefit_indep_no_interaction)$coeff
summary(geefit_exch_no_interaction)$coeff
summary(geefit_unstruct_no_interaction)$coeff
###  Same as the numbers given in Table 6.9

# the robust variances are not the same in general
geefit_indep_no_interaction$robust.variance
geefit_exch_no_interaction$robust.variance
geefit_unstruct_no_interaction$robust.variance
