library(gee)
# library(MASS)
library(geepack)  # for the ohio dataset


# we aim to reproduce the results in Example 6.4 of F & T
?ohio  # manual for the dataset  
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
ohio$age= as.factor(ohio$age)
ohio$smoke= as.factor(ohio$smoke)



contrasts(ohio$age)
contrasts(ohio$smoke)
# F and T uses effect coding in the example so we need to set the contrasts here
options(contrasts = c("contr.sum", "contr.poly"))   # set the contrasts for the covariates to be 
contrasts(ohio$age)
contrasts(ohio$smoke)
#contrast for smoke status needs to be further adjusted
contrasts(ohio$smoke) = matrix(c(-1, 1), 2, 1)

contrasts(ohio$smoke)

geefit.exchangeable = gee( formula = resp ~ age*smoke, data = ohio, family = binomial, 
              id=id, corstr = "exchangeable")

geefit.independence = gee( formula = resp ~ age*smoke, data = ohio, family = binomial, 
              id=id, corstr = "independence")

geefit.unstructured = gee( formula = resp ~ age*smoke, data = ohio, family = binomial, 
              id=id, corstr = "unstructured")

# same number as Table 6.8 in F & T
summary(geefit.independence)$coeff
summary(geefit.exchangeable)$coeff
summary(geefit.unstructured)$coeff


geefit.independence$robust.variance
geefit.exchangeable$robust.variance
geefit.unstructured$robust.variance

# the three robust variances happen to look exactly the same here, probably just a numerical coincidence
#  however if I take a difference then R says they differ by very small numbers
geefit.independence$robust.variance - geefit.exchangeable$robust.variance


summary(geefit.independence)$scale

# Now I first tease out the Pearson residuals and then compute the method of moment dispersion estimator by hand
Pearson_resid = (ohio$resp - geefit.independence$fitted)/
                   sqrt(geefit.independence$fitted.values*(1 - geefit.independence$fitted.values))
                 
scale_by_hand  = sum(Pearson_resid^2)/(geefit.independence$nobs - length(geefit.independence$coeff))
scale_by_hand
# same number as the one reported in summary

###################################################################################################
# this is to verify how they compute the naive se

# We first regress by fixing the scale to be 1
geefit.independence.scale.fix = gee( formula = resp ~ age*smoke, data = ohio, family = binomial, 
                           id=id, corstr = "independence", scale.fix = TRUE)

summary(geefit.independence.scale.fix)$coeff[, "Naive S.E."]*sqrt(summary(geefit.independence)$scale)
# is same as 
summary(geefit.independence)$coeff[, "Naive S.E."]
#### so it only differs by the dispersion parameter, which makes sense. 


################################The beta estimate for beta_a4  (age=  10)#####
beta_age_10 = - sum(summary(geefit.independence)$coeff[c("age1", "age2", "age3"), 1])
beta_age_10
# same as the book
robust_se_age_10 =   sqrt( rep(1, 3) %*%geefit.independence$robust.variance[c("age1", "age2", "age3"), c("age1", "age2", "age3")]%*%rep(1, 3))

robust_se_age_10 
# same as reported on p.273 in the book




#################################### (Table 6.9)
### fit without interactions

geefit.independence.no.interaction = gee( formula = resp ~ age +smoke, data = ohio, family = binomial, 
                                                         id=id, corstr = "independence")
                 
geefit.exchangeable.no.interaction = gee( formula = resp ~ age + smoke, data = ohio, family = binomial, 
                                          id=id, corstr = "exchangeable")


geefit.unstructured.no.interaction = gee( formula = resp ~ age + smoke, data = ohio, family = binomial, 
                                          id=id, corstr = "unstructured")

summary(geefit.independence.no.interaction )$coeff
summary(geefit.exchangeable.no.interaction)$coeff
summary(geefit.unstructured.no.interaction)$coeff
###  Same as the numbers given in Table 6.9


# the robust variances are not the same in general
geefit.independence.no.interaction$robust.variance
geefit.exchangeable.no.interaction$robust.variance
geefit.unstructured.no.interaction$robust.variance


