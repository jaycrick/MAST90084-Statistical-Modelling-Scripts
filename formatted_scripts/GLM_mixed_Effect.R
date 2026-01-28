# MASS::glmmPQL (penalized quasi-likelihood)
# lme4::glmer (Laplace approximation and adaptive Gauss-Hermite quadrature [AGHQ])
# MCMCglmm (Markov chain Monte Carlo)
# glmmML (AGHQ)
# glmmAK (AGHQ?)
# glmmADMB (Laplace)
# glmm (from Jim Lindsey’s repeated package: AGHQ)
# gamlss.mx
# ASREML-R   (EM + )
# sabreR
# mcemGLM  (MCMC+ EM)

library(conflicted)
library(lme4)
# library(mcemGLM)
library(geepack) # for the ohio dataset
library(ordinal)
library(MASS)

head(ohio)
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


glmer_fit <- lme4::glmer(
  resp ~ age * smoke + (1 | id),
  data = ohio,
  family = binomial,
  control = glmerControl(optimizer = "Nelder_Mead")
)
# the "default" recommended by Faraway (2016)'s book

summary(glmer_fit)

## the following can closely reproduce the numbers in table 7.2 of F&T for Gauss-Hermite with 10 quadrature points
glmer_fit_n_agq_10 <- lme4::glmer(
  resp ~ age * smoke + (1 | id),
  data = ohio,
  family = binomial,
  control = glmerControl(optimizer = "Nelder_Mead"),
  nAGQ = 10
)


summary(glmer_fit_n_agq_10)


head(ranef(glmer_fit)) # "estimate" the random effects, which are conditional modes as indicated by the documentation of ranef()


################################################## ew
# this is an alternative (and faster) method for fitting glmm that doesn't require numerical integration, see ch 13 in Faraway (2016) for more details.
glmm_pql_fit <- MASS::glmmPQL(
  resp ~ age * smoke,
  random = ~ 1 | id,
  family = binomial,
  data = ohio
) # based on penalzied quasi-likelihood

summary(glmm_pql_fit)

head(ranef(glmm_pql_fit)) # this should be the version of ranef() in nlme


#### wine bitterness dataset

options(contrasts = c("contr.treatment", "contr.poly"))
head(wine) # this is from the ordinal package


# relevel(wine_FT$temp)

class(wine$bottle)
class(wine$rating)
class(wine$judge)
class(wine$temp)
class(wine$contact)
contrasts(wine$contact)
contrasts(wine$temp)
contrasts(wine$contact) <- c(0, 1)
contrasts(wine$temp) <- c(1, 0)
contrasts(wine$contact)
contrasts(wine$temp)


wine_fixed_fit_polr <- polr(
  rating ~ temp + contact,
  data = wine,
  Hess = TRUE,
  method = "logistic"
)
# use our old polr() function for comparison
summary(wine_fixed_fit_polr)


#  we will ak=lso try the function in the ordinal package
wine_fixed_fit_clm <- clm(rating ~ temp + contact, data = wine) # another fixed effect fit with the "ordinal" package
summary(wine_fixed_fit_clm)
wine_fixed_fit_clm$coefficients


# for comparison, I computed these thresholds reported in the book
book_coeff <- c(
  -5.289,
  -5.289 + exp(0.974),
  -5.289 + exp(0.974) + exp(0.739),
  -5.289 + exp(0.974) + exp(0.739) + exp(0.426)
)
book_coeff

# reported log likelihood number is close to table 7.4 of F & T, but the est. coeffs are not the same..
# using the F&T 's book package dataset doesn't help either..
# wine_FT = Fahrmeir::wine
# wine_FT$score = as.ordered(wine_FT$score)
# str(wine_FT)
# contrasts(wine_FT$temp)
# contrasts(wine_FT$contact)
# polr(score ~ temp + contact, data = wine_FT, Hess = T, method = "logistic")

## fit the random effect model
wine_mixed_fit <- clmm(
  rating ~ temp + contact + (1 | judge),
  data = wine,
  nAGQ = 10
)

summary(wine_mixed_fit)

# results are consistent in a sense that the beta's have larger magnitude

wine_mixed_fit$fitted.values

wine_mixed_fit$contrasts

ranef(wine_mixed_fit) # can also get estimates of random effects, but ranef() here comes from the ordinal package
