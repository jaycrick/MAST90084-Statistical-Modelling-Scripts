## This script is used to analyse Examples 3.2 & 3.4 on the breathing test results.
library(conflicted)
library(MASS)
BTR.dat <- read.csv(
  "data/Example3-2BTR.csv",
  colClasses = c("factor", "factor", "factor", "numeric")
)
str(BTR.dat)
# again one can download the dataset from the book "Fahrmier" package and possibly get it into the right form with dplyr package
is.data.frame(BTR.dat)
BTR.dat

# explore and modify the class of the data variables
is.factor(BTR.dat$BTR)
is.ordered(BTR.dat$BTR)
BTR.dat$BTR <- as.ordered(BTR.dat$BTR) # set BTR to be ordinal


is.factor(BTR.dat$Age)
is.factor(BTR.dat$Smoking)
is.factor(BTR.dat$Freq)


###################################### Usage of polr()
##### Ordered Logistic or Probit Regression
# polr(formula, data, weights, start, ..., subset, na.action,
#      contrasts = NULL, Hess = FALSE, model = TRUE,
#      method = c("logistic", "probit", "loglog", "cloglog", "cauchit"))

### Note the signs of beta.hat are opposite to those given in the F & T,
# compare with the formula of polr in the documentation page.
#########################################################

# The default is dummy coding, with the contrast matrix function contr.treatment.
getOption(x = "contrasts") # display the current contrasts settings

# See how the details of the dummy coding for our two covariates
contrasts(BTR.dat$Smoking)
contrasts(BTR.dat$Age)


###### Fit a cumulative logistic/ proportional odds model:################
## this includes the interaction effect of Age and smoking as well only
BTR.logit <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "logistic"
)
summary(BTR.logit)
names(BTR.logit) # no residual readily available. One can access the linear predictors with "lp".
BTR.logit$lp
# They should be the eta as appeared in the document page for polr()
BTR.logit$zeta
# These give the thresholds

Z <- cbind(
  (BTR.dat$Age == "40to59"),
  (BTR.dat$Smoking == "2Former"),
  (BTR.dat$Smoking == "3Current"),
  (BTR.dat$Age == "40to59") * (BTR.dat$Smoking == "2Former"),
  (BTR.dat$Age == "40to59") * (BTR.dat$Smoking == "3Current")
)

Z %*% BTR.logit$coeff # "hand-computed" eta are the same!


## another fit that contains the main effects only
BTR.logit1 <- polr(
  BTR ~ Age + Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "logistic"
)
summary(BTR.logit1)

anova(BTR.logit, BTR.logit1)
# note that the LR statistic is the difference between the deviances of the two models

###### Fit a grouped Cox or proportional hazards model:################
BTR.ph <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "cloglog"
)
summary(BTR.ph)

###### Fit a cumulative model with the extreme maximal-value distribution:################
BTR.extm <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "loglog"
)
summary(BTR.extm)


## Now change to effect coding
options(contrasts = c("contr.sum", "contr.poly"))
## Reference page for setting contrast:
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
# See how the details of the effect coding for our two covariates
contrasts(BTR.dat$Smoking)
contrasts(BTR.dat$Age)

# the columns indicate the coding variables
BTR.logit <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "logistic"
)
BTR.logit1 <- polr(
  BTR ~ Age + Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "logistic"
)


summary(BTR.logit)
summary(BTR.logit1)
## the coeff. estimates are same as Table 3.3. in F & T. Deviances remain the same
anova(BTR.logit, BTR.logit1)
# LR stat same as before

## log likelihood for saturated model:
data_mat <- t(matrix(BTR.dat$Freq, nr = 3)) # form a matrix with grouped counts in each row
probs <- data_mat / rowSums(data_mat) # normalize each row to get MLE probabilities for the saturated model


# The for loop below computes the log likelihood for the saturated model
loglike_saturated <- 0
for (row in 1:nrow(data_mat)) {
  for (col in 1:3) {
    eval_vec <- rep(0, 3)
    eval_vec[col] <- 1
    increment <- data_mat[row, col] *
      dmultinom(x = eval_vec, prob = probs[row, ], log = TRUE)
    print(increment)
    if (!is.na(increment)) loglike_saturated <- loglike_saturated + increment
  }
}

# the "deviance of the full model"
BTR.logit$deviance - abs(2 * loglike_saturated)

# this number checks out with the 8.146 deviance in Table 3.3 of F & T
# apparently the "residual deviance" reported in "BTR.logit" is just -2 times the log likelihood, be careful!

###### Fit a grouped Cox or proportional hazards model:################
BTR.ph <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "cloglog"
)
summary(BTR.ph)
BTR.ph$deviance - abs(2 * loglike_saturated)

###### Fit a cumulative model with the extreme maximal-value distribution:################
BTR.extm <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = BTR.dat,
  weights = Freq,
  Hess = T,
  method = "loglog"
)
summary(BTR.extm)
BTR.extm$deviance - abs(2 * loglike_saturated)
## These also give the same estimates and deviances as in Table 3.3 of F & T

###  other tricks and commands

# residuals do not seem to be readily available at all
BTR.logit$residual
resid(BTR.logit)


# fitted values and predictions
BTR.logit$fitted
predict(BTR.logit, BTR.dat, type = "probs")
predict(BTR.logit, type = "probs") # same results as from the above.

# perform model selection with AIC criterion (not covered in class until time allows)
a <- step(BTR.logit) # the interaction model is also chosen

# get the variance-covariance matrix of the paramter estimates
vcov(BTR.logit) # variance matrix of parameter estimates, approximation of the Hessian inverse.
sqrt(diag(vcov(BTR.logit))) == summary(BTR.logit)$coeff[, "Std. Error"] # These are the same
solve(BTR.logit$Hess) # slightly different from vcov(BTR.logit); Hess is the observed info matrix
confint(BTR.logit) # compute confidence interval (CI)
# compute the lower and upper CI limit "by hand"
summary(BTR.logit)$coefficients[, 1] +
  qnorm(0.975) * sqrt(diag(vcov(BTR.logit))) # not the same as confint(BTR.logit)
summary(BTR.logit)$coefficients[, 1] +
  qnorm(0.025) * sqrt(diag(vcov(BTR.logit))) # not the same as confint(BTR.logit)
# visit p.220 in Venables and Ripley if interested in the profile function below
profile(BTR.logit)
