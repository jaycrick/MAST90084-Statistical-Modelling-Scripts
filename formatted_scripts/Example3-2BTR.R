## This script is used to analyse Examples 3.2 & 3.4 on the breathing test results.
library(conflicted)
library(MASS)
btr_dat <- read.csv(
  "data/Example3-2BTR.csv",
  colClasses = c("factor", "factor", "factor", "numeric")
)
str(btr_dat)
# again one can download the dataset from the book "Fahrmier" package and possibly get it into the right form with dplyr package
is.data.frame(btr_dat)
btr_dat

# explore and modify the class of the data variables
is.factor(btr_dat$BTR)
is.ordered(btr_dat$BTR)
btr_dat$BTR <- as.ordered(btr_dat$BTR) # set BTR to be ordinal


is.factor(btr_dat$Age)
is.factor(btr_dat$Smoking)
is.factor(btr_dat$Freq)


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
contrasts(btr_dat$Smoking)
contrasts(btr_dat$Age)


###### Fit a cumulative logistic/ proportional odds model:################
## this includes the interaction effect of Age and smoking as well only
btr_logit <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "logistic"
)
summary(btr_logit)
names(btr_logit) # no residual readily available. One can access the linear predictors with "lp".
btr_logit$lp
# They should be the eta as appeared in the document page for polr()
btr_logit$zeta
# These give the thresholds

Z <- cbind(
  (btr_dat$Age == "40to59"),
  (btr_dat$Smoking == "2Former"),
  (btr_dat$Smoking == "3Current"),
  (btr_dat$Age == "40to59") * (btr_dat$Smoking == "2Former"),
  (btr_dat$Age == "40to59") * (btr_dat$Smoking == "3Current")
)

Z %*% btr_logit$coeff # "hand-computed" eta are the same!


## another fit that contains the main effects only
btr_logit1 <- polr(
  BTR ~ Age + Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "logistic"
)
summary(btr_logit1)

anova(btr_logit, btr_logit1)
# note that the LR statistic is the difference between the deviances of the two models

###### Fit a grouped Cox or proportional hazards model:################
btr_ph <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "cloglog"
)
summary(btr_ph)

###### Fit a cumulative model with the extreme maximal-value distribution:################
btr_extm <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "loglog"
)
summary(btr_extm)


## Now change to effect coding
options(contrasts = c("contr.sum", "contr.poly"))
## Reference page for setting contrast:
# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
# See how the details of the effect coding for our two covariates
contrasts(btr_dat$Smoking)
contrasts(btr_dat$Age)

# the columns indicate the coding variables
btr_logit <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "logistic"
)
btr_logit1 <- polr(
  BTR ~ Age + Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "logistic"
)


summary(btr_logit)
summary(btr_logit1)
## the coeff. estimates are same as Table 3.3. in F & T. Deviances remain the same
anova(btr_logit, btr_logit1)
# LR stat same as before

## log likelihood for saturated model:
data_mat <- t(matrix(btr_dat$Freq, nr = 3)) # form a matrix with grouped counts in each row
probs <- data_mat / rowSums(data_mat) # normalize each row to get MLE probabilities for the saturated model


# The for loop below computes the log likelihood for the saturated model
loglike_saturated <- 0
for (row in seq_len(nrow(data_mat))) {
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
btr_logit$deviance - abs(2 * loglike_saturated)

# this number checks out with the 8.146 deviance in Table 3.3 of F & T
# apparently the "residual deviance" reported in "btr_logit" is just -2 times the log likelihood, be careful!

###### Fit a grouped Cox or proportional hazards model:################
btr_ph <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "cloglog"
)
summary(btr_ph)
btr_ph$deviance - abs(2 * loglike_saturated)

###### Fit a cumulative model with the extreme maximal-value distribution:################
btr_extm <- polr(
  BTR ~ Age + Smoking + Age:Smoking,
  data = btr_dat,
  weights = Freq,
  Hess = TRUE,
  method = "loglog"
)
summary(btr_extm)
btr_extm$deviance - abs(2 * loglike_saturated)
## These also give the same estimates and deviances as in Table 3.3 of F & T

###  other tricks and commands

# residuals do not seem to be readily available at all
btr_logit$residual
resid(btr_logit)


# fitted values and predictions
btr_logit$fitted
predict(btr_logit, btr_dat, type = "probs")
predict(btr_logit, type = "probs") # same results as from the above.

# perform model selection with AIC criterion (not covered in class until time allows)
a <- step(btr_logit) # the interaction model is also chosen

# get the variance-covariance matrix of the paramter estimates
vcov(btr_logit) # variance matrix of parameter estimates, approximation of the Hessian inverse.
sqrt(diag(vcov(btr_logit))) == summary(btr_logit)$coeff[, "Std. Error"] # These are the same
solve(btr_logit$Hess) # slightly different from vcov(btr_logit); Hess is the observed info matrix
confint(btr_logit) # compute confidence interval (CI)
# compute the lower and upper CI limit "by hand"
summary(btr_logit)$coefficients[, 1] +
  qnorm(0.975) * sqrt(diag(vcov(btr_logit))) # not the same as confint(btr_logit)
summary(btr_logit)$coefficients[, 1] +
  qnorm(0.025) * sqrt(diag(vcov(btr_logit))) # not the same as confint(btr_logit)
# visit p.220 in Venables and Ripley if interested in the profile function below
profile(btr_logit)
