library(conflicted)
library(Fahrmeir)
library(gee) # for robust (i.e., sandwich-estimator-type) quasi-likelihood inference
library(geepack)
library(MASS) # negative binomial glm fitting


data(cells)
cells


########################### Example 2.3###########################

## aims to study whether the two agents stimulate cell differentiation synergistically or independently.
glm_poisson <- glm(formula = y ~ TNF * IFN, family = poisson, data = cells) # just fit the Poisson GLM as in Ex 2.3


summary(glm_poisson)

summary(glm_poisson)$coeff

round(summary(glm_poisson)$coeff, digits = 4)

anova(glm_poisson)
round(glm_poisson$coeff, digits = 4)
glm_poisson$deviance # same as the book

## covariance matrices of beta
summary(glm_poisson)$cov.scaled
summary(glm_poisson)$cov.unscaled
### These two are the same, which makes sense under the exponential family assumption is true #####

########################### Example 2.6###########################
# estimated Pearson statistic, same as in Eg 2.6
pearson_chisq_stat <-
  sum((glm_poisson$y - glm_poisson$fitted.values)^2 / glm_poisson$fitted.values)

1 - pchisq(glm_poisson$deviance, df = 12)
1 - pchisq(pearson_chisq_stat, df = 12)

# both reject the model, but we should take these results with a grain of salt

# the est. dispersion in Example 2.6, very close to the book
est_disperson <- pearson_chisq_stat / glm_poisson$df.residual
est_disperson

# perform quasi "model-based" (also known as "naive") approach assuming the variance structure is correctly specified:

glm_quasi_poisson <- glm(
  formula = y ~ TNF * IFN,
  family = quasipoisson,
  data = cells
)

summary(glm_quasi_poisson)

summary(glm_quasi_poisson)$coefficients
anova(glm_quasi_poisson)
resid(glm_quasi_poisson) # deviance residual by default
resid(glm_quasi_poisson, type = "deviance")
resid(glm_quasi_poisson, type = "working")
resid(glm_quasi_poisson, type = "pearson")

## These two are different now by the scale/disperson
summary(glm_quasi_poisson)$cov.scaled
summary(glm_quasi_poisson)$cov.unscaled

summary(glm_poisson)$cov.unscaled * summary(glm_quasi_poisson)$dispersion # same as summary(glm_quasi_poisson )$cov.scaled

## WARNING: the dispersion estimate provided by glm() actually leverage the "working residuals" and the IWLS weights! So it is not the same as our standard estimate based on Pearson residuals!

summary(glm_quasi_poisson)$dispersion

with(glm_quasi_poisson, sum(weights * residuals^2)) /
  glm_quasi_poisson$df.residual

# note that the weights here are same as glm_poisson$weights, the weights by assuming phi = 1 under the full likelihood model

########################### Example 2.7###########################

# Plotting variance against mean by TNF
cells_tbl <- as_tibble(cells) # create a "tibble" dataframe
cells_tbl_grouped_tnf <- group_by(cells_tbl, TNF)
cells_tbl_summary <- summarize(
  cells_tbl_grouped_tnf,
  est_var = var(y) * 3 / 4,
  est_mean = mean(y)
)
# note that var() compute the unbiased variance estimate by default; to reproduce the results on p.59 of F&T, we have to use the biased variance estimate
cells_summary <- as.data.frame(cells_tbl_summary)
cells_summary
plot(
  cells_summary$est_mean,
  cells_summary$est_var,
  xlab = "mean",
  ylab = "variance"
)


# use the gee function for the "robust inference" in this example instead

####### this should get the second column of table 2.7 corresponding to the linear working variance structure##########
gee_mu <- gee(
  formula = y ~ TNF * IFN,
  id = seq_len(nrow(cells)),
  family = quasi(variance = "mu", link = "log"),
  data = cells
)

summary(gee_mu) # the dispersion provided by gee is the same as our own estimated dispersion based on Pearson residuals!

summary(gee_mu)$coeff
# the following should be the robust z-values, i.e. estimates rescaled by sandwich variance estimate
robust_z <- gee_mu$coeff / sqrt(diag((gee_mu)$robust.variance))
round(2 * pnorm(abs(robust_z), lower.tail = FALSE), 3)
# the following should be the estimates rescaled by model-based variance estimate
naivez <- gee_mu$coeff / sqrt(diag((gee_mu)$naive.variance))
round(2 * pnorm(abs(naivez), lower.tail = FALSE), 3)

# try to fit with geeglm from the geepack instead
gee_mu_geepack <- geeglm(
  formula = y ~ TNF * IFN,
  id = seq_len(nrow(cells)),
  family = poisson(link = "log"),
  data = cells,
  scale.fix = FALSE
)

gee_mu_geepack_fix <- geeglm(
  formula = y ~ TNF * IFN,
  id = seq_len(nrow(cells)),
  family = poisson(link = "log"),
  data = cells,
  scale.fix = TRUE
)


## I think the second column in Table 2.7 is not correctly printed; they have printed the
## naive p-values, which should have been the robust p-values

####### now the third column of table 2.7 corresponding to the purely quadratic working variance structure####

glm_gee_mu2 <- gee(
  formula = y ~ TNF * IFN,
  id = seq_len(nrow(cells)),
  family = quasi(variance = "mu^2", link = "log"),
  data = cells
)

summary(glm_gee_mu2)$scale #  dispersion, same as the one shown
robust_z <- glm_gee_mu2$coeff / sqrt(diag(glm_gee_mu2$robust.variance))
round(2 * pnorm(abs(robust_z), lower.tail = FALSE), 3)
naivez <- glm_gee_mu2$coeff / sqrt(diag(glm_gee_mu2$naive.variance))
round(2 * pnorm(abs(naivez), lower.tail = FALSE), 3)
# the robust p-values match the ones in F&T, but not the naive ones
# so all the p-values reported in parantheses of Table 2.7 in F&T should have been robust p-values
