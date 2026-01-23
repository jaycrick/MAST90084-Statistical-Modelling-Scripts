library(conflicted)
library(dplyr)
library(nnet) # to fit the a Multinomial log linear model, we will use the function multinom()
library(VGAM)
library(glmnet)
library(Fahrmeir)
library(tidyr)

## for simplicity a .csv is already prepared in the right format
caes_dat <- read.csv("data/Example3-1Caes.csv")
head(caes_dat)
is.data.frame(caes_dat)
str(caes_dat)


# #####################################
#
# # alternatively, one can use dplyr package to manipulate the dataset from the Fahrmeir package
#
# caesar_df <- as_tibble(caesar) # create a "tibble" dataframe
# caesar_grouped <- group_by(caesar_df, noplan, factor, antib) # Group the data by different combinations of the covariates
# caesar_summary <- summarize(
#   caesar_grouped,
#   size = sum(w),
#   Noinf = w[which(y == 3)],
#   Inf1 = w[which(y == 1)],
#   Inf2 = w[which(y == 2)]
# )
# caesar <- as.data.frame(caesar_summary)
#
#
# # eliminating unnecesary rows (combination of covariates) with zero cases
# caesar <- caesar[which(caesar$size != 0), ]
# rownames(caesar) <- seq_len(nrow(caesar))
#
# ########  These make the values of the factors exactly look like example 2.1 in the book #####
# caesar$factor <- as.integer(caesar$factor == "risk factors")
# caesar$noplan <- as.integer(caesar$noplan == "not")
# caesar$antib <- as.integer(caesar$antib == "antibiotics")

?multinom
# multinom() use BFGS to fit, which is different from Fisher scoring and is a quasi-Newton method

caes_nnet <- multinom(
  as.matrix(caes_dat[, 2:4]) ~ NoPlan + Antib + RiskF,
  data = caes_dat
)

# First column of the response is treated as reference category
caes_nnet
summary(caes_nnet) # numbers are same as the book
summary(caes_nnet)$coeff
t(summary(caes_nnet)$coeff)
attributes(caes_nnet)
caes_nnet$deviance # note that this deviance reported is simply minus two the loglikelihood (hasn't been adjusted by the saturated model ); read the documentation

# also try the glmnet() package
caes_glmnet <- glmnet(
  x = cbind(as.matrix(caes_dat[, 5:7])),
  y = as.matrix(caes_dat[, 2:4]),
  family = "multinomial",
  lambda = c(0)
)
caes_glmnet$beta

# manipulate the components of caes_glmnet to get a coefficient matrix of the same shape as summary(caes_nnet)$coeff
beta_matrix <- cbind(
  caes_glmnet$beta[[2]] - caes_glmnet$beta[[1]],
  caes_glmnet$beta[[3]] - caes_glmnet$beta[[1]]
)

beta_matrix <- rbind(
  caes_glmnet$a0[-1] - caes_glmnet$a0[1],
  beta_matrix
)
t(as.matrix(beta_matrix))


## we can also fit it with the original "wide format" data from the Fahrmeir package
data(caesar)
caesar
?caesar # y = 1 is Inf1, y = 2 is Inf2, y = 3 is noInf
# we need to force the reference level to be y =3
as.factor(caesar$y) # the first level shown is the default reference level
caesar$y <- relevel(caesar$y, ref = "3")

# we also need to make sure the other covariates have the reference level of our preferred choice
as.factor(caesar$noplan)
caesar$noplan <- relevel(caesar$noplan, ref = "planned")
as.factor(caesar$antib)
caesar$antib <- relevel(caesar$antib, ref = "without")
as.factor(caesar$factor)
caesar$factor <- relevel(caesar$factor, ref = "without")

caes_nnet2 <- multinom(y ~ noplan + antib + factor, data = caesar, weights = w)

summary(caes_nnet2)
caes_nnet2$deviance # same as caes_nnet$deviance

##########################################################
##########################################################
### Now we explore other features of caes_nnet
caes_nnet$fitted
caes_nnet$residuals # resid(caes_nnet) does the same thing, which is  "category means - fitted category probabilities".

caes_nnet_grp <- caes_dat[, 2:4]
caes_nnet_grp[, 1] <- caes_dat[, 2] / caes_dat[, 1]
caes_nnet_grp[, 2] <- caes_dat[, 3] / caes_dat[, 1]
caes_nnet_grp[, 3] <- caes_dat[, 4] / caes_dat[, 1]
caes_nnet_grp - caes_nnet$fitted
## This returns the residuals given by caes_nnet$residuals

caes_nnet$deviance
predict(caes_nnet, data.frame(NoPlan = 1, Antib = 1, RiskF = 0), type = "probs")
predict(caes_nnet, data.frame(NoPlan = 1, Antib = 1, RiskF = 0), type = "class")
ratios <- exp(summary(caes_nnet)$coeff %*% c(1, 1, 1, 0)) # prob(Inf1)/prob(noInf) and prob(Inf2)/prob(noInf)
prob_no_inf <- 1 / (sum(ratios) + 1) # just checking if prob of noInf is correctly given by predict() function, and it is

prob_no_inf
# compare with predict(caes_nnet, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs")
predict(
  caes_nnet,
  data.frame(NoPlan = 1, Antib = 1, RiskF = 0),
  type = "probs",
  se.fit = TRUE
) # doesn't seem to have a difference
##########################################################
##########################################################

#####################################################################################################################################################################################################################################
# It is good to try the VGAM package, which fits these models with Fisher scoring

caes_vgam <- vglm(
  formula = cbind(noInf, Inf1, Inf2) ~ NoPlan + Antib + RiskF,
  data = caes_dat,
  family = multinomial(refLevel = "noInf")
)


# by default the LAST column of the response matrix is treated as the reference level, so we need to specify refLevel = 1

summary(caes_vgam)
coefvlm(caes_vgam, matrix.out = TRUE) # grab the coefficients
# In the summary, the coeff of (Intercept):1  should correspond to the first response column that is not the reference level. If you shift the order, the order of the coefficients appearing in summary() will also change as :
summary(vglm(
  formula = cbind(noInf, Inf2, Inf1) ~ NoPlan + Antib + RiskF,
  data = caes_dat,
  family = multinomial(refLevel = "noInf")
))

fittedvlm(caes_vgam) # get the fitted probabilities

# basically, up to numerical differences,  the same estimated coeff values as caes_nnet, likely because Fisher scoring is used here instead
summary(caes_nnet)$coeff


# One can also use the versatile "parallel" argument to set some coefficients to be the same across the classes (This is part of Example 3.11)

caes_vgam_parallel <- vglm(
  formula = cbind(noInf, Inf1, Inf2) ~ NoPlan + Antib + RiskF,
  data = caes_dat,
  family = multinomial(refLevel = "noInf", parallel = TRUE ~ NoPlan + RiskF - 1)
)
coefvlm(caes_vgam_parallel, matrix.out = TRUE) # both Antib and the Intercept have uniform coefficients, because the intercept is implicitly included in the rhs of TRUE ~ Antib, unless you do -1


# we can also try another style using the caesar dataset from Fahrmeir's package, attempt:

vglm(
  formula = y ~ noplan + antib + factor,
  data = caesar,
  weights = w,
  family = VGAM::multinomial(refLevel = "3")
) # this prompts errors


# we have to filter out the rows with zero counts first, otherwise vglm will complain
caesar_no_zero_w <- dplyr::filter(caesar, w > 0) # this is same as  caesar[which(caesar$w!=0), ]
names(caesar_no_zero_w)
levels(caesar_no_zero_w$noplan)
levels(caesar_no_zero_w$antib)
levels(caesar_no_zero_w$factor)
# the reference levels are the first category that appear, they are not what we want
# can be reset by using relevel()
caesar_no_zero_w$noplan <- relevel(caesar_no_zero_w$noplan, ref = "planned")
caesar_no_zero_w$antib <- relevel(caesar_no_zero_w$antib, ref = "without")
caesar_no_zero_w$factor <- relevel(caesar_no_zero_w$factor, ref = "without")

caes_vgam2 <- vglm(
  formula = y ~ noplan + antib + factor,
  data = caesar_no_zero_w,
  weights = w,
  family = VGAM::multinomial(refLevel = "3")
)

summary(caes_vgam2)
coefvlm(caes_vgam2, matrix.out = TRUE) # coefficients same as before


# These two gives different log likelihoods and deviances!!!!!
attributes(caes_vgam)$criterion # the deviance reported here is exacty the same as p.109 on F&T, so we can trust that it is adjusted with the saturated model already
attributes(caes_vgam2)$criterion # the deviance reported here is simply -2 times the loglikelihood
# One can also directly obtain
logLik(caes_vgam)
logLik(caes_vgam2)


## some data manipulation below attempting to retrieve  a "long" form of caesar from caes_dat
caesar_new <- pivot_longer(
  data = caes_dat[, -1],
  cols = noInf:Inf2,
  names_to = "inf_type",
  values_to = "w"
)
caesar_new$inf_type <- factor(
  caesar_new$inf_type,
  levels = unique(caesar_new$inf_type)
) # change inf_type to a factor, with reference level noInf
as.data.frame(caesar_new)
# an older version of pivot_longer() is gather(), but that one isn't in active development anymore
caes_nnet_tibble <- multinom(
  inf_type ~ NoPlan + Antib + RiskF,
  data = caesar_new,
  weights = w
)
summary(caes_nnet_tibble)$coeff

summary(caes_nnet2)$coeff # same coefficients as before

# final remark: The R packages mlogit and mnlogit also allow us to fit these models with category-specific covariates, i.e. those in section 3.2.2 of F & T; see their respective papers in the Journal of Statistical Softwares (JSS) for more details.
