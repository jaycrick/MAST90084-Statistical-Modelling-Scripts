# Reproducing example 2.1 (p.30) and 2.4 (p.51) in the F and T

# pls install Fahrmeir and the dplyr package if you haven't done so
library(conflicted)
library(dplyr)
library(Fahrmeir) ## remember to install the package "Fahrmeir" to get the dataset!
data(caesar)
?caesar # we will only use yl as response for now (yes or no infection status)
caesar # this is in a contingency table format

caesar_tibble <- as_tibble(caesar) # create a "tibble" dataframe

caesar_grouped <- group_by(caesar_tibble, noplan, factor, antib) # Group the data by different combinations of the covariates

caesar_summary <- summarize(
  caesar_grouped,
  size = sum(w),
  Infect_count = sum(yl * w)
)
caesar <- as.data.frame(caesar_summary)
# summing the total counts and infected count for each combo of the covariate values,
# and convert back to a usual data.frame

# eliminating unnecesary rows (combination of covariates) with zero cases
caesar <- caesar[which(caesar$size != 0), ]
row.names(caesar) <- seq_len(nrow(caesar))
caesar

# look at a description of the structure of caesar, like variable types
str(caesar)

# look at the coding scheme of each variables of the type "factor", using the function contrasts()
# (by default, they are usually based on dummy coding)
contrasts(caesar$noplan)
contrasts(caesar$factor)
contrasts(caesar$antib)
# note that they are coded in an exact opposite way to Example 2.1 on p.30

# we can redefine the reference levels of these variables of type "factor",
# so the coding is exactly like example 2.1 on  p.30 in F & T.
caesar$noplan <- relevel(caesar$noplan, ref = "planned")
caesar$factor <- relevel(caesar$factor, ref = "without")
caesar$antib <- relevel(caesar$antib, ref = "without")

# look at the contrasts again
contrasts(caesar$noplan)
contrasts(caesar$factor)
contrasts(caesar$antib)

# fit the regression
caesar_glm <- glm(
  Infect_count / size ~ antib + factor + noplan,
  data = caesar,
  weight = size,
  family = binomial
) # default link is logit already

caesar_glm
# the coeff estimates are the same as the numbers in example 2.1, but the default covariate names by R look kind of weird

######## Alternatively, we can force our covariates as integers, with values (1 or 0) exactly look like example 2.1 in the book #####
caesar$noplan <- as.integer(caesar$noplan == "not")
caesar$factor <- as.integer(caesar$factor == "risk factors")
caesar$antib <- as.integer(caesar$antib == "antibiotics")

# Fit the regression again
caesar_glm <- glm(
  Infect_count / size ~ antib + factor + noplan,
  data = caesar,
  weight = size,
  family = binomial
) # default link is logit already

caesar_glm # the coeff estimates are again same as the numbers in example 2.1
summary(caesar_glm)
deviance(caesar_glm)
# gives the unscaled deviance fo the model, which will be the same as scaled deviance for binomial and poisson data. (The documentation of glm() about what it is reporting for the deviance may be misleading, b/c it ignores the fact that deviance only becomes twice the log-likelihood difference when rescaled by the dispersion! )

anova(caesar_glm, test = "Chisq")
anova(caesar_glm, test = "F") # complain that F test isn't suitable here!

resid(caesar_glm, type = "deviance")
resid(caesar_glm, type = "pearson")
resid(caesar_glm, type = "working") ## caesar_glm$residuals will also give you the working residuals!
resid(caesar_glm, type = "response")
sum(resid(caesar_glm, type = "deviance")^2) # equal to caesar_glm$deviance

## Pearson and deviance test for model goodness-of-fit
1 - pchisq(caesar_glm$deviance, df = 3)
1 - pchisq(sum(resid(caesar_glm, type = "pearson")^2), df = 3)
# both reject the model: Infect_count/size ~ antib +factor + noplan at level 0.05

# compare fitted and realized proportions as on the top of p.52
round(caesar$Infect_count / caesar$size, digits = 2) # actual
round((caesar_glm)$fitted.values, digits = 2) # fitted
# should get similar result as the top table on p.52

# one can also fit with a probit link like what was done on p. 31 (towards the end of Ex 2.1)
caesar_glm_probit <- glm(
  Infect_count / size ~ antib + factor + noplan,
  data = caesar,
  weight = size,
  family = binomial(link = "probit")
)
summary(caesar_glm_probit)$coeff # estimates same as those reported by the book.


########################  Now we play with the interaction effects like in Ex 2.4  ########################

########################   adding noplan::antib interaction  ########################
caesar_glm_noplan_antib <- glm(
  Infect_count / size ~ factor + noplan * antib,
  data = caesar,
  weight = size,
  family = binomial
)

anova(caesar_glm_noplan_antib) # get the same residual deviances as reported in the book

noplan_antib_interaction_delta_deviance <- anova(
  caesar_glm_noplan_antib
)$Deviance[5]
1 - pchisq(noplan_antib_interaction_delta_deviance, df = 1) # so the noplan::factor interaction is insignificant


########################   adding noplan::factor interaction ########################

caesar_glm_noplan_factor <- glm(
  Infect_count / size ~ noplan + factor + antib + noplan:factor,
  data = caesar,
  weight = size,
  family = binomial
)

# eqiuvalently, you can do
# caesar_glm_noplan_factor <- glm(Infect_count/size ~  antib + noplan*factor,
#                    data=caesar , weight=size, family=binomial)

summary(caesar_glm_noplan_factor) # will see some enormous estimates and std.error for some coefficients

1 - pchisq(sum(resid(caesar_glm_noplan_factor, type = "pearson")^2), df = 2)
1 - pchisq(sum(resid(caesar_glm_noplan_factor, type = "deviance")^2), df = 2)
# this model is accepted at usual level of significance
# compare fitted and realized proportions as on p.53
round(caesar$Infect_count / caesar$size, 2) # actual
round(caesar_glm_noplan_factor$fitted.values, 2) # fitted


# the book recommended we should  add fictitious datum to the two zero Infect_count cells to avoid existence of MLE issue
# The following is what I do to closely reproduce the numbers on the top of p.53 in the book

caesar
caesar_augment <- caesar
caesar_augment$Infect_count[3] <- 0.5
caesar_augment$size[3] <- caesar_augment$size[3] + 1


caesar_augment # fictitious datum added to the 3rd row


caesar_glm_noplan_factor_augment <- glm(
  Infect_count / size ~ noplan + factor + antib + noplan:factor,
  data = caesar_augment,
  weight = size,
  family = binomial
)


summary(caesar_glm_noplan_factor_augment)

# actual vs fitted
round(
  rbind(
    caesar_augment$Infect_count / caesar_augment$size,
    caesar_glm_noplan_factor_augment$fitted
  ),
  digits = 2
)

# chi-square tests
1 -
  pchisq(
    sum(resid(caesar_glm_noplan_factor_augment, type = "pearson")^2),
    df = 2
  )
1 -
  pchisq(
    sum(resid(caesar_glm_noplan_factor_augment, type = "deviance")^2),
    df = 2
  )
# do not reject the new model with noplan_factor interaction at usual level such as 0.1 or 0.05
