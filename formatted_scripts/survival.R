#######
## Breast Cancer
library(conflicted)
library(survival)

bcancer <- read.csv("data/survival.csv")

head(bcancer)
tail(bcancer)
# status is a "non-censored" indicator.
# 0 if the person is right censored,
# 1 if it is uncensored

# group is for staining status: positive (P) or negative (N) staining

cancer.km <- survfit(Surv(time, status) ~ group, data = bcancer) # fit KM estimator for the survival function
cancer.km
summary(cancer.km)
# ? summary.survfit

par()$las # las is 0 by default, control axis label style

# las = 1 adjusts to horizontal axis labels, even for the y-axis
orig_par <- par(mar = c(3, 3, 1, 1), las = 1) # this will assign new graphical parameter setting, but also store the original graphical parameters to be retrieved later
par()$las # las has been changed to 1
orig_par$las # orig_par$las stores the original las value
orig_par$mar # orig_par$las stores the original mar value
par()$las # current las value
par()$mar # current mar value

# plot KM survival curves for the two groups
plot(cancer.km, lwd = 2, cex.axis = 1.5)
par(orig_par) # set the graphical parameters back to the original


# check Weibull suitability
opar <- par(mar = c(4, 4, 0, 0), las = 1)
negative <- bcancer$group == "N"
positive <- bcancer$group == "P"
# set the scene by no plotting first, with type = "n"
plot(
  log(cancer.km$time),
  log(-log(cancer.km$surv)),
  type = "n",
  xlab = "Log(time)",
  ylab = "Log(-Log(S(time)))"
)
# plot the negative group
lines(
  log(cancer.km$time[negative]),
  log(-log(cancer.km$surv[negative])),
  col = "darkgreen",
  type = "b"
)
# plot the positive group
lines(
  log(cancer.km$time[positive]),
  log(-log(cancer.km$surv[positive])),
  col = "purple",
  type = "b"
)
par(opar) # restores original graphical parameter setting
par()$las # current las value
par()$mar # current mar value


# check PH assumption
# plot transformed survival curve
opar <- par(mar = c(2, 2, 0, 0), las = 1)
help(plot.survfit)
# create a cloglog survival curve
# plot(cancer.km, lty = 1:2, col = c("Red", "Blue"), xlim = c(5, 300))
plot(
  cancer.km,
  fun = "cloglog",
  lty = 1:2,
  col = c("Red", "Blue"),
  xlim = c(5, 300)
)
legend(
  "topleft",
  legend = c("Negative Stain", "Positive Stain"),
  lty = 1:2,
  col = c("Red", "Blue"),
  bty = "n"
)
par(opar)


########################
# exponential regression

# (a)
cancer_exp <- survreg(
  Surv(time, status) ~ group,
  data = bcancer,
  dist = "exponential"
)
summary(cancer_exp)

# wald test
wald_stat <- cancer_exp$coeff[2] / sqrt(cancer_exp$var[2, 2])
2 * pnorm(-abs(wald_stat)) # p-value

# LR test
lr_stat <- -2 * (cancer_exp$loglik[1] - cancer_exp$loglik[2])
pchisq(q = lr_stat, df = 1, lower.tail = FALSE) # p-value

# (b)
exp(-cancer_exp$coef[1])
# (Intercept)
# 0.003026634
exp(-sum(cancer_exp$coef))
# [1] 0.007838746

# (c)
sum(bcancer$status[bcancer$group == "N"]) /
  sum(bcancer$time[bcancer$group == "N"]) # MLE of lambda for group "N" using the formula
# [1] 0.003026634
sum(bcancer$status[bcancer$group == "P"]) /
  sum(bcancer$time[bcancer$group == "P"]) # MLE of lambda  for group "P" using the formula
# [1] 0.007838746

## (d)  compute CI using delta method
beta <- cancer_exp$coeff[2]
var_beta <- (cancer_exp$var)[2, 2]
var_delta <- (exp(-beta))^2 * var_beta
# CI upper bound
exp(-beta) + qnorm(0.975) * sqrt(var_delta)
exp(-beta) - qnorm(0.975) * sqrt(var_delta)
# the lower bound doesn't cross to the -ve number region, which is lucky

## one can also compute the CI using the fact that exp(-x) is a monotone transformation
exp(-(beta - qnorm(0.975) * sqrt(var_beta)))
exp(-(beta + qnorm(0.975) * sqrt(var_beta)))
# This guarantees that the lower bound doesn't cross to the -ve number region

#######################
# weibull regression
cancer_wbl <- survreg(
  Surv(time, status) ~ group,
  data = bcancer,
  dist = "weibull"
)
summary(cancer_wbl)
cancer_wbl$coef
cancer_wbl$scale
cancer_wbl$var


# (a)
lambda_n <- exp(-cancer_wbl$coef[1])

lambda_p <- exp(-sum(cancer_wbl$coef))

alpha <- 1 / cancer_wbl$scale

s_n_100 <- exp(-(lambda_n * 100)^alpha)
s_p_100 <- exp(-(lambda_p * 100)^alpha)
lambda_n
lambda_p
alpha
s_n_100
s_p_100

# (b)

hazard_ratio <- (lambda_p / lambda_n)^alpha

hazard_ratio
# (c) details to be added
## Wald
log(cancer_wbl$scale) / sqrt(cancer_wbl$var[3, 3])

# The p-value is
2 * pnorm(-abs(log(cancer_wbl$scale) / sqrt(cancer_wbl$var[3, 3])))

## LR
2 * (cancer_wbl$loglik[2] - cancer_exp$loglik[2])

qchisq(.95, df = 1)
pchisq(
  2 * (cancer_wbl$loglik[2] - cancer_exp$loglik[2]),
  df = 1,
  lower.tail = FALSE
)
