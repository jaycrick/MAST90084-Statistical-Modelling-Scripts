# linear random effect model
library(lme4)
library(faraway)


# paper for lme4 package: https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf



### this is from p.200, ch 10 of Faraway on random effect models
# take a look at the dataset "pulp" first which test paper brightness depending on a shift operator*-




help(pulp)

pulp

mmod <- lmer(bright ~ 1+ (1|operator), pulp)   # use REML estimate for var-cov component by default
# use  MLE if you want
smod <- lmer(bright ~ 1+(1|operator), pulp, REML=  FALSE)


# mmod is an object of class lmerMod

summary(mmod)

summary(smod)


# note that lmer doesn't report p-values for their estimates
# The between subject SD 0.21 of the MLE method is smaller than 
# the between subject SD 0.26 of the REML method because
# MLE tends to bias the estimates towards zero.

fixef(mmod)
fixef(smod)
# the fixed effect estimates of the two methods happen to be the same
# but they should generally differ

# one can also  estimate the random effects, which are the posterior means (or modes)

ranef(mmod)$operator






summary(mmod)$vcov   # gives the variance-covariance for the fixed effects, same as 0.1494^2
# Section 5.1 in lme4 's JSS paper gives more details on how this is computed

summary(mmod)$sigma  # residual standard error (Estimated standard deviation for the epsilon errors)
summary(mmod)$varcor  # report the estimated random effect std dev and the error std dev
summary(mmod)$coeff  # fixed effect coefficient, etc

components = as.data.frame(VarCorr(mmod))$vcov  # Q and sigma_epsilon^2
Vi = components[ 2]*diag(5) + components[ 1]*tcrossprod(rep(1, 5))


V = Matrix::bdiag(Vi , Vi , Vi , Vi )  # the boldfaced 
Z = rep(1, 20)


#hand-computed fixed effect estimate
(1/(t(Z)%*%solve(V)%*%Z))*t(Z)%*% solve(V)%*%pulp$bright

# hand computed standard error for fixed effect (the usual covariance matrix for the GLS (general least square) estimate)
sqrt(1/(t(Z)%*%solve(V)%*%Z))

# these numbers check out with ones reported in summary(mmod)$coeff




# one can also plot the residuals as model diagnostics
# The default residuals are computed using the estimated random effects. This means
#these residuals can be regarded as estimates of epsilons which is usually what we want
par(mfrow=  c(1, 2))
qqnorm(residuals(mmod),main="")
plot(fitted(mmod),residuals(mmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
dev.off()

# compute the residual by hand
residual_manual <- pulp$bright - rep(ranef(mmod)$operator[, 1], each  = 5) - fixef(mmod)
# they check out with the residual reported
residual_manual == residuals(mmod)



# From longitudinal data chapter (Ch 11) of Faraway 2016


help(psid)


head(psid, 20)

psid$cyear <- psid$year-78 
# as explained by Faraway, the year is centered at the year 1978 so that the intercept will predict the log income in 1978, but not 1900!

mmod <- lmer(log(income) ~ cyear*sex +age+educ+(cyear|person),psid)

smod <- lmer(log(income) ~ cyear*sex +age+educ+(cyear|person),psid, REML=  FALSE)

summary(mmod)
summary(mmod)$vcov

### this time REML and MLE give different fixed effect estimates
fixef(mmod)
fixef(smod)

sqrt(diag(summary(mmod)$vcov))
VarCorr(mmod)  # there is a random effect for both intercept and slope, and hence a correlation betwen them
