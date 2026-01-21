library(Fahrmeir)
library(gee)   #for robust (i.e., sandwich-estimator-type) quasi-likelihood inference
library(MASS)  #negative binomial glm fitting


data(cells)
cells


###########################Example 2.3###########################

## aims to study whether the two agents stimulate cell differentiation synergistically or independently.
glm.Poisson = glm(formula  =  y~ TNF*IFN , 
                  family = poisson, data = cells ) # just fit the Poisson GLM as in Ex 2.3


summary(glm.Poisson)

summary(glm.Poisson)$coeff

round(summary(glm.Poisson)$coeff, digits =4)

anova(glm.Poisson)
round(glm.Poisson$coeff, digits = 4)
glm.Poisson$deviance # same as the book

##covariance matrices of beta
summary(glm.Poisson)$cov.scaled
summary(glm.Poisson)$cov.unscaled
### These two are the same, which makes sense under the exponential family assumption is true #####







###########################Example 2.6###########################
# estimated Pearson statistic, same as in Eg 2.6
Pearson.chisq.stat =  
  sum((glm.Poisson$y - glm.Poisson$fitted.values )^2/glm.Poisson$fitted.values)

1- pchisq(glm.Poisson$deviance, df = 12)
1- pchisq(Pearson.chisq.stat, df = 12)

# both reject the model, but we should take these results with a grain of salt

# the est. dispersion in Example 2.6, very close to the book
est.disperson =  Pearson.chisq.stat/glm.Poisson$df.residual
est.disperson

# perform quasi "model-based" (also known as "naive") approach assuming the variance structure is correctly specified:

glm.quasiPoisson = glm(formula  =  y~ TNF*IFN ,
                  family = quasipoisson, 
                  data = cells )

summary(glm.quasiPoisson)

summary(glm.quasiPoisson)$coefficients
anova(glm.quasiPoisson )
resid(glm.quasiPoisson)   # deviance residual by default
resid(glm.quasiPoisson, type= "deviance") 
resid(glm.quasiPoisson, type = "working")
resid(glm.quasiPoisson, type = "pearson")

## These two are different now by the scale/disperson
summary(glm.quasiPoisson )$cov.scaled
summary(glm.quasiPoisson )$cov.unscaled

summary(glm.Poisson )$cov.unscaled*summary(glm.quasiPoisson )$dispersion # same as summary(glm.quasiPoisson )$cov.scaled

## WARNING: the dispersion estimate provided by glm() actually leverage the "working residuals" and the IWLS weights! So it is not the same as our standard estimate based on Pearson residuals!

summary(glm.quasiPoisson)$dispersion

with(glm.quasiPoisson,sum(weights*residuals^2) )/glm.quasiPoisson$df.residual

# note that the weights here are same as glm.Poisson$weights, the weights by assuming phi = 1 under the full likelihood model





###########################Example 2.7###########################

#Plotting variance against mean by TNF
cells_tbl <- tibble::as_tibble(cells)    # create a "tibble" dataframe
cells_tbl_grouped_TNF <- dplyr::group_by(cells_tbl, TNF) 
cells_tbl_summary <- dplyr::summarize(cells_tbl_grouped_TNF, est_var = var(y)*3/4, est_mean = mean(y))
# note that var() compute the unbiased variance estimate by default; to reproduce the results on p.59 of F&T, we have to use the biased variance estimate
cells_summary= as.data.frame( cells_tbl_summary )
cells_summary
plot(cells_summary$est_mean, cells_summary$est_var, xlab = "mean", ylab = "variance")


# use the gee function for the "robust inference" in this example instead

#######this should get the second column of table 2.7 corresponding to the linear working variance structure##########
gee.mu= gee(formula  =  y~ TNF*IFN ,
             id = 1:nrow(cells),
                       family = quasi(variance = "mu" , link = "log"), 
                       data = cells )

summary(gee.mu) # the dispersion provided by gee is the same as our own estimated dispersion based on Pearson residuals!

summary(gee.mu)$coeff
# the following should be the robust z-values, i.e. estimates rescaled by sandwich variance estimate
Robustz = gee.mu$coeff/sqrt(diag((gee.mu)$robust.variance))   
round(2*pnorm(abs(Robustz), lower.tail = F), 3)
# the following should be the estimates rescaled by model-based variance estimate
naivez = gee.mu$coeff/sqrt(diag((gee.mu)$naive.variance))
round(2*pnorm(abs(naivez), lower.tail = F), 3)

# try to fit with geeglm from the geepack instead
gee.mu.geepack= geepack::geeglm(formula  =  y~ TNF*IFN ,
            id = 1:nrow(cells),
            family = poisson(link = "log"), 
            data = cells, scale.fix = F )

gee.mu.geepack.fix= geepack::geeglm(formula  =  y~ TNF*IFN ,
                                id = 1:nrow(cells),
                                family = poisson(link = "log"), 
                                data = cells, scale.fix = T )


##I think the second column in Table 2.7 is not correctly printed; they have printed the 
## naive p-values, which should have been the robust p-values


####### now the third column of table 2.7 corresponding to the purely quadratic working variance structure####

glm.gee.mu2= gee(formula  =  y~ TNF*IFN ,
                id = 1:nrow(cells),
                family = quasi(variance = "mu^2" , link = "log"), 
                data = cells )

summary(glm.gee.mu2)$scale #  dispersion, same as the one shown 
Robustz = glm.gee.mu2$coeff/sqrt(diag(glm.gee.mu2$robust.variance))
round(2*pnorm(abs(Robustz), lower.tail = F), 3)
naivez = glm.gee.mu2$coeff/sqrt(diag(glm.gee.mu2$naive.variance))
round(2*pnorm(abs(naivez), lower.tail = F), 3)
# the robust p-values match the ones in F&T, but not the naive ones
# so all the p-values reported in parantheses of Table 2.7 in F&T should have been robust p-values













