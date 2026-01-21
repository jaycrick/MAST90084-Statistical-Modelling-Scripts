## for simplicity a .csv is already prepared in the right format
Caes.dat <- read.csv("Example3-1Caes.csv")
head(Caes.dat)
is.data.frame(Caes.dat)
str(Caes.dat)


# #####################################
# 
# 
# # alternatively, one can use dplyr package to manipulate the dataset from the Fahrmeir package
# 
# caesar_df <- dplyr::tbl_df(Fahrmeir::caesar)    # create a "tibble" dataframe
# caesar_grouped <- dplyr::group_by(caesar_df, noplan, factor, antib)  # Group the data by different combinations of the covariates
# caesar_summary <- dplyr::summarize(caesar_grouped, size = sum(w),
#                                    Noinf = w[which(y==3)],
#                                    Inf1 =  w[which(y==1)],
#                                    Inf2 =  w[which(y==2)])
# caesar <- as.data.frame( caesar_summary )
# 
# 
# #eliminating unnecesary rows (combination of covariates) with zero cases
# caesar  = caesar[which(caesar$size != 0), ]
# rownames(caesar) = 1:nrow(caesar)
# 
# ########  These make the values of the factors exactly look like example 2.1 in the book #####
# caesar$factor = as.integer(caesar$factor == "risk factors")
# caesar$noplan = as.integer(caesar$noplan == "not")
# caesar$antib = as.integer(caesar$antib == "antibiotics")





library(nnet)  # to fit the a Multinomial log linear model, we will use the function multinom()
?multinom
# multinom() use BFGS to fit, which is different from Fisher scoring and is a quasi-Newton method



Caes_nnet<-nnet::multinom(as.matrix(Caes.dat[,2:4])~NoPlan+Antib+RiskF,data=Caes.dat)

# First column of the response is treated as reference category
Caes_nnet
summary(Caes_nnet)  # numbers are same as the book
summary(Caes_nnet)$coeff
t(summary(Caes_nnet)$coeff)
attributes(Caes_nnet)
Caes_nnet$deviance  # note that this deviance reported is simply minus two the loglikelihood (hasn't been adjusted by the saturated model ); read the documentation

# also try the glmnet() package
library(glmnet)
Caes_glmnet<-glmnet(x = cbind( as.matrix(Caes.dat[, 5:7])),
                    y = as.matrix(Caes.dat[,2:4]) , 
                    family = "multinomial", lambda= c(0))
Caes_glmnet$beta

# manipulate the components of Caes_glmnet to get a coefficient matrix of the same shape as summary(Caes_nnet)$coeff
beta_matrix = 
  cbind(Caes_glmnet$beta[[2]] - Caes_glmnet$beta[[1]] ,
Caes_glmnet$beta[[3]] - Caes_glmnet$beta[[1]] )

beta_matrix = rbind(
Caes_glmnet$a0[-1]- Caes_glmnet$a0[1],
beta_matrix 
)
t(as.matrix(beta_matrix))


## we can also fit it with the original "wide format" data from the Fahrmeir package
library(Fahrmeir)
data(caesar)
caesar
?caesar   # y = 1 is Inf1, y = 2 is Inf2, y = 3 is noInf
# we need to force the reference level to be y =3
as.factor(caesar$y)  # the first level shown is the default reference level
caesar$y  = relevel(caesar$y, ref = "3")

# we also need to make sure the other covariates have the reference level of our preferred choice
as.factor(caesar$noplan)
caesar$noplan  = relevel(caesar$noplan, ref = "planned")
as.factor(caesar$antib)
caesar$antib  = relevel(caesar$antib, ref = "without")
as.factor(caesar$factor)
caesar$factor  = relevel(caesar$factor, ref = "without")

Caes_nnet2<-multinom(y~noplan+antib+factor,data=caesar, weights = w)

summary(Caes_nnet2)
Caes_nnet2$deviance  # same as Caes_nnet$deviance

##########################################################
##########################################################
### Now we explore other features of Caes_nnet
Caes_nnet$fitted
Caes_nnet$residuals  # resid(Caes_nnet) does the same thing, which is  "category means - fitted category probabilities".

Caes_nnet.grp=Caes.dat[,2:4]
Caes_nnet.grp[,1]=Caes.dat[,2]/Caes.dat[,1]
Caes_nnet.grp[,2]=Caes.dat[,3]/Caes.dat[,1]
Caes_nnet.grp[,3]=Caes.dat[,4]/Caes.dat[,1]
Caes_nnet.grp-Caes_nnet$fitted
##This returns the residuals given by Caes_nnet$residuals

Caes_nnet$deviance
predict(Caes_nnet, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs")
predict(Caes_nnet, data.frame(NoPlan=1, Antib=1, RiskF=0),type="class")
ratios = exp(summary(Caes_nnet)$coeff%*%c(1, 1, 1, 0))   # prob(Inf1)/prob(noInf) and prob(Inf2)/prob(noInf)
probNoInf =   1/(sum(ratios)+1) # just checking if prob of noInf is correctly given by predict() function, and it is

probNoInf
# compare with predict(Caes_nnet, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs")
predict(Caes_nnet, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs", se.fit=TRUE)  # doesn't seem to have a difference 
##########################################################
##########################################################



 

#####################################################################################################################################################################################################################################
# It is good to try the VGAM package, which fits these models with Fisher scoring
library(VGAM)

Caes_vgam <-vglm(formula = cbind(noInf, Inf1, Inf2)~NoPlan+Antib+RiskF,
                       data = Caes.dat,
                family = multinomial(refLevel = "noInf"))



# by default the LAST column of the response matrix is treated as the reference level, so we need to specify refLevel = 1

summary(Caes_vgam)
coefvlm(Caes_vgam,  matrix.out= TRUE) # grab the coefficients
# In the summary, the coeff of (Intercept):1  should correspond to the first response column that is not the reference level. If you shift the order, the order of the coefficients appearing in summary() will also change as : 
summary(vglm(formula = cbind(noInf, Inf2, Inf1)~NoPlan+Antib+RiskF,
     data = Caes.dat,
     family = multinomial(refLevel = "noInf")))

fittedvlm(Caes_vgam) # get the fitted probabilities

# basically, up to numerical differences,  the same estimated coeff values as Caes_nnet, likely because Fisher scoring is used here instead
summary(Caes_nnet)$coeff



# One can also use the versatile "parallel" argument to set some coefficients to be the same across the classes (This is part of Example 3.11)

Caes_vgam_parallel <-vglm(formula = cbind(noInf, Inf1, Inf2)~NoPlan+Antib+RiskF,
                          data = Caes.dat,
                          family = multinomial(refLevel = "noInf", parallel = TRUE ~NoPlan + RiskF -1 ))
coefvlm(Caes_vgam_parallel,  matrix.out= TRUE) # both Antib and the Intercept have uniform coefficients, because the intercept is implicitly included in the rhs of TRUE ~ Antib, unless you do -1




# we can also try another style using the caesar dataset from Fahrmeir's package, attempt:

VGAM::vglm(formula = y~noplan+antib+factor,data=Fahrmeir::caesar, weights = w,
           family = VGAM::multinomial(refLevel= "3"))  # this prompts errors
  

# we have to filter out the rows with zero counts first, otherwise vglm will complain
caesar_no_zero_w = dplyr::filter(Fahrmeir::caesar, w>0)   # this is same as  caesar[which(caesar$w!=0), ] 
names(caesar_no_zero_w)
levels(caesar_no_zero_w$noplan)
levels(caesar_no_zero_w$antib)
levels(caesar_no_zero_w$factor)
# the reference levels are the first category that appear, they are not what we want
# can be reset by using stat::relevel()
caesar_no_zero_w$noplan <- relevel(caesar_no_zero_w$noplan, ref = "planned")
caesar_no_zero_w$antib <-relevel(caesar_no_zero_w$antib, ref = "without")
caesar_no_zero_w$factor <- relevel(caesar_no_zero_w$factor, ref = "without")

Caes_vgam2 <-VGAM::vglm(formula = y~noplan+antib+factor,data=caesar_no_zero_w, weights = w,
                   family = VGAM::multinomial(refLevel= "3"))
  
summary(Caes_vgam2)
coefvlm(Caes_vgam2, matrix.out = T)   # coefficients same as before


# These two gives different log likelihoods and deviances!!!!!
attributes(Caes_vgam)$criterion  # the deviance reported here is exacty the same as p.109 on F&T, so we can trust that it is adjusted with the saturated model already
attributes(Caes_vgam2)$criterion  # the deviance reported here is simply -2 times the loglikelihood
# One can also directly obtain
logLik(Caes_vgam)
logLik(Caes_vgam2)


## some data manipulation below attempting to retrieve  a "long" form of caesar from Caes.dat
caesar_new <- tidyr::pivot_longer(data = Caes.dat[, -1], 
                           cols  = noInf:Inf2, 
                           names_to = "inf_type",
                           values_to = "w")
caesar_new$inf_type = factor(caesar_new$inf_type, levels = unique(caesar_new$inf_type)) # change inf_type to a factor, with reference level noInf
as.data.frame(caesar_new)
# an older version of pivot_longer() is gather(), but that one isn't in active development anymore
Caes_nnet_tibble<-multinom(inf_type~NoPlan+Antib+RiskF,data=caesar_new, weights = w)
summary(Caes_nnet_tibble)$coeff

summary(Caes_nnet2)$coeff # same coefficients as before


# final remark: The R packages mlogit and mnlogit also allow us to fit these models with category-specific covariates, i.e. those in section 3.2.2 of F & T; see their respective papers in the Journal of Statistical Softwares (JSS) for more details.