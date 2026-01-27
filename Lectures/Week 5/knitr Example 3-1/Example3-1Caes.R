setwd("~/Dropbox/MAST 90084 S1 2020/R scripts")

## for simplicity a .csv is already prepared in the right format
Caes.dat <- read.csv("Example3-1Caes.csv")
head(Caes.dat)
is.data.frame(Caes.dat)
#####################################


## alternatively, one can use dplyr package to manipulate the dataset from the Fahrmeir package
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



Caes.dat


library(nnet)  # to fit the a Multinomial log linear model, we will use the fucntion multinom

# one can also consider the "mlogit" package
Caes1=multinom(as.matrix(Caes.dat[,2:4])~NoPlan+Antib+RiskF,data=Caes.dat)

# First column of the response is treated as reference category
Caes1
summary(Caes1)  # numbers are same as the book
attributes(Caes1)

Caes1$fitted
Caes1$residuals  # resid(Caes1) does the same thing, = category means - fitted category probabilities.

Caes1.grp=Caes.dat[,2:4]
Caes1.grp[,1]=Caes.dat[,2]/Caes.dat[,1]
Caes1.grp[,2]=Caes.dat[,3]/Caes.dat[,1]
Caes1.grp[,3]=Caes.dat[,4]/Caes.dat[,1]
Caes1.grp-Caes1$fitted
##This returns the residuals given by Caes1$residuals

Caes1$deviance
predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs")
predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="class")
ratios = exp(summary(Caes1)$coeff%*%c(1, 1, 1, 0))   # prob(Inf1)/prob(noInf) and prob(Inf2)/prob(noInf)
probNoInf =   1/(sum(ratios)+1) # just checking if prob of noInf is correctly given by predict() function, and it is
# compare with predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs")
predict(Caes1, data.frame(NoPlan=1, Antib=1, RiskF=0),type="probs", se.fit=TRUE)  # doesn't seem to have a difference 


step_obj = step(Caes1)
Caes2=multinom(as.matrix(Caes.dat[,2:4])~NoPlan+Antib,data=Caes.dat)
summary(Caes2)
Caes2$edf
Caes2$deviance - Caes1$deviance
1-pchisq(24.43605,Caes1$edf - Caes2$edf)


# unfortunately I can't find a package for R that readily implements multinomial 
#logit regression with constraints to replicate the result at the end of Ex. 3.11

# maybe we can try the mph.fit function developed by J Lang!

### It also looks like the mlogit function in STATA allows us to enforce linear constraints, havn't tried it tho!