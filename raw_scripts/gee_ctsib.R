# This script reproduces the simulations on p.292 of Faraway, 2nd edition, 2016, or p.225 in the first edition

library(gee) 
library(geepack) # another package for fitting gee
library(faraway) # Faraway's book package of the datasets


options(contrasts = c("contr.treatment", "contr.poly")) 
data(ctsib)
?ctsib  # read the description of the data
head(ctsib, 20) # take a look at the data
str(ctsib)
ctsib$stable <- ifelse (ctsib$CTSIB==1, 1, 0)  # create binary responses; 1 represents completely stable, otherwise not


library(gee)
gg <- gee(stable ~
            Sex+Age+Height+Weight+Surface+Vision, id=Subject,
          family=binomial,data=ctsib,corstr="exchangeable",scale.fix=TRUE)


gg$coefficients
summary(gg)

# Faraway didn't fit with scale.fix = F, but we try it:
gg.scale <- gee(stable ~
            Sex+Age+Height+Weight+Surface+Vision, id=Subject,
          family=binomial,data=ctsib,corstr="exchangeable",scale.fix=F)


gg.scale$coefficients
gg.scale$scale




# # use the geepack:geeglm function, its estimate of dispersion is NOT based on our typical Pearson residual method of moment estimator
# See the original paper by Yan and Fine
# modgeep <- geeglm(stable ~ Sex + Age + Height + Weight + Surface + Vision, id=Subject, corstr="exchangeable", scale.fix=TRUE,  data=ctsib, family=binomial)
# modgeep$coefficients
# 
# summary(modgeep)
# summary(modgeep)$scale.value
# summary(modgeep)$dispersion
# 
# modgeep.scale <-geeglm(stable ~ Sex + Age + Height + Weight + Surface + Vision, id=Subject, corstr="exchangeable", scale.fix=FALSE,  data=ctsib, family=binomial)
# 
# modgeep.scale$coefficients
# summary(modgeep.scale)$dispersion
