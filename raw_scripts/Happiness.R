library(Fahrmeir)
library(VGAM)
# library(mvord)   # not used

data(happy)

happy
?happy

## note that to reproduce the first regression in (3.5.3) of F & T we cannot use polr() since it
## doesn't allow for category-specific beta coefficients. 

#######################ARRANGE THE DATA INTO A WIDE FORM FOR DOING THE FIRST REGRESSION PART IN (3.5.3) ######
happy_tibble <- dplyr::tbl_df(happy) 

happy_grouped1 <- dplyr::group_by(happy_tibble, Sex) 

happy_summary1 <- dplyr::summarize(happy_grouped1, 
                                  school1= as.numeric(sum(n[which(School=='<12')])),
                                   school2= as.numeric(sum(n[which(School=='12')])),
                                  school3= as.numeric(sum(n[which(School=='13-16')])),
                                  school4= as.numeric(sum(n[which(School=='>16')]))
                                                                )
happy1<- as.data.frame( happy_summary1 )  
happy1

#######contrast settings for the first regression part in display (3.5.3) of F & T ###########
options('contrasts')  # view current contrast setting
contrasts(happy1$Sex)
options(contrasts = c("contr.sum", "contr.poly"))  #I realize that we need effect coding to reproduce the numbers in the book
contrasts(happy1$Sex)
contrasts(happy1$Sex) =  as.matrix(c(-1, 1))  # adjust the contrast matrix so female serves as reference 

#################### Now we do regression for the first line of display (3.5.3) in F & T ####
happy.logit1.vglm =vglm(cbind(school1,school2,school3,school4 )~Sex, 
                        data=happy1, family = cumulative())

summary(happy.logit1.vglm )# the coefficient values are  same as
# those reported in the first six row of Table 3.11 in F & T. 



attributes(happy.logit1.vglm)$predictors    # we can also look at the predictors
sqrt(diag(vcov(happy.logit1.vglm))) # same as the std. error reported by summary(happy.logit1.vglm )


#######################ARRANGE THE DATA INTO A WIDE FORM FOR DOING THE SECOND REGRESSION PART IN (3.5.3) ######

happy_grouped2 <- dplyr::group_by(happy_tibble, Sex, School)

happy_summary2 <- dplyr::summarize(happy_grouped2, 
                                  happy_low= as.numeric(sum(n[which(Rep.happiness=='Not to happy')])),
                                  happy_medium= as.numeric(sum(n[which(Rep.happiness=='Pretty happy')])),
                                  happy_high= as.numeric(sum(n[which(Rep.happiness=='Very happy')]))
)
happy2<- as.data.frame( happy_summary2 )
happy2
#######################ARRANGE THE DATA into FOR DOING THE SECOND REGRESSION IN (3.5.3) of F&T######


#############Now we go the second regression part in (3.5.3) of F&T######################
contrasts(happy2$School)  # it is correctly being effect coded, but I want to change it back to dummy code first
options(contrasts = c("contr.treatment", "contr.poly"))
contrasts(happy2$School) # < 12 is the reference level, which is sweet
happy.logit2.vglm =vglm(cbind(happy_low, happy_medium, happy_high)~  School , 
                        data=happy2, family = cumulative(link = "logitlink"))  # cumulative() must be regressed with an intercept
summary(happy.logit2.vglm ) 
happy.logit2.vglm.coeff = attributes(happy.logit2.vglm)$coeff 
happy.logit2.vglm.coeff # For now, only the two intercept estimates, which correspond to reference level <12 of School, matches the numbers in the book
# To recover the original estimates in F&T, we need to add the intercepts to each pair of coefficients for each other Schooling levels
rep(happy.logit2.vglm.coeff[1:2], 3) + happy.logit2.vglm.coeff[3:8]
# the number in the books are now recovered, as reported in the last 6 rows of F&T Table 3.11




###  this adds up  the  loglikelihood resulting from the first and second parts of (3.5.3), 
## to give the likelihood of the full GLM model
logLik_GLM = logLik(happy.logit1.vglm) + logLik(happy.logit2.vglm)

##alternatively, one can do
##a = attributes(summary(happy.logit1.vglm ))$criterion$loglikelihood+
##attributes(summary(happy.logit2.vglm ))$criterion$loglikelihood

# These get the log likelihood of individual groups
logLik(happy.logit1.vglm, sum = F)
logLik(happy.logit2.vglm, sum = F)


# Now we compute the saturated log likelihood

happy_male = happy[which(happy$Sex == "Males"), ]

happy_female = happy[which(happy$Sex == "Females"), ]

logLik_saturated= dmultinom(happy_male$n , prob = happy_male$n /sum(happy_male$n ), log = TRUE)+
  dmultinom(happy_female$n , prob = happy_female$n /sum(happy_female$n ), log = TRUE)


## this is the deviance 13.27, as reported in the book
2*(logLik_saturated- logLik_GLM)


