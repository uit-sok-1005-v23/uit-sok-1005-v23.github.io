rm(list=ls())

#' ## Chapter 10

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/mroz.def")
# mroz.def
# 
# taxableinc federaltax hsiblings hfathereduc hmothereduc siblings lfp hours kidsl6 kids618 age educ wage wage76 hhours hage heduc hwage faminc mtr mothereduc fathereduc unemployment largecity exper
# 
# Obs: 753
# 
# variable   		description
# -----------------------------------------------------------------------------
# taxableinc     	Taxable income for household 
# federaltax     	Federal income taxes 
# hsiblings       husband's number of siblings  
# hfathereduc     husband's father's education level
# hmothereduc     husband's mothers's education level
# siblings       	Wife's number of siblings
# lfp        	dummy variable = 1 if woman worked in 1975, else 0
# hours       	Wife's hours of work in 1975
# kidsl6        	Number of children less than 6 years old in household
# kids618       	Number of children between ages 6 and 18 in household
# age         	Wife's age
# educ         	Wife's educational attainment, in years
# wage         	Wife's 1975 average hourly earnings, in 1975 dollars
# wage76       	Wife's wage reported at 1976 interview, for 1976
# hhours       	Husband's hours worked in 1975
# hage         	Husband's age
# heduc         	Husband's educational attainment, in years
# hwage         	Husband's wage, in 1975 dollars
# faminc     	Family income, in 1975 dollars
# mtr        	marginal tax rate facing the wife, includes Soc Sec taxes
# mothereduc      wife's mother's education level
# fathereduc      wife's father's education level
# unemployment   	Unemployment rate in county of residence
# bigcity        	Dummy variable = 1 if live in large city (SMSA), else 0
# exper         	Actual years of wife's previous labor market experience
# 
# 
# THE MROZ DATA FILE IS TAKEN FROM THE 1976 PANEL STUDY OF INCOME
# DYNAMICS, AND IS BASED ON DATA FOR THE PREVIOUS YEAR, 1975.  OF THE 753
# OBSERVATIONS, THE FIRST 428 ARE FOR WOMEN WITH POSITIVE HOURS
# WORKED IN 1975, WHILE THE REMAINING 325 OBSERVATIONS ARE FOR WOMEN
# WHO DID NOT WORK FOR PAY IN 1975.

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"))
head(mroz)
library(mosaic)

#' Example 10.1

# women working
work <- mroz %>% filter(lfp==1)

fit <- lm(log(wage)~educ+exper+I(exper^2), data = work)
summary(fit)

######################################## Example 10.2###########
fit2 <- lm(log(wage)~educ, data = work)
summary(fit2)

#' Education might be endogenous.
#' Education of mother might be used as an instrument.
cor(educ~mothereduc, data = work)

#install.packages("AER")
library(AER)
## model 
fit2.iv <- ivreg(log(wage) ~ educ | mothereduc, data = work)
summary(fit2.iv)

################################Example 10.3##################
#' Behind the curtains, first stage
lm(educ~mothereduc, data = work)
#' Then use the predicted educ from the first stage as an instrument in the second stage
lm(log(wage)~predict(lm(educ~mothereduc, data = work)), data = work)
coef(fit2.iv)

#' The coefficients are the same, but note that the se are not.

##################################Example 10.4########################
#' More instruments, FATHER EDUCATION, IN ADDITION
summary(lm(educ~mothereduc+fathereduc, data = work))

fit2.iv2 <- ivreg(log(wage) ~ educ | mothereduc+fathereduc, data = work)
summary(fit2.iv2)

######################################Example 10.5##############################
fit.iv <- ivreg(log(wage) ~ educ+exper+I(exper^2) | exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(fit.iv)

####################################Example 10.7####################
#'  Hausman test for endogeneity
first.stage <- lm(educ~exper+I(exper^2)+mothereduc+fathereduc, data = work)
summary(first.stage) # Table 10.1

#' Save the residuals of first stage
vhat <- resid(first.stage)

#' Estimate second stage by OLS, and include vhat (residuals from first stage)
second.stage.ols <- lm(log(wage) ~ educ+exper+I(exper^2)+vhat, data = work)
summary(second.stage.ols)
#' If we reject the null hypothesis that the coefficient on vhat is zero, we conclude that education is endogenous.
#' vhat is significant at a 10% level

#' Test for surplus instruments N*R^2
summary(lm(resid(fit.iv) ~ exper+I(exper^2)+mothereduc+fathereduc, data = work))$r.square*428


