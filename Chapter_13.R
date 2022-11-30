
rm(list=ls())

# Example 13.1

#' Estimating a VEC model
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/gdp.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/gdp.rdata"))
str(gdp)
head(gdp)

gdp <- ts(gdp, start=c(1970,1), end=c(2000,4), frequency=4)

ts.plot(gdp[,"usa"],gdp[,"aus"], type="l", lty=c(1,2), col=c(1,2), main="GDP")
legend("topleft", border=NULL, legend=c("USA","AUS"), lty=c(1,2), col=c(1,2))

#' They have a common trend, and a nonconstant mean, 
#' the series are probably nonstationary.
library(tseries)

#' Test for stationarity, 
#' H0: Unit-root, i.e. nonstationary.
adf.test(gdp[,"usa"])
adf.test(gdp[,"aus"])

#ADF in first differences
adf.test(diff(gdp[,"usa"]))
adf.test(diff(gdp[,"aus"]))

#' The stationarity tests indicate that both series are I(1).

#' Check for cointegration.
#' Estimate the long-run relationship
library(dynlm)
fit1 <- dynlm(aus~usa-1, data=gdp)
summary(fit1) 

#' The intercept term is omitted because it has 
#' no economic meaning.
#' If USA were to increase by 1 unit, Australia would increase 
#' by 0.985 unit. 

#' residuals 
ehat <- resid(fit1)
plot(ehat)

#Autocorrelation 
library(forecast)
 ggAcf(ehat) + labs(title = "Correlogram for the residual")
 
 #The autocorrelation function (acf) is a set of correlation coefficients
 #' between a time series (the residuals in this ) and various lags of itself.

 #' A visual inspection of the the time series suggests that 
 #' the residuals may be stationary 
 
 #' A formal unit root test
fit2 <- dynlm(d(ehat)~L(ehat)-1)
summary(fit2)

#Since the cointegrating r/s doesnot contain an intercept term, 
# the 5% level critical value is -2.76.

#' Our test rejects the null of no cointegration, meaning that the series are cointegrated.
#' 
#' With cointegrated series we can construct a VEC model to better understand the causal relationship between the two variables.

#Example 13.9
vecaus<- dynlm(d(aus)~L(ehat), data=gdp)
vecusa <- dynlm(d(usa)~L(ehat), data=gdp)

summary(vecaus)
summary(vecusa)

#' The coefficient on the error correction term e_(t-1) is significant for Australia, 
#' suggesting that changes in the US (large) economy do affect Australian (small) economy.
#' 
#' The error correction coefficient in the US equation is not statistically significant,
#' suggesting that changes in Australia do not influence American economy.
#' 
#'To interpret the sign of the error correction coefficient, one should remember that e_(t-1)
#'  measures the deviation of Australian economy from its cointegrating level of...of the US economy.  
round(coef(fit1),3) 


#' Slightly more advanced approach, estimated as a system
library(tsDyn)
VECM.EG <- lineVar(gdp, lag=1, model="VECM", estim = "2OLS", I = "diff")
VECM.EG
summary(VECM.EG)

?lineVar

#Fit a VECM with Engle-Granger 2OLS estimator:
vecm.eg <- VECM(gdp, lag=1)
vecm.eg
summary(vecm.eg)

?VECM

#Fit a VECM with Johansen MLE estimator:
vecm.jo <- VECM(gdp, lag=1, estim="ML")
vecm.jo
summary(vecm.jo)

# VECM.EG <- lineVar(gdp, lag=1, model="VECM", estim = "2OLS", I = "diff")
# VECM.EG
# summary(VECM.EG)

#' The VEC model is a multivariate dynamic model that incorporates a cointegration equation.
#' It is relevant when we have two variables that are both I(1), but are cointegrated.
#' 
#' What do we do when we are interested in interdependencies when there is no cointegration?
#' Then we turn to the VAR model.


# -----------------------------------------------------



#Example 13.2

#' ## VAR models

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/fred5.def")

# Obs: 118 quarterly observations on U.S. macro variables from 1986Q1 to 2015Q2.
# 
# C or CONS      log of real personal consumption expenditure
# Y     	       log of real personal disposable income
rm(list=ls())

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/fred5.rdata"))
str(fred5)
head(fred5)

fred <- ts(fred5, start=c(1986,1),end=c(2015,2),frequency=4)

ts.plot(fred[,"consn"],fred[,"y"], type="l", lty=c(1,2), col=c(1,2))
legend("topleft", border=NULL, legend=c("consumption","income"), lty=c(1,2), col=c(1,2))

#' Are the two series stationary?
forecast::Acf(fred[,"consn"])

adf.test(fred[,"consn"]) # I(1)?
adf.test(diff(fred[,"consn"])) # Not I(0)!
adf.test(diff(diff(fred[,"consn"]))) # The consumption series is I(2)

forecast::Acf(fred[,"y"])
adf.test(fred[,"y"])
adf.test(diff(fred[,"y"])) # I(1)

#' Since one of the series, consumption, is I(2), and income is I(1),
#' 
#' there cannot exist a cointegration relationship between them.

library(vars)
Dc <- diff(fred[,"consn"])
Dy <- diff(fred[,"y"])

varmat <- as.matrix(cbind(Dc,Dy))

varfit <- VAR(varmat) # `VAR()` from package `vars`
summary(varfit)

# impulse response function 
plot(irf(varfit))

?irf

#' The dotted lines show the 95 percent interval estimates of these effects.
#' The impulse (shock) to Dc at time zero has large effects the next period,
#' but the effects become smaller and smaller as the time passes.


#' The VAR function prints the values corresponding to the impulse response graphs.
irf(varfit)

par(mfrow=c(1,1))
plot(fevd(varfit))

#' Almost 100 percent of the variance in  Dc  is caused by  Dc  itself,
#' while some 85 percent in the variance of  Dy  is caused by  Dy  and the rest is caused by  Dc . 

