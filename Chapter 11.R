# ------------------------------------------------------------


#' Chapter 11

#'  Example 11.1 

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/truffles.def")

# truffles.def
# 
# p  q  ps  di  pf
# 
# Obs:   30
# 
# p = price of premium truffles, $ per ounce
# q = quantity of truffles traded in a market period, in ounces
# ps = price of choice truffles (a substitute), $ per ounce
# di = per capita disposable income, in units of $1000 per month
# pf = hourly rental price of truffle pig, $ per hour 
       # (the price of a factor of production )

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"))

head(truffles)
summary(truffles)

require(sem) || {install.packages("sem");require(sem)}
#-------------------------------------------------------

# Table 11.1

head(truffles,5)

summary(truffles)

library(mosaic)
favstats(~p, data = truffles)

#' Try OLS first
demand <- lm(q~p+ps+di, data = truffles)
summary(demand)
#' Note wrong sign on p, and insignificant

supply <- lm(q~p+pf, data = truffles)
summary(supply)


# Table 11.2a (reduced form for quantity of truffles, q)

summary(lm(q~ps+di+pf, data = truffles))

# Table 11.2b (reduced form for price of truffles, p)

summary(lm(p~ps+di+pf, data = truffles))

#-------------------------------------------------------
#' 2SLS Estimation for truffle Demand, Table 11.3a 
summary(tsls(q ~ p + ps + di, ~ps + pf + di, data = truffles))

#' 2SLS Estimation for truffle Supply, Table 11.3b 
summary(tsls(q ~ p + pf, ~ps + pf + di, data = truffles))



#' Estimated as a system, systemfit

#' Specify the two equations
eqDemand <- q~p+ps+di
eqSupply <- q~p+pf

#' Put them in a list
eqSystem <- list(demand = eqDemand, supply = eqSupply)

#--------------------------------------------------------------------
??systemfit

browseURL("https://www.jstatsoft.org/article/view/v023i04") 
#--------------------------------------------------------------------------
require(systemfit) || {install.packages("systemfit");require(systemfit)}

#' Estimate the system
fit2sls <- systemfit(eqSystem, method = "2SLS", inst=~ps+di+pf, data = truffles)
print(fit2sls)
summary(fit2sls)

??AER

install.packages("AER")
library(AER)
#' IV estimation demand
summary(ivreg(q ~ p + ps + di, ~ps + pf + di, data = truffles))

#' IV estimation supply
summary(ivreg(q ~ p + pf, ~ps + pf + di, data = truffles))

# Any difference between 2SLS and IV?
# demand
coef(tsls(q ~ p + ps + di, ~ps + pf + di, data = truffles))-coef(ivreg(q ~ p + ps + di, ~ps + pf + di, data = truffles))
# supply
coef(tsls(q ~ p + pf, ~ps + pf + di, data = truffles))-coef(ivreg(q ~ p + pf, ~ps + pf + di, data = truffles))

# --------------------------------------

#' Do a Hausman test for endogeneity of price in the demand and supply equation

#' Reduced form for price
redf.price <- lm(p~ps+di+pf, data=truffles)
# save residuals
vhat <- resid(redf.price)

#' Hausman test, p. 505, artificial regression
#' Demand
summary(lm(q~p+ps+di+vhat, data=truffles))
#' If we reject the null hypothesis that the coeff on vhat is zero, 
#' we conclude that price is endogenous.
#' vhat is significant at a 5% level.

#' Supply
summary(lm(q~p+pf+vhat, data=truffles))
#' vhat is not significant at a 5% level, hence price is 
#' exogenous in the supply equation.


#----------------------------------------------------------------


#' Example 11.2: Supply and demand at the Fulton Fish Market (in New York) 

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/fultonfish.def")

# fultonfish.def
# 
# obs:  111
# 
# date lprice quan lquan mon tue wed thu stormy mixed rainy cold totr diff change
# 
# date            date
# lprice          log(Price) of whiting( a common type of fish) per pound
# quan            Quantity of whiting sold, pounds
# lquan           log(Quantity)
# mon             Monday
# tue             Tuesday
# wed             Wednesday
# thu             Thursday
# stormy          High wind and waves
# mixed           Mixed wind and waves
# rainy           Rainy day on shore
# cold            Cold day on shore
# totr            Total received
# diff            Inventory change = totr-quan
# change          = 1 if diff large

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/fultonfish.rdata"))

head(fultonfish)

#' Table 11.4a Reduced form for ln(Quantity) Fish
summary(lm(lquan ~ stormy+mon+tue+wed+thu, data = fultonfish))

#' Table 11.4b Reduced form for ln(Price) Fish
summary(lm(lprice ~ stormy+mon+tue+wed+thu, data = fultonfish))

#' None of the daily indicator variables are statistically significant. 

#check their joint significance
library(car)
fit_price= lm(lprice ~ stormy+mon+tue+wed+thu, data = fultonfish)
linearHypothesis(fit_price , c("mon=0","tue=0","wed=0","thu=0"))

#' Also the joint F-test of significance of the daily  indicator variables
#'  indicate that they are not jointly significant.
#' This implies the supply equation is not identified. 

#' Table 11.5 2SLS Estimates for Fish Demand
summary(tsls(lquan ~ lprice+mon+tue+wed+thu , ~mon+tue+wed+thu+stormy, data = fultonfish))






