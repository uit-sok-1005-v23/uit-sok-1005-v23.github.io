
#' POE5 - Chapter 16
rm(list=ls())

#' Data definition file:
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/transport.def")

# Obs:   21 
# 
# autotime	commute time via auto, minutes
# bustime		commute time via bus, minutes
# dtime		=(bus time - auto time)/10, 10 minute units
# auto		= 1 if auto chosen

library(mosaic)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/transport.rdata"))
View(transport)

names(transport)
head(transport)

summary(transport)
tally(~auto, data=transport) # frequency
tally(~auto, data=transport, format="percent")
gf_histogram(~auto, data=transport)


# linear probability Model

LPM <- lm(auto ~ dtime, data = transport)
summary(LPM)

confint(LPM)

#' We estimate that if travel times by public transportation and automobile
#' are equal, so that DTime =0, then the probability of a person choosing
#' automobile travel is 0.4848, close to 50-50, with 95% interval estimates
#' of [0.34, 0.63]
#' 
#' We estimate that, holding all else constant, an increase of 10 minutes
#' in the difference in travel time,increasing public transportation 
#' travel time relative to automobile travel time, increases the probability
#' of choosing automobile travel by 0.07, with a 95% interval estimates
#' of [0.05, 0.09], which seems relatively precise. 

#' The fitted model can be used to estimate the probability of 
#' automobile travel for any commuting time differentials. 
#' 
#' For Example, if dtime=0, no commuting time differentials 
f <- makeFun(LPM) 
f(dtime=0, interval="confidence")
f(0)

#' If dtime =1, a 10 minute longer commute by public transportation
#' we estimate the probability of automobile travel to be 0.5551. 
f(dtime=1)
f(1)


#' Probit model
?glm
p1 <- glm(auto ~ dtime, x=TRUE, family = binomial(link = "probit"), data = transport)
summary(p1)

#' The negative sign of the intercept implies that when commuting times
#' by bus and auto are equal so that dtime =0, individuals have a bias 
#' against driving to work, relative to public transportation. 

#' The estimated probability of a person choosing to drive to work when 
#' dtime = 0, is: 
f <- makeFun(p1) 
f(dtime=0)
f(0)

#' The positive sign of b2 indicates that an increase in public 
#' transportation travel time, relative to auto travel time, increase 
#' the probability that an individual will choose to drive to work, and 
#' this coeff is statistically significant. 

confint(p1) # confidence interval on parameters



#' Plot the predicted probabilities as a function of dtime
require(rockchalk) || {install.packages("rockchalk"); require(rockchalk)}
predictOMatic(p1) # predicted probabilities
predictOMatic(p1, interval="confidence")

plotCurves(p1, plotx = "dtime")
plotCurves(p1, plotx = "dtime", opacity=80, col="red", interval="confidence") # Wald CI

# The same plot 
plotSlopes(p1, plotx = "dtime", interval = "conf")

# erer::maTrend - Plot the predicted probabilities as a function of dtime
require(erer) || {install.packages("erer"); require(erer)}
dp=maBina(w = p1, x.mean = TRUE, rev.dum = TRUE)
tdp <- maTrend(q = dp, nam.c = "dtime", simu.c = FALSE)
tdp
plot(tdp)


# Finding the marginal effect at dtime=2 
#' (i.e., assuming travel via public transportation 
#' takes 20 minutes longer than auto travel)

f(2) 
qnorm(f(2)) # qnorm calculates the inverse of the cdf
dnorm(qnorm(f(2))) # then calculating the dnorm of the inverse gives you the pdf
dnorm(qnorm(f(2)))*coef(p1)[2] # finally, the marginal effect


#' A 10-minutes increase in the travel time via public
#' transportation increases the probability of travel via auto by
#' approximately 0.1037, given that taking the bus already requires 
#' 20 minutes more travel time than driving. 

g <- function(x) {dnorm(qnorm(f(x)))*coef(p1)[2]}
g(2)
curve(g(x), -10,10, main="Plot of the marginal effect of dtime", xlab="dtime") 
segments(2,0,2,g(2), col="blue", lty=2)
segments(2,g(2),-10,g(2), col="red", lty=2)

#' Average marginal effect
#' <http://www.rdocumentation.org/packages/erer/functions/maBina>
#' The function "maBina" calculates marginal effects for a binary 
#' probit or logit model and their standard errors.
#' x.mean =TRUE: calculate marginal effects at the means of independent variables.
#' If FALSE, marginal effects are calculated for each observation and then averaged. 
library(erer)
maBina(w = p1, x.mean = TRUE, rev.dum = TRUE) # calculated at the mean of a variable 
g(mean(transport$dtime))
maBina(w = p1, x.mean = FALSE, rev.dum = TRUE) # AME, POE4, p. 594, calculated as the mean of a each data point

#' <http://www.rdocumentation.org/packages/mfx/functions/probitmfx>
#' The function "probitmfx" from the "mfx" package calculates 
#' the marginal effects for a probit regression.
require(mfx) || {install.packages("mfx"); require(mfx)}
probitmfx(formula=auto ~ dtime, data=transport)  # calculated at the mean of a variable 
probitmfx(formula=auto ~ dtime, atmean = FALSE, data=transport) # AME, POE4, p. 594, calculated as the mean of a each data point


#' Logit model
l1 <- glm(auto ~ dtime, family = binomial(link = "logit"), data = transport)
summary(l1)

car::compareCoefs(p1,l1) # comparing the probit (model 1) and logit (mmodel 2)
coef(l1)[2]/coef(p1)[2]



# Flip coding on auto (compare the estimates from logit models)
transport <- transport %>% mutate(inv.auto = ifelse(auto==0,1,0))
il1 <- glm(inv.auto ~ dtime, family = binomial(link = "logit"), data = transport)
summary(il1)

car::compareCoefs(l1,il1)





rm(list=ls())

######################################
#' Coke
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/coke.def")

# Obs:   1140 individuals
# 
# coke       	=1 if coke chosen, =0 if pepsi chosen
# pr_pepsi        price of 2 liter bottle of pepsi
# pr_coke         price of 2 liter bottle of coke
# disp_pepsi      = 1 if pepsi is displayed at time of purchase, otherwise = 0
# disp_coke       = 1 if coke is displayed at time of purchase, otherwise = 0
# pratio          price coke relative to price pepsi

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/coke.rdata"))
View(coke)

#logit <- glm(coke~I(pr_coke/pr_pepsi)+disp_coke+disp_pepsi, family = binomial(link = "logit"), data = coke)
logit <- glm(coke~pratio+disp_coke+disp_pepsi, family = binomial(link = "logit"), data = coke, x=TRUE)
summary(logit)

#' Here we donot interpreter the coefficients, or the magnitude of the coeff. 
#' We can only interpreter the sign of the coeff. whether the explanatory 
#' variables more likely or less likely affect the choice    

library(mosaic)
# CIs using profiled log-likelihood
confint(logit)
# CIs using standard errors
confint.default(logit)

# Wald hypothesis test, POE5, p. 695
# <http://www.rdocumentation.org/packages/aod/functions/wald.test>
# Computes a Wald chi-square test for 1 or more coefficients, given their variance-covariance matrix.
require(aod) || {install.packages("aod");require(aod)}
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 2:4) # overall significance (except intercept)

#find Chi-Square critical value
qchisq(0.95, df=3, lower.tail=TRUE)

#' Since x2 > CV, we reject the null hypothesis that none of the 
#' explanatory variables help explain the choice of Coke versus Pepsi. 

probit <- glm(coke~pratio+disp_coke+disp_pepsi, family = binomial(link = "probit"), data = coke)
summary(probit)


#' Joint hypothesis, disp_coke=0 & disp_pepsi=0
#' 
#' Test the joint hypothesis that neither the Coke nor Pepsi
#' display affects the probability of choosing Coke.
require(multcomp)
summary(glht(probit, linfct = ("disp_coke+disp_pepsi = 0"))) # POE4, p. 696
#' We reject H0 and conclude that the Coke and Pepsi display has an effect on 
#' the probability of choosing Coke. 

# Alternately, using Wald test 
wald.test(b = coef(probit), Sigma = vcov(probit), Terms = 3:4) 


#' Results from both models
library(stargazer)
stargazer(probit,logit, type = "text", intercept.bottom = FALSE)
#' The parameters and their estimates vary across the models and no 
#' direct comparison is very useful, but some rules of thumb exist,
#' See the PPt (page = 29). 

#' Plot the predicted probabilities
library(rockchalk)
plotCurves(logit, plotx = "pratio")
plotCurves(logit, plotx = "pratio", opacity=80, col="red", interval="confidence") # Wald CI

plotCurves(logit, plotx = "pratio", modx = "disp_pepsi")
abline(v=mean(coke$pratio), col="red") # the mean of pratio

plotCurves(logit, plotx = "pratio", modx = "disp_coke")
abline(v=mean(coke$pratio), col="red")


# Predicted probabilities 

f <- makeFun(logit) 
f(pratio=mean(coke$pratio), disp_pepsi = 1, disp_coke = mean(coke$disp_coke))
f(pratio=mean(coke$pratio), disp_pepsi = 0, disp_coke = mean(coke$disp_coke))

P1=f(pratio=mean(coke$pratio), disp_pepsi = 1, disp_coke = mean(coke$disp_coke))
P0=f(pratio=mean(coke$pratio), disp_pepsi = 0, disp_coke = mean(coke$disp_coke))

P1-P0  

plotCurves(logit, plotx = "pratio", modx = "disp_pepsi")
abline(v=mean(coke$pratio))
abline(h=P1, col="red")
abline(h=P0, col="blue")



#' Marginal effects 
#' 
#' <http://www.rdocumentation.org/packages/erer/functions/maBina>
#' The function "maBina" calculates marginal effects for a binary 
#' probit or logit model and their standard errors.
#' x.mean =TRUE: calculate marginal effects at the means of independent variables.
#' If FALSE, marginal effects are calculated for each observation and then averaged.
require(erer)
maBina(w = logit, x.mean = TRUE, rev.dum = TRUE)


#' The function "probitmfx" and "logitmfx from the "mfx" package calculates 
#' the marginal effects for a probit and logit regressions.
#' #' <http://www.rdocumentation.org/packages/mfx/functions/probitmfx>
library(mfx)
logitmfx(formula=coke~pratio+disp_pepsi+disp_coke, data=coke)



# What is the effect of disp_pepsi when pratio is 1.5?

# A closer look at elasticities in logit models
#-------------------------------------------------------------
p <- function(l) {1/(1+(1/exp(-l)))}
p(2)

g <- function(l) {exp(-l)/(1+exp(-l))}
g(2)

h <- function(l) {1/(1+exp(l))}
h(2)

i <- function(l) {1/(1+exp(-l))}
1-i(2)

x <- seq(-2,2, length.out = 100)
curve(p, -2,2)
lines(x,g(x), col="red")
lines(x,h(x), col="blue", add=TRUE)
lines(x,1-i(x), col="green", add=TRUE)

rm(x,p,g,h,i)  # remove x, g, h, i 
#-----------------------------------------------------------------

logit

#' Elasticity as a function of pratio
e <- function(pratio,disp_coke,disp_pepsi) {
            1/(1+exp(coef(logit)[1]+pratio*coef(logit)[2]+disp_coke*coef(logit)[3]+
            disp_pepsi*coef(logit)[4]))*pratio*coef(logit)[2]}
summary(coke$pratio)
#' pratio=(pr_coke/pr_pepsi)

#' No advertising
curve(e(x,0,0), min(coke$pratio), max(coke$pratio), ylim=c(-4,0), xlab = "pratio", ylab = "elasticity",
      main = "Plot of the pratio elasticity")

#' Coke advertising
curve(e(x,1,0), min(coke$pratio), max(coke$pratio), add=T, col="blue") # Own advertising make choice of coke more inelastic

#' Pepsi advertising
curve(e(x,0,1), min(coke$pratio), max(coke$pratio), add=T, col="red") # Competitor advertising make choice of coke more inelastic

#' Both advertise
curve(e(x,1,1), min(coke$pratio), max(coke$pratio), add=T, col="green")
legend("topright", legend=c("No advertising", "Coke advertising", "Pepsi advertising","Both advertise"),
       col=c("black","blue","red", "green"), lty=c(1,1,1,1))

#' At equal price, pepsi advertising makes choice of coke more elastic
e(1,1,0) #pratio =1 (i.e., pr_coke = pr_pepsi)
e(1,1,1)

#' Elasticity as a function of pratio, ver. 2
e2 <- function(pratio,disp_coke,disp_pepsi) {(1-f(pratio,disp_coke,disp_pepsi))*pratio*coef(logit)[2]}
e2(1,1,0)
e2(1,1,1)


#' Estimate a logit model with individual prices 
names(coke)
logit2 <- glm(coke ~ pr_coke + pr_pepsi + disp_coke + disp_pepsi, family = binomial(link = "logit"), data = coke, x=TRUE)
summary(logit2)

g <- makeFun(logit2)

#' Own price elasticity
e3 <- function(pr_coke,pr_pepsi,disp_coke,disp_pepsi) {(1-g(pr_coke,pr_pepsi,disp_coke,disp_pepsi))*pr_coke*coef(logit2)[2]}

curve(e3(x,mean(coke$pr_pepsi),0,0), min(coke$pr_coke), max(coke$pr_coke), ylim=c(-2,0), xlab = "pr_coke", main="Coke own price elasticity") 
curve(e3(x,mean(coke$pr_pepsi),1,0), min(coke$pr_coke), max(coke$pr_coke), add=T, col="red") # Own advertising make choice of coke more inelastic

#' Substitute price elasticity
e4 <- function(pr_coke,pr_pepsi,disp_coke,disp_pepsi) {(1-g(pr_coke,pr_pepsi,disp_coke,disp_pepsi))*pr_pepsi*coef(logit2)[3]}
curve(e4(mean(coke$pr_coke),x,0,0), min(coke$pr_pepsi), max(coke$pr_pepsi), ylim=c(0,2), xlab = "pr_pepsi", main="Coke: substitute pepsi price elasticity") 


#' -----------------------------------------------------------------------
#browseURL("https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html")
#install.packages("margins")
library(margins)
margins(logit2)
# AME
summary(margins(logit2, type = "response"))
cplot(logit2, "pr_coke")
#plotCurves(logit2, plotx = "pr_coke", ylim=c(0.2,0.7))

# Make a plot of the own price elasticity of pepsi, from the choice of pepsi as the dependent variable
# What is the own price elasticity at the mean price of pepsi, with no advertising?

#' -----------------------------------------------------------------------







#' Multinomial logit


rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/nels_small.def")

# Obs:   1000 observations 
# 
# psechoice	= 1 if first postsecondary education was no college
# = 2 if first postsecondary education was a 2-year college
# = 3 if first postsecondary education was a 4-year college
# hscath		= 1 if catholic high school graduate
# grades		= average grade in math, english and social studies on 13 point scale with 1 = highest
# faminc		= gross 1991 family income (in $1000)
# famsiz		= number of family members
# parcoll		= 1 if most educated parent graduated from college or had an advanced degree
# female		= 1 if female
# black		= 1 if black

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/nels_small.rdata"))

View(nels_small)


library(mosaic)
#' Descriptive statistics
tally(~psechoice, data=nels_small)
tally(~psechoice, data=nels_small, format = "percent")
#' Of 1000 respondents, 22.2% selected not to attend a college upon graduation,
#'  25.1% selected to attend a 2-year college and 52.7% selected a 4-year college. 

summary(nels_small)
sd(nels_small$psechoice)
mean(nels_small$grades)
sd(nels_small$grades)

#' Two different ways to estimate multinational logit model 

#' First approach: using mlogit package 
#'install.packages("mlogit")
library(mlogit) 
d1 <- mlogit.data(nels_small, shape = "wide", choice = "psechoice")
head(d1)
m1 <- mlogit(psechoice ~ 0| grades, reflevel = "1", data = d1) # reflevel = "no college"
summary(m1)  # Table 16.2, page 706
#' Based on the estimates, what can we say?
#' Recall that a larger numerical value of GRADES represents a poorer 
#' academic performance. 
#' The parameter estimates for the coefficients of GRADES are negative and 
#' statistically significant. 
#' 
#' If the value of GRADES increases, the probability that high-school
#' graduates will choose a 2-year or a 4-year college goes down, relative
#' to the the probability of not attending college. 
#' This is the anticipated effect, as we expect that a poorer academic 
#' performance will increase the odds of not attending college. 

#-------------------------------------------------

#' Second approach: using the "multinom" function from the "nnet" package 
library(nnet)

str(nels_small)
nels_small$psechoice <- relevel(as.factor(nels_small$psechoice), ref = "1") #base/reference category

str(nels_small)

m2=multinom(psechoice ~ grades, data = nels_small)
summary(m2)

#' A nice presentation 
library(stargazer)
stargazer(m2, type = "text", intercept.bottom = FALSE,title = "The Multinomial nels_small Choice Estimates", header=FALSE)


# The multinom function does not include p-values for the regression coefficients,
# we can calculate p-values using Wald tests (here z-tests)
z <- summary(m2)$coefficients/summary(m2)$standard.errors
z

# 2-tailed p-values
p <- 2*(1 - pnorm(abs(z)))
p



#' MNLM only intercept included in the model
m3=multinom(psechoice ~ 1, data = nels_small)
summary(m3)

#Over all significance of the model using anova
anova(m2,m3)

#' prediction of probability 

#' Use the function "fitted" to predict the probabilities within the data.
head(fitted(m2), outcome = FALSE) # predicted probabilities per individual, slightly different from mlogit

predict(m2,newdata = data.frame(grades=nels_small$grades), type="probs")

predict(m2, newdata=data.frame(grades=6.64), "probs") 
predict(m2, newdata=data.frame(grades=2.635), "probs") # same as in Table 16.3

# At the same time 
predict(m2,newdata=data.frame(grades=c(6.64,2.635)) , type="probs")

newdata=data.frame(grades=mean(nels_small$grades))# prediction at the mean
predict(m2,newdata, type="probs")



# Marginal effects

#browseURL("http://www.jstor.org/stable/pdf/25046697.pdf")
#browseURL("https://cran.r-project.org/web/packages/effects/")
library(effects)
plot(effect("grades", m2, xlevels=list(grades=min(nels_small$grades):max(nels_small$grades))),lty=4,colors = "red", main = "Marginal Effect of Change in grade on the choice probabilities ")

# Same as Table 16.3
eff.models2 <- allEffects(m2, xlevels=list(grades=c(6.64, 2.635)))
eff.models2
as.data.frame(eff.models2[[1]])



# The probability ratio, or relative risk ratio
exp(coef(m2))
#' The relative risk ratio for a one-unit increase in the variable 
#' grades is 0.7344448 for being in two-year college vs. no college.

#' The relative risk ratio for a one-unit increase in the variable grades 
#' is 0.4934929 for being in four-year college vs. no college.











