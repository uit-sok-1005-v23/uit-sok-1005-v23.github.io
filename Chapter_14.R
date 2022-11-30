rm(list=ls())
r <- read.table("http://www.principlesofeconometrics.com/poe4/data/dat/byd.dat")
names(r) <- "r"
head(r)
plot(r$r, type="l")
abline(h=mean(r$r), col="red", lwd=2)
#The time series shows evidence of time-varying volatility and clustering

plot(density(r$r))
#Summary of the data 
summary(r)
sd(r$r) 
#Normality test 
install.packages("tseries")
library(tseries)
jarque.bera.test(ts(r$r))

#So, the time series shows evidence of time-varying volatility and clustering, and 
#the unconditional distribution of is non-normal. 


#-------------------------------------
#random.r <- rnorm(500,1,1)
#summary(random.r)
#plot(r$r, type="l")
#lines(random.r, type="l", col="red")
#-------------------------------------------------

#
#

#Testing ARCH effects and estimating volatility

#First, estimate the mean equation(in this example we take, rt=b0 +et) 
#where rt is the monthly return.
#Second, retrieve the estimated residuals, and fitt a model for the residuals 
#Third, estimate (14.3) and us LM test 
#Fourth, estimate the variance equation, if the data has an ARCH effect

rt=ts(r, freq=1)

# Step-1: the mean equation 
m1 <- lm(rt~1)
summary(m1)

#step-2:Regress residual square ~ constant plus lagged residual square (eq14.3) 
library(dynlm)
e2=ts(resid(m1)^2,freq=1)  
m2 <- dynlm(e2~L(e2, 1))
summary(m2)

#step-3: Use LM test, Ho:no ARCH vs H1: ARCH effect  
#LM-statistic=(T-q)*R^2, compare with  chisqr(0.95,q=1), where q - is the order of lag in the rhS

#Reject H0 if LM-stastic is greater than or equal to chisqr(0.95,q=1)
out=summary(m2)
names(out)

out$df[2]*out$r.squared   
## [1] 61.91036
# Since  61.91036 >chisqr(0.95,1)=3.841
# we reject the null hypothesis.


#You can also Check Autocorrelation
# We reject the null hypothesis of no autocorrelation
# if the p-value is smaller than alpha.
Box.test(r^2)
Box.test(r^2, type="Ljung-Box") 

#step-4: Estimating the variance model 

#Function GARCH(p,q) from the tseries package, becomes an ARCH model when
#used with the order= c(0,1). 

#This function can be used to estimate and plot the variance ht 
require(tseries) 
byd.arch <- garch(ts(resid(m1),freq=1) ,c(0,1))
summary(byd.arch)

#Fitted values, volatility/variance 
hhat <- ts(2*byd.arch$fitted.values[-1,1]^2)
plot.ts(hhat)


#The GARCH model
#Using package rugarch for GARCH models.

#https://cran.r-project.org/web/views/Finance.html
library(rugarch)

#Both the mean and the variance models can be estimated 
#simultaneously using the package "rugarch".
# There are two steps to fit GARCH or ARCH model using rugarch pachage.

# First step, specify the mean and the variance model using the function "ugarchspec"
#Second step, fit the model using the function "ugarchfit"

#ugarchspec - is method for creating a univariate GARCH specification object prior to fitting.
??ugarchspec

garchSpec <- ugarchspec(
  variance.model=list(model="sGARCH",garchOrder=c(1,0)),  # Standard GARCH model (sGARCH)
  mean.model=list(armaOrder=c(0,0)), 
  distribution.model="std")

#ugarchfit - method for fitting a variety of univariate GARCH models.
#??ugarchfit

garchFit <- ugarchfit(spec=garchSpec, data=rt)
coef(garchFit)

names(garchFit@fit) # @ call-the call of the garch function

#Fitted value of the mean model
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)

#Fitted values of the variance model
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat, main="Standard GARCH model (sGARCH) with dataset 'byd'")

#
#There are differnt GRACH models 
# T-GARCH model
#this model allows us to model asymmetric effect treating bad news and good 
# news terms in the model
#GARCH-in-Mean, Exponential GARCH (EGARCH) model, etc. 

#In R, the "fGARCH model" subsumes many GARCH models. 

# TGARCH 
garchMod <- ugarchspec(variance.model=list(model="fGARCH",
                                           garchOrder=c(1,1),
                                           submodel="TGARCH"),
                       mean.model=list(armaOrder=c(0,0)), 
                       distribution.model="std")

garchFit <- ugarchfit(spec=garchMod, data=rt)
coef(garchFit)

#Fitted mean model 
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)

#Fitted variance 
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat, main="The tGARCH model with dataset 'byd' ")


# GARCH-in-mean
garchMod <- ugarchspec(
  variance.model=list(model="fGARCH",
                      garchOrder=c(1,1),
                      submodel="APARCH"),
  mean.model=list(armaOrder=c(0,0),
                  include.mean=TRUE,
                  archm=TRUE,
                  archpow=2), 
  distribution.model="std")

garchFit <- ugarchfit(spec=garchMod, data=rt)
coef(garchFit)

#Fitted mean model
rhat <- garchFit@fit$fitted.values
plot.ts(rhat)

#Fitted variance model
hhat <- ts(garchFit@fit$sigma^2)
plot.ts(hhat, main="Version of the GARCH-in-mean model with dataset 'byd' ")

#---------------------------------------------------------------------
require(fGarch) 

# eq 14.4a & b
summary(garchFit(~garch(1,0),r)) 

#' r=b0=mu
#' a0=omega
#' a1=alpha1

#plot(garchFit(~garch(1,0),r))

summary(garchFit(~garch(1,1),r)) # chap 14.4.1 eq.14.7
#' r=mu
#' a0=omega
#' a1=alpha1
#' b1=beta1
#plot(garchFit(~garch(1,1),r))
#----------------------------------------------------------------------

#install.packages("rugarch")
library(rugarch)
r_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0)))
r_garch11_fit <- ugarchfit(spec = r_garch11_spec, data = r)
r_garch11_fit

#------------------------------------------------------------------------
#Backtesting VaR
r_garch11_roll <- ugarchroll(r_garch11_spec, r, n.start = 120, refit.every = 1,
                             refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                             VaR.alpha = 0.01, keep.coef = TRUE)

report(r_garch11_roll, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)

# Forecast
r_garch11_fcst <- ugarchforecast(r_garch11_fit, n.ahead = 20)
r_garch11_fcst

#plot(r_garch11_fcst)

# Threshold Garch
# Tsay
# Source/run these functions first
# http://faculty.chicagobooth.edu/ruey.tsay/teaching/introTS/Tgarch11.R
#m3=Tgarch11(r)

## 4) Garch in mean 
# Tsay
# Source/run these functions first
# http://faculty.chicagobooth.edu/ruey.tsay/teaching/introTS/garchM.R
#m4=garchM(r)


#Prediction can be obtained using the function ugarchboot() from the package ugarch.
#Supplementary:



