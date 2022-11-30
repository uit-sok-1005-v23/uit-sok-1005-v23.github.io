
rm(list=ls())

#######################
### erer package by Changyou Sun
### systemfit package by Arne Henningsen and Jeff D. Hamann
### dplyr by Hadley Wickham et al.
#######################

#load packages
library(systemfit)
library(erer)
library(dplyr)


#setting directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#get the data
sok3008 <- read.csv("sok3008.csv", sep=";")

#rename 
mydata <- sok3008
head(mydata)

#Define monthly dummies
mydata <- mutate(mydata,
               m1=ifelse(Month==1, 1,0),
               m2=ifelse(Month==2, 1, 0),
               m3=ifelse(Month==3, 1, 0),
               m4=ifelse(Month==4, 1, 0),
               m5=ifelse(Month==5, 1, 0),
               m6=ifelse(Month==6, 1, 0),
               m7=ifelse(Month==7, 1, 0),
               m8=ifelse(Month==8, 1, 0),
               m9=ifelse(Month==9, 1, 0),
               m10=ifelse(Month==10, 1, 0),
               m11=ifelse(Month==11, 1, 0),
               m12=ifelse(Month==12, 1, 0))

#Define all variables, pi, lnpi, totextp, lny, Ri, lnP, lnyP
mydata <- mutate(mydata, p1=XCod/QCod, 
                         p2=XHaddock/QHaddock,
                         p3=XAlaska/QAlaska, 
                 
                        lnp1=log(p1),  
                        lnp2=log(p2), 
                        lnp3=log(p3),
                 
                        totexp=XCod+XHaddock+XAlaska,
                 
                        lny=log(totexp),
                        
                        R1=XCod/totexp, 
                        R2=XHaddock/totexp,
                        R3=XAlaska/totexp,
                 
                       lnP=R1*lnp1+R2*lnp2+R3*lnp3,   # stone's price index 
                       lnyP=lny-lnP)        

names(mydata)

head(mydata)
summary (mydata)   


#use erer package
#Define price, share and demandshifter vectors with strings of variable names.
#Eg the vector of pricenames should be lnp1, lnp2, lnp3. ET
PriceNames <- c("lnp1", "lnp2", "lnp3")
ShareNames <- c("R1", "R2", "R3")
demandshifters <- c("m2", "m3", "m4", "m5", "m6","m7","m8", "m9","m10", "m11","m12")


#Keep a data frame not time series to use with dickey-fuller 
mydataClean <- mydata

#define mydata as time series
mydata <- ts(mydata) # declare that mydata is a time series
class(mydata)

#static AIDS 
#model without monthly dummies
#Estimate LA-AIDS without monthly dummies and without restrictions
StatAIDS <- aiStaFit(y=mydata, share=ShareNames, price=PriceNames,expen="lnyP",
                   omit="R3", hom=FALSE, sym=FALSE)
summary (StatAIDS$est)

#Estimate with dummies and without restrictions
#With dummies
StatAIDSM <- aiStaFit(y=mydata, share=ShareNames, price=PriceNames,expen="lnyP",
                    omit="R3", hom=FALSE, sym=FALSE, shift=demandshifters)
summary(StatAIDSM$est)

#############################################################################
#' Confirm that we actually get the same results by using systemfit. 
#' We'll also use this for the Durbin Watson tests later
#' Define all the equations as we did with the rotterdam model 
eq1 <- R1~lnp1+lnp2+lnp3+lnyP+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
eq2 <- R2~lnp1+lnp2+lnp3+lnyP+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
eq3 <- R3~lnp1+lnp2+lnp3+lnyP+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12


#define the system with eq1 and eq2 and do a systemfit. Confirm similar results
system12 <- list(eq1, eq2)
AIDSsta12 <- systemfit(system12, data=mydata, method="SUR")
summary(AIDSsta12)

##################################################################################

# Go back to our estimation using the erer package 
StatAIDSM

#Estimate elasticities using aiElas(). Then investigate formula on Github.
aiElas(StatAIDSM)


#Estimate LA-AIDS with restrictions
# with homogeneity only  
homStatAIDSM <- aiStaFit(y=mydata, share=ShareNames, price=PriceNames,expen="lnyP",
                       omit="R3", hom=TRUE, sym=FALSE, shift=demandshifters)
# with symmetry only 
symStatAIDSM <- aiStaFit(y=mydata, share=ShareNames, price=PriceNames,expen="lnyP",
                       omit="R3", hom=FALSE, sym=TRUE, shift=demandshifters)
# with both homogeneity and symmetry 
homsymStatAIDSM <- aiStaFit(y=mydata, share=ShareNames, price=PriceNames,expen="lnyP",
                          omit="R3", hom=TRUE, sym=TRUE, shift=demandshifters)

#Do tests using lrtest. use StaAIDSM as benchmark (i.e., unrestricted model)
lrtest(homStatAIDSM$est,StatAIDSM$est)  # comparing models with without restrictions and model with homo restrictions   
qt(0.95,2)

lrtest(symStatAIDSM$est,StatAIDSM$est)  # comparing models with without restrictions and model with symmetry restrictions

lrtest(homsymStatAIDSM$est,StatAIDSM$est) # comparing models with without restrictions and model with both homo & symmetry restrictions

#run systemfit in order to test autocorrelation problem
system12 <- list(eq1, eq2)
AIDSsta12 <- systemfit(system12, data=mydata, method="SUR")
summary(AIDSsta12)

#' durbinWatsonTest is in the package car
#' Should be close to 2 for no autocorrelation, i.e. e_t is correlated with e_{t-1}
#' Autocorrelation can be caused by several things. E.g. misspecification and omitted variables.
#' Can cause H_0 to be rejected when true, i.e. underestimate the variance.

durbinWatsonTest(residuals(AIDSsta12$eq[[1]]))
durbinWatsonTest(residuals(AIDSsta12$eq[[2]]))

#' Could also check if stationary or not (unit root), through e.g. 
#' Dicky-Fuller (DF), Augmented-DF, Kwiatkowski-Phillips-Schmidt-Shin test (KPSS)

#for ADF:
#library(tseries)
tseries::adf.test((mydataClean$R1))
#' Since the p-value is not less than .05, we do not reject H0. 
#' This means the time series is non-stationary. In other words, 
#' it has some time-dependent structure and does not have constant variance over time.

#and differenced series:

tseries::adf.test(diff(mydataClean$R1))

#take care of autocorrealtion problem and non-stationarity

#Dynamic AIDS

#without restrction and without monthly dummies
DyAIDS <- aiDynFit(StatAIDS,AR1=TRUE)
summary(DyAIDS$est)

summary(StatAIDS$est)

#without restriciton and with monthly
DyAIDSM <- aiDynFit(StatAIDSM,AR1=TRUE)
summary(DyAIDSM$est)

#with homegeneity and with monthly
homDyAIDSM <- aiDynFit(homStatAIDSM,AR1=TRUE, mode=c("dynamic"))
summary(homDyAIDSM$est)

#with symestry and with monthly
symDyAIDSM <- aiDynFit(symStatAIDSM,AR1=TRUE, mode=c("dynamic"))
summary(symDyAIDSM$est)


#with both homegeneity and symestry and with monthly
homsymDyAIDSM <- aiDynFit(homsymStatAIDSM,AR1=TRUE)
summary(homsymDyAIDSM$est)

#do tests with lrtest()
lrtest(homDyAIDSM$est, DyAIDSM$est)
lrtest(symDyAIDSM$est, DyAIDSM$est)
lrtest(homsymDyAIDSM$est, DyAIDSM$est)

aiElas (homsymDyAIDSM)

################################################################################
################################################################################
#################################### miceconaids ###############################
################################################################################
################################################################################

#' The following code is created by Arne Henningsen and is included in the
#' Vignette of the micEconAids-package. See 
#' https://cran.r-project.org/web/packages/micEconAids/index.html
#' and
#' https://cran.r-project.org/web/packages/micEconAids/vignettes/micEconAids_vignette.pdf
#' All creds goes to Arne Henningsen for both the micEconAids and the systemfit-package.

library(micEconAids)
data("Blanciforti86")

nrow(Blanciforti86)

B86 <- Blanciforti86[1:32,]

priceNames <- c( "pFood1", "pFood2", "pFood3", "pFood4" )
shareNames <- c( "wFood1", "wFood2", "wFood3", "wFood4" )

laaidsResult <- aidsEst(priceNames, shareNames, "xFood", data=B86,
                        priceIndex="S")

summary(laaidsResult$est)

#' To correct a problem where budget shares are in both the dependant and in stone price
#' index, can used lagged shares in the Stone Price:

laaidsResultSL <- aidsEst(priceNames, shareNames, "xFood", data=B86, priceIndex="SL")

#The price index can easily be changed for Paasche, Laspeyres, and Tornqvist:

#Paasche
laaidsResultP <- aidsEst(priceNames, shareNames, "xFood", data=B86, priceIndex="P")
#Laspeyres
laaidsResultL <- aidsEst(priceNames, shareNames, "xFood", data=B86, priceIndex="L")
#Tornqvist
laaidsResultT <- aidsEst(priceNames, shareNames, "xFood", data=B86, priceIndex="T")

#' Estimating the non-linear AIDS has been proven a bit more complicated. Can
#' be done by using an iterative linear least squares estimator (ILLE) which first
#' estimates initial values of the share equiation, and the second step
#' the translog price index can be updated with the newly estimated coefficients:

aidsResult <- aidsEst(priceNames, shareNames, "xFood", data=B86, method="IL")

summary(aidsResult$est)

aidsResultNon <- aidsEst( priceNames, shareNames, "xFood", data = B86,
                          method = "IL", sym = FALSE, hom=FALSE)
summary(aidsResultNon$est)

######### elasticitiees

pMeans <- colMeans(B86[, priceNames])
wMeans <- colMeans(B86[, shareNames])

aidsResultElas <- aidsElas(coef(aidsResult), prices=pMeans, 
                           shares=wMeans)

summary(aidsResultElas)
#Can easily apply the delta method:

aidsResultElasCov <- aidsElas(coef(aidsResult), prices=pMeans,
                              shares=wMeans, coefCov=vcov(aidsResult),
                              df=df.residual(aidsResult))

summary(aidsResultElasCov)

#######This can actually be done much easier:

elas(aidsResult)

