#rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#######################
### erer package by Changyou Sun
### systemfit package by Arne Henningsen and Jeff D. Hamann
### dplyr by Hadley Wickham et al.
#######################
#load packages
library(systemfit)
library(erer)
library(dplyr)


#get the data
sok3008 <- read.csv("sok3008.csv", sep=";")

#rename 
mydata <- sok3008

#Define monthly dummies
mydata <- mutate(mydata, m1=ifelse(Month==1, 1,0),
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







#use erer package
#Define price share and demandshifter vectors with strings of variable names.
#Eg the vector of pricenames should be lnp1, lnp2, lnp3. ET





#Keep a data frame not time series to use with dicky-fuller
mydataClean <- mydata


#define mydata as time series





#static AIDS 
#model without monthly dummies
#Estimate LA-AIDS without munthly dummies and without restrictions




#Estimate with dummies and without restrictions
#With dummies





#' Confirm that we actually get the same results by using systemfit. 
#' We'll also use this for the Durbin Watson tests later
#' Define allt he equations as we did with the rotterdam






#define the system with eq1 and eq2 and do a systemfit. Confirm similar results






#Estimate elasticities using aiElas(). Then investigate formula on Github.






#Estimate LA-AIDS with restrictions





#Do tests using lrtest. use StaAIDSM as becnhmark (i.e. unrestricted model)





#run systemfit in order to test autocorrelation problem





#' durbinWatsonTest is in the package car
#' Should be close to 2 for no autocorrelation, i.e. e_t is correlated with e_{t-1}
#' Autocorrelation can be caused by several things. E.g. misspecification and omitted variables.
#' Can cause H_0 to be rejected when true, i.e. underestimate the variance.
#' use durbinWatsonTest() on residuals() of the model$eq[[i]]






#' Could also check if stationary or not (unit root), through e.g. 
#' Dicki-Fuller (DF), Augmented-DF, Kwiatkowski-Phillips-Schmidt-Shin test (KPSS)
#' check first on mydataClean$R1, then diff(mydataClean$R1) using tseries::adf.test()

#ADF regular share




#and differenced series:



#take care of autocorrealtion problem and non-stationarity
#Dynamic AIDS
#without restrction and without monthly dummies




#without restriciton and with monthly





#with homegeneity and with monthly




#with symestry and with monthly





#with both homegeneity and symestry and with monthly




#do tests with lrtest()




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

#########elasticitiees

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
