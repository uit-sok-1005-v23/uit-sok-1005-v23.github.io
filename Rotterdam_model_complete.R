
#remove all previous objects from R
rm(list=ls())

# loading the important packages 
library(systemfit) #systemfit
library(dplyr) #for mutate() and lag()
library(car) #linearHypothesis


#Set the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#reads the file
 sok3008 <- read.csv("sok3008.csv", sep=";")

# data and variables
mydata <- sok3008
head(mydata)

#Quick investigation of the data with summary and plots QCod etc
summary (mydata$QCod);summary (mydata$QHaddock); summary (mydata$QAlaska)
plot(mydata$QCod, type='l')
plot(mydata$QHaddock, type='l')
plot(mydata$QAlaska, type='l')


#add monthly dummies to the data.frame (use ifelse(mydata$Month==i, 1, 0))
#use variable name "mi", e.g. for January, we get "m1"
mydata$m1 <- ifelse(mydata$Month==1, 1, 0)
mydata$m2 <- ifelse(mydata$Month==2, 1, 0)
mydata$m3 <- ifelse(mydata$Month==3, 1, 0)
mydata$m4 <- ifelse(mydata$Month==4, 1, 0)
mydata$m5 <- ifelse(mydata$Month==5, 1, 0)
mydata$m6 <- ifelse(mydata$Month==6, 1, 0)
mydata$m7 <- ifelse(mydata$Month==7, 1, 0)
mydata$m8 <- ifelse(mydata$Month==8, 1, 0)
mydata$m9 <- ifelse(mydata$Month==9, 1, 0)
mydata$m10 <- ifelse(mydata$Month==10, 1, 0)
mydata$m11 <- ifelse(mydata$Month==11, 1, 0)
mydata$m12 <- ifelse(mydata$Month==12, 1, 0)

head(mydata,10)

#We'll now define the variables. 
#Use 1=cod, 2=haddock, 3=Alaska such that given pi, then p1 is the price of cod,
#p2 is the price of haddock etc. lnp1 is log price for cod. R1 is budget share for cod etc.

#calculate prices, "pi", and add it to the data.frame
mydata$p1 <- mydata$XCod/mydata$QCod
mydata$p2 <- mydata$XHaddock/mydata$QHaddock
mydata$p3 <- mydata$XAlaska/mydata$QAlaska

#define log prices "lnpi". PS. in R, log() is by default the natural logarithm. 
mydata$lnp1 <- log(mydata$p1)
mydata$lnp2 <- log(mydata$p2)
mydata$lnp3 <- log(mydata$p3)

#define log quantity, "lnqi"
mydata$lnq1 <- log(mydata$QCod)
mydata$lnq2 <- log(mydata$QHaddock)
mydata$lnq3 <- log(mydata$QAlaska)

#Define TotEXP (XCod+XHaddock+XAlaska)
mydata$TotEXP <- mydata$XCod+mydata$XHaddock+mydata$XAlaska

#Define Budget shares "Ri"
mydata$R1 <- mydata$XCod/mydata$TotEXP
mydata$R2 <- mydata$XHaddock/mydata$TotEXP
mydata$R3 <- mydata$XAlaska/mydata$TotEXP

#Define diff log prices "dlnpi"

mydata$dlnp1 <- mydata$lnp1-lag(mydata$lnp1)
mydata$dlnp2 <- mydata$lnp2-lag(mydata$lnp2)
mydata$dlnp3 <- mydata$lnp3-lag(mydata$lnp3)

#define diff log quantity "dlnqi"
mydata$dlnq1 <- mydata$lnq1-lag(mydata$lnq1)
mydata$dlnq2 <- mydata$lnq2-lag(mydata$lnq2)
mydata$dlnq3 <- mydata$lnq3-lag(mydata$lnq3)

#define two-period moving average shares (mean share), "Si"
mydata$S1 <- (mydata$R1+lag(mydata$R1))/2
mydata$S2 <- (mydata$R2+lag(mydata$R2))/2
mydata$S3 <- (mydata$R3+lag(mydata$R3))/2

#'Define the dependent variables, i.e. weighted change in the
#' quantity consumed, "Sidlnqi"
mydata$S1dlnq1 <- mydata$S1*mydata$dlnq1
mydata$S2dlnq2 <- mydata$S2*mydata$dlnq2
mydata$S3dlnq3 <- mydata$S3*mydata$dlnq3

#Define the Divisa Volume Index, "dlnEXP" (i.e. sum of dependent variables)
mydata$dlnEXP <- mydata$S1dlnq1+mydata$S2dlnq2+mydata$S3dlnq3

head(mydata)

#Remove first row
mydata <- mydata[-1,]


summary (mydata$p1);summary (mydata$p2); summary (mydata$p3)

plot (mydata$p1, type='l')
plot (mydata$p2, type='l')
plot (mydata$p3, type='l')

summary (mydata$R1);summary (mydata$R2);summary (mydata$R3)



# Estimate the model 


#' storing the means of the budget shares for later because we know
#'  we're going to use these to calculate elasticities. 
#'  Name the variables "ms1", "ms2" etc

ms1 <- mean(mydata$S1)
ms2 <- mean(mydata$S2)
ms3 <- mean(mydata$S3)

ms1+ms2+ms3

#' Defining our demand system in single equation first:
#system without monthly dummies
eq1 <- S1dlnq1~dlnp1+dlnp2+dlnp3+dlnEXP
eq2 <- S2dlnq2~dlnp1+dlnp2+dlnp3+dlnEXP
eq3 <- S3dlnq3~dlnp1+dlnp2+dlnp3+dlnEXP

#system with monthly dummies
eq1 <- S1dlnq1~dlnp1+dlnp2+dlnp3+dlnEXP+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
eq2 <- S2dlnq2~dlnp1+dlnp2+dlnp3+dlnEXP+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
eq3 <- S3dlnq3~dlnp1+dlnp2+dlnp3+dlnEXP+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12


#' note: whether to estimated model with dummies or without dummies depends on 
#' your study, and your data

#' First we will define the model without homogeneity and symmetry restriction.
#' This will be our kind of "benchmark" model

#' First define the system using list() on the eqs. We must omit equation 3 
#' because of singularity

system12 <- list (eq1, eq2)

#' Then estimate the model using systemfit(system12, data, method="SUR"). 
#' Then, check summary. 
Rot12 <- systemfit (system12, data=mydata, method="SUR")
summary (Rot12)


#' We will define our null hypothesis for join hypothesis tests as follows:
#' 
#' The null hypothesis is here: H0: The monthly dummies are jointly equal to zero:

resmonth <- c("eq1_m2=0", "eq1_m3=0", "eq1_m4=0","eq1_m5=0","eq1_m6=0","eq1_m7=0","eq1_m8=0",
           "eq1_m9=0", "eq1_m10=0","eq1_m11=0","eq1_m12=0", 
           "eq2_m2=0", "eq2_m3=0", "eq2_m4=0","eq2_m5=0","eq2_m6=0","eq2_m7=0","eq2_m8=0",
           "eq2_m9=0", "eq2_m10=0","eq2_m11=0","eq2_m12=0")     

#' The wald test which computes the chi square:

linearHypothesis (Rot12, resmonth, test="Chisq")

#' Find the critical chi squared - remember to input correct DF 
#' (22 for this one. Can be seen in the output above):

qchisq(0.95,22)

#' We're able to reject the null hypothesis in this case since the computed chi square 
#' is larger than the critical chi squared, i.e. the montly dummies
#' are not jointly equal to zero, and we continue with our monthly model.
#' OBS! Don't base this on the joint test alone. If the data really is not seasonal,
#' it might be a really poor choice to include seasonal dummies as the reduction in DFs
#' might be bad.


#' Here we will define our theoretical restrictions in the same way as we did
#'  with our
#' monthly H0's (think about the substitution matrix):

#homogeneity H0: The equations are homogeneous
reshom <- c("eq1_dlnp1+eq1_dlnp2+eq1_dlnp3=0","eq2_dlnp1+eq2_dlnp2+eq2_dlnp3=0" )

# symmetry: H0: The equations are symmetric
ressym <- c("eq1_dlnp2-eq2_dlnp1=0")

#Homo and sym: H0: the equations are homogeneous AND symmetric:

reshomsym <- c("eq1_dlnp1+eq1_dlnp2+eq1_dlnp3=0",
            "eq2_dlnp1+eq2_dlnp2+eq2_dlnp3=0",
            "eq1_dlnp2-eq2_dlnp1=0")

#test homogeneity
linearHypothesis (Rot12, reshom, test="Chisq")
qchisq(0.95,2) #critical chi-squared
#' Not able to reject since critical > computed

#test symmetry
linearHypothesis (Rot12, ressym, test="Chisq")
qchisq(0.95, 1) #critical chi-squared
#' Not able to reject since critical > computed

#test homogeity and symmetry together
linearHypothesis (Rot12, reshomsym, test="Chisq")

qchisq(0.95, 3) #critical chi-squared
#' Not able to reject since critical > computed

#' To sum up: we're not able to reject any of the homogeneity and symmetry joint
#' hypothesis. This can be a good sign, and can be an indication that the data is somewhat in 
#' accordance with our theoretical restrictions (remember that the H0's are that
#' our equations are in fact homogeneous and symmetric, and since we're not able to 
#' reject that, we keep going in our estimations with our fully theoretical restricted model)
#' 
#' IF the symmetry and homo restriction are rejected, you should still consider continuing
#' with the restricted model which will hopefully yield estimates which makes more sense, but you 
#' can and should discuss a bit (a few lines) why your 
#' model and the data does not support the theoretical restrictions.

#We can now define models with either single or both restrictions imposed:

#Rotterdam model with homogeneity
Rot12h <- systemfit(system12, data=mydata, method="SUR", restrict.matrix=reshom)
summary(Rot12h)

#Rotterdam model with symmetry
Rot12s <- systemfit(system12, data=mydata, method="SUR", restrict.matrix=ressym)
summary(Rot12s)

#Rotterdam model with both homogeneity and symmetry 
Rot12hs <- systemfit(system12, data=mydata, method="SUR", restrict.matrix=reshomsym)
summary(Rot12hs)


#' We will now estimate our elasticities. We continue with the fully restricted model, Rot12hs.
#' First we need to extract the coefficients from our model:


#' Easy way for Elasticities based on model with both homogeneity and symmetry imposed
#' The coef() function will extract the coefficients from a summary table and their 
#' corresponding errors and t and p-values. You can also use Rot12hs$coefficients
#' to just extract the coefficients
#' 
coef(summary(Rot12hs)) 
coeff <- round(coef(summary(Rot12hs)), digits=3) # get a matrix for whole information of estimated coefficients
#Just extract relevant information. Remember to adjust this if you have 4 goods or other included variables:

#define first a vector which contains all relevant varaibles to make code easier to read:

relCoeffs <- c("eq1_dlnp1", "eq1_dlnp2", "eq1_dlnp3", "eq1_dlnEXP",
               "eq2_dlnp1", "eq2_dlnp2", "eq2_dlnp3", "eq2_dlnEXP")

coeff[relCoeffs,]

coeffRelevant <- coeff[relCoeffs,]  

coeffnum <- coeff[relCoeffs,1] #just extract coefficients without standard errors etc
coeffnum 

#' Here we're making a vector repeating the means to use to find elasticities 
mw <- c(rep(ms1,4),rep(ms2,4)) # mean vector of budget shares

# calculate elasticites: eij=THETAij/msi
els <- round(coeffnum/mw, digit=3) #estimated coefficients/mean of market share
els
#output <- cbind(coeffRelevant, mw, els)#present estimated coefficients with t value and p value, mean of market share and then elasticities together

#print elasticities in a table
elasq1 <- els[1:4]
elasq2 <- els[5:8]
elsoutput <- rbind(elasq1, elasq2)
elsoutput

#define mean for all coefficients, including seasonal dummies:
#mw16 <- c(rep(ms1,16),rep(ms2,16))

#names(summary(Rot12hs)) #to see what have been included in the results of summary


# Use the Slutsky eqtn to find the Marshallian elasticities: eij=eij*-RjAi
#e11
e11 <- els[1]-ms1*els[4] 
e12 <- els[2]-ms2*els[4]
e13 <- els[3]-ms3*els[4]

e21 <- els[5]-ms1*els[8] 
e22 <- els[6]-ms2*els[8]
e23 <- els[7]-ms3*els[8]

#remember, we keep the income elasticity. This does not change. Construct final matrix with expenditure elast.

marshMatrix <- round(rbind(c(e11, e12, e13, elasq1[4]), c(e21,e22,e23, elasq2[4])), digits=3)
marshMatrix 


########## estimate model with equation 1 and 3 to get results for equation 3 #####################################

# define the system for eq1 and eq3 and its restrictions 
system13 <- list (eq1, eq3)
Rot13 <- systemfit (system13, data=mydata, method="SUR")
summary (Rot13)

# model with homogeneity 
reshom13 <- c("eq1_dlnp1+eq1_dlnp2+eq1_dlnp3=0","eq2_dlnp1+eq2_dlnp2+eq2_dlnp3=0" )
Rot13h <- systemfit (system13, data=mydata, method="SUR",restrict.matrix=reshom13)
summary (Rot13h)

# model with symmetry 
ressym13 <- c("eq1_dlnp3-eq2_dlnp1=0")
Rot13s <- systemfit (system13, data=mydata, method="SUR",restrict.matrix=ressym13)
summary (Rot13s)

# Model with both 
reshomsym13 <- c("eq1_dlnp1+eq1_dlnp2+eq1_dlnp3=0",
              "eq2_dlnp1+eq2_dlnp2+eq2_dlnp3=0",
              "eq1_dlnp3-eq2_dlnp1=0" )

Rot13hs <- systemfit(system13, data=mydata, method="SUR", restrict.matrix=reshomsym13)
summary(Rot13hs)


##################################################################
########### Test adding up example
##################################################################

#From the homogeneity restriction we have that sum_j(theta_ij)=0, consequently, 
#for 3 goods: theta31=-theta11-theta21
#
coef(Rot12hs)
theta11 <- coef(Rot12hs)[2] # coefficent of eq1_dlnp1
theta21 <- coef(Rot12hs)[18] # coefficent of eq2_dlnp1
theta31 <- coef(Rot13hs)[18] # coefficent of eq2_dlnp1

-theta11-theta21
theta31

#From engel aggregation, we have that sum_i(mu_i)=1, then for 3 goods
#mu3=1-mu1-mu2
coef(Rot12hs)
mu1 <- coef(Rot12hs)[5] # coeff. of eq1_dlnEXP 
mu2 <- coef(Rot12hs)[21] # coeff  eq2_dlnEXP 
mu3 <- coef(Rot13hs)[21]  # coeff  eq2_dlnEXP 

1-mu1-mu2
mu3

#The adding up of the intercept, sum_i(alpha_i)=0, i.e.
#alpha3=-alpha1-alpha2
coef(Rot12hs)
alpha1 <- coef(Rot12hs)[1]   #eq1_(Intercept) 
alpha2 <- coef(Rot12hs)[17]  #eq2_(Intercept) 
alpha3 <- coef(Rot13hs)[17]  #eq2_(Intercept) 

-alpha1-alpha2
alpha3

#Cournot aggregation, sum_i(theta_ij)=0
coef(Rot12hs)
theta11 <- coef(Rot12hs)[2] #eq1_dlnp1 
theta12 <- coef(Rot12hs)[3] #eq1_dlnp2 
theta13 <- coef(Rot13hs)[4] # eq1_dlnp3

-theta11-theta12
theta13

# Easy way for Elasticities based on model with both homo and symm restrictions


######################################################################################################

# extract coefficients for eq 1 and 3.

#Just extract relevant information. Remember to adjust this if you have 4 goods or other included variables:
#define first a vector which contains all relevant variables to make code easier to read:

#' eventhough we're now working with the third equation in our demand system, we still
#' have to use the "eq2" notation since we still only have estimated two equations.
#' I.e. R will still lable equation 3 as equation 2 in its output:
#' 
coeff13 <- round(coef(summary(Rot13hs)), digits=3)  #get a matrix for whole information of estimated coefficients
coeff13

relCoeffs13 <- c("eq1_dlnp1", "eq1_dlnp2", "eq1_dlnp3", "eq1_dlnEXP",
               "eq2_dlnp1", "eq2_dlnp2", "eq2_dlnp3", "eq2_dlnEXP")

coeffRelevant13 <- coeff13[relCoeffs13,]  
coeffRelevant13

coeffnum13 <- coeff13[relCoeffs13,1] #just extract coefficients without standard errors etc
coeffnum13

#' Here we're making a vector
mw13 <- c(rep(ms1,4),rep(ms3,4)) # mean vector of budget shares

# cacluate elasticites:  eij=THETAij/msi
els13 <- round(coeffnum13/mw13, digit=3) #estimated coefficients/mean of market share
els13 
#output13 <- cbind(coeffRelevant13, mw13, els13)#present estimated coefficients with t value and p value, 
                                               #mean of market share and then elasticities together

#print elasticities in a table
elasq1 <- els13[1:4]
elasq3 <- els13[5:8]
elsoutput13 <- rbind(elasq1, elasq3)
elsoutput13 

#define mean for all coefficients, including seasonal dummies:
mw1613 <- c(rep(ms1,16),rep(ms3,16))

#names(summary(Rot13hs)) #to see what have been included in the results of summary




######################################################################################################
######################################################################################################
######################################################################################################

#' we can now create the final Hicksian elasticity matrix:
elsoutput
elsoutput13

elasMatrix <- rbind(elsoutput, elsoutput13[2,])

colnames(elasMatrix) <- c("p1", "p2", "p3", "EXP")
rownames(elasMatrix) <- c("q1", "q2", "q3")

#Our full elasticity matrix is then:
elasMatrix

#Often, you will see results presented in its transposed form, e.g. Paper (1) Kinnuckan. 
#We can use the function t() to find the transpose of a matrix in R:

t(elasMatrix)

#You decide how you want to present it in your paper



######################################################################################################
######################################################################################################
######################################################################################################


# Use the Slutsky eqtn to find the Marshallian elasticities: eij=eij*-RjAi
#e11
e11 <- els13[1]-ms1*els13[4] #2, 3, 4..is the position of hicksian elasticites in els maxtrix
e12 <- els13[2]-ms2*els13[4]
e13 <- els13[3]-ms3*els13[4]

e31 <- els13[5]-ms1*els13[8] #2, 3, 4..is the position of hicksian elasticites in els maxtrix
e32 <- els13[6]-ms2*els13[8]
e33 <- els13[7]-ms3*els13[8]

#marshallian elasticity matrix:
round(rbind(marshMatrix, c(e31, e32, e33, elasq3[4])), digits=3)




#####################################
#####################################
#####################################
#Restrictions for 4 equations (If you are interested to consider four goods)
#####################################
#####################################
#####################################



#homogeneity H0: The equations are homogeneous
reshom <- c("eq1_dlnp1+eq1_dlnp2+eq1_dlnp3+eq1_dlnp4=0",
            "eq2_dlnp1+eq2_dlnp2+eq2_dlnp3+eq2_dlnp4=0",
            "eq3_dlnp1+eq3_dlnp2+eq3_dlnp3+eq3_dlnp4=0" )

# symmetry: H0: The equations are symmetric
ressym <- c("eq1_dlnp2-eq2_dlnp1=0",
            "eq1_dlnp3-eq3_dlnp1=0",
            "eq2_dlnp3-eq3_dlnp2=0")

#Homo and sym: H0: the equations are homogeneous AND symmetric:

reshomsym <- c("eq1_dlnp1+eq1_dlnp2+eq1_dlnp3+eq1_dlnp4=0",
               "eq2_dlnp1+eq2_dlnp2+eq2_dlnp3+eq2_dlnp4=0",
               "eq3_dlnp1+eq3_dlnp2+eq3_dlnp3+eq3_dlnp4=0",
               "eq1_dlnp2-eq2_dlnp1=0",
               "eq1_dlnp3-eq3_dlnp1=0",
               "eq2_dlnp3-eq3_dlnp2=0")
