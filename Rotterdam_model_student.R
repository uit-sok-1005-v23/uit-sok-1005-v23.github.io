
# R code to estimate Rotterdam model
# (We are going to use the data posted on canvas (sok3008.csv, you can find it under your file folder)


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
head(sok3008)

#Quick investigation of the data with summary and plots QCod, etc
summary(mydata$QCod); summary(mydata$QHaddock); summary(mydata$QAlaska)
plot(mydata$QCod, type='l')
plot(mydata$QHaddock, type='l')
plot(mydata$QAlaska, type='l')


#add monthly dummies to the data.frame (use ifelse(mydata$Month==i, 1, 0))
#use variable name "mi", e.g. for January, we get "m1"
mydata$m1 <- ifelse(mydata$Month==1, 1, 0)
mydata$m2 <- ifelse(mydata$Month==2, 1, 0)
#etc...

head(mydata)

#We'll now define the variables. 
#Use 1=cod, 2=haddock, 3=Alaska such that given pi, then p1 is the price of cod,
#p2 is the price of haddock etc. lnp1 is log price for cod. R1 is budget share for cod etc.

#calculate prices, "pi", and add it to the data.frame





# define log prices "lnpi". PS. in R, log() is by default the natural logarithm. 





# define log quantity, "lnqi"





#Define TotEXP (XCod+XHaddock+XAlaska)





#Define Budget shares "Ri"





#Define diff log prices "dlnpi"






#define diff log quantity "dlnqi"





#define two-period moving average shares (mean share), "Si"





#Define the dependent variables, i.e. weighted change in the quantity consumed, "Sidlnqi"





#Define the Divisa Volume Index, "dlnEXP" (i.e. sum of dependent variables)





head(mydata)
#Remove first row
mydata2 <- mydata[-1,]
head(mydata2)

summary (mydata$p1);summary (mydata$p2); summary (mydata$p3)

plot (mydata$p1, type='l')
plot (mydata$p2, type='l')
plot (mydata$p3, type='l')

summary (mydata$R1);summary (mydata$R2);summary (mydata$R3)



#estimate the model 

#' storing the means of the budget shares for later because we know we're going to use these 
#' to calculate elasticities. Name the variables "ms1", "ms2" etc


ms1+ms2+ms3 #should sum to 1
#' Definining our demand system in single equation firsts:
#system without monthly dummies





#system with monthly dummies






#' note: whether to estimated model with dummies or without dummies depends on your study
#' and your data

#' First we will define the model without homeogeneity and symmetry restriction.
#' This will be our kind of "benchmark" model

#' First define the system using list() on the eqs. We must omit equation 3 because of singularity



#' Then estimate the model using systemfit(system12, data, method="SUR").
#' Then, check summary.
 





#' We will define our null hypothesis for join hypothesis tests as follows:
#' 
#' The null hypothesis is here: H0: The monthly dummies are jointly equal to zero:

resmonth <- c("eq1_m2=0", "eq1_m3=0", "eq1_m4=0","eq1_m5=0","eq1_m6=0","eq1_m7=0","eq1_m8=0",
              "eq1_m9=0", "eq1_m10=0","eq1_m11=0","eq1_m12=0", 
              "eq2_m2=0", "eq2_m3=0", "eq2_m4=0","eq2_m5=0","eq2_m6=0","eq2_m7=0","eq2_m8=0",
              "eq2_m9=0", "eq2_m10=0","eq2_m11=0","eq2_m12=0")     

#' The wald test which computes the chi square (use car::linearHypothesis(, test="Chisq"):




#' Find the critical chi squared - remember to input correct DF 
#' (22 for this one. Can be seen in the output above):










#' Here we will define our theoretical restrictions in the same way as we did with our
#' monthly H0's (think about the substitution matrix):

#homogeneity H0: The equations are homogeneous




# symmetry: H0: The equations are symmetric





#Homo and sym: H0: the equations are homogeneous AND symmetric:






#test homogeneity






#test symmetry





#test homogeity and symmetry together







#We can now define models with either single or both restrictions imposed:

#Rotterdam model with homogeneity





#Rotterdam model with symmetry





#Rotterdam model with both homogeneity and symmetry 






#' We will now estimate our elasticities. We continue with the fully restricted model, Rot12hs.
#' First we need to extract the coefficients from our model:


#' Easy way for Elasticities based on model with both homogeneity and symmetry imposed
#' The coef() function will extract the coefficients from a summary table and their 
#' corresponding errors and t and p-values. You can also use Rot12hs$coefficients
#' to just extract the coefficients
#' 




#define first a vector which contains all relevant varaibles to make code easier to read:






#' Here we're making a vector repeating the means to use to find elasticities





#calculate elasticites eij=THETAij/msi






#print elasticities in a table




# Use the Slutsky eqtn to find the Marshallian elasticities eij=eij*-RjAi
#e11





#remember, we keep the income elasticity. This does not change. Construct final matrix with expenditure elas.






##########estimate model with equation 1 and 3 to get results for equation 3#####################################

#define the system for eq1 and eq3 and its restrictions



#estimate:

#model with homogeneity




#model with symetry




#model with both




##################################################################
########### Test adding up example
##################################################################

#From the homogeneity restriction we have that sum_j(theta_ij)=0, consequently, 
#for 3 goods: theta31=-theta11-theta21
#





#From engel aggregation, we have that sum_i(mu_i)=1, then for 3 goods
#mu3=1-mu1-mu2





#The adding up of the intercept, sum_i(alpha_i)=0, i.e.
#alpha3=-alpha1-alpha2





#Cournot aggregation, sum_i(theta_ij)=0






######################################################################################################

#extract coefficients for eq 1 and 3

#Just extract relevant information. Remember to adjust this if you have 4 goods or other included variables:
#define first a vector which contains all relevant varaibles to make code easier to read:

#' eventhough we're now working with the third equation in our demand system, we still
#' have to use the "eq2" notation since we still only have estimated two equations.
#' I.e. R will still lable equation 3 as equation 2 in its output:
#' 





#Calculate elasticites eij=THETAij/msi




#print elasticities in a table




#define mean for all coefficients, including seasonal dummies:









######################################################################################################
######################################################################################################
######################################################################################################
#' we can now create the final Hicksian elasticity matrix:




#Our full elasticity matrix is then:




#Often, you will see results presented in its transposed form, e.g. Paper (1) Kinnuckan. 
#We can use the function t() to find the transpose of a matrix in R:




#You decide how you want to present it in your paper
######################################################################################################
######################################################################################################
######################################################################################################


# Use the Slutsky eqtn to find the Marshallian elasticities eij=eij*-RjAi
#e11







#marshallian elasticity matrix:





#####################################
#####################################
#####################################
#Restrictions for 4 equations:
#####################################
#####################################
#####################################



#homogeneity H0: The equations are homogeneous





# symmetry: H0: The equations are symmetric





#Homo and sym: H0: the equations are homogeneous AND symmetric:





