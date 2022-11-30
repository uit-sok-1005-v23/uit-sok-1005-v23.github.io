

# SUR Estimation

rm(list=ls())
require(systemfit) || {install.packages("systemfit");require(systemfit)}

# Example, eq. 11.2.30

table.11.1 <- tibble::tribble(
  ~y1,   ~x12,  ~x13,      ~y2,   ~x22,  ~x23,
  40.05292, 1170.6,  97.8,  2.52813,  191.5,   1.8,
  54.64859, 2015.8, 104.4, 24.91888,    516,   0.8,
  40.31206, 2803.3,   118,  29.3427,    729,   7.4,
  84.21099, 2039.7, 156.2, 27.61823,  560.4,  18.1,
  127.5724, 2256.2, 172.6, 60.35945,  519.9,  23.5,
  124.8797, 2132.2, 186.6, 50.61588,  628.5,  26.5,
  96.55514, 1834.1, 220.9, 30.70955,  537.1,  36.2,
  131.1601,   1588, 287.8, 60.69605,  561.2,  60.8,
  77.02764, 1749.4, 319.9, 30.00972,  617.2,  84.4,
  46.96689, 1687.2, 321.3,  42.5075,  626.7,  91.2,
  100.6597, 2007.7, 319.6, 58.61146,  737.2,  92.4,
  115.7467, 2208.3,   346, 46.96287,  760.5,    86,
  114.5826, 1656.7, 456.4, 57.87651,  581.4, 111.1,
  119.8762, 1604.4, 543.4, 43.22093,  662.3, 130.6,
  105.5699, 1431.8, 618.3, 22.87143,  583.8, 141.8,
  148.4266, 1610.5, 647.4, 52.94754,  635.2, 136.7,
  194.3622, 1819.4, 671.3,  71.2303,  723.8, 129.7,
  158.2037, 2079.7, 726.1,  61.7255,  864.1, 145.5,
  163.093, 2371.6, 800.3, 85.13053, 1193.5, 174.8,
  227.5634, 2759.9, 888.9, 88.27518, 1188.9, 213.5
)

head(table.11.1)

# Equation by equation OLS
fit1 <- lm(y1~x12+x13, data = table.11.1)
fit2 <- lm(y2~x22+x23, data = table.11.1)

# Eq. 11.2.31
fit1 ; fit2


# Testing for Contemporaneous Correlation
x <- matrix(resid(fit1),resid(fit2), nrow=length(table.11.1$y1), ncol = 2)
cor(x)

# H0: s12 = 0, H1: s12 != 0
# LM-test, equation 11.2.34 
length(table.11.1$y1)*cor(x)[1,2]^2 > qchisq(0.95, df=1, ncp = 0, lower.tail = TRUE, log.p = FALSE)

?qchisq

# Accept H0, we donot have cont. corr. Hence, we can estimate using OLS or SUR

#' Estimated as a system
eq1 <- y1~x12+x13
eq2 <- y2~x22+x23

eqSystem <- list(eq1 = eq1, eq2 = eq2)

# Estimated equation by equation OLS
fitols <- systemfit(eqSystem, method = "OLS", data = table.11.1)
print(fitols)
coef(summary(fitols))

# Estimated equation by equation SUR
fitsur <- systemfit(eqSystem, method = "SUR", data = table.11.1)
print(fitsur)
coef(summary(fitsur))
summary(fitsur)

round(coef(fitols)-coef(fitsur),4)

# ------------------------------------------------------------

# A further example
rm(list=ls())

table.11.3 <- tibble::tribble(
  ~p1,    ~p2,    ~p3,      ~y,    ~q1,    ~q2,     ~q3,
  10.763,  4.474,  6.629, 487.648, 11.632, 13.194,   45.77,
  13.033, 10.836, 13.774, 364.877, 12.029,  2.181,  13.393,
  9.244,  5.856,  4.063, 541.037,  8.916,  5.586, 104.819,
  4.605,  14.01,  3.868, 760.343, 33.908,  5.231, 137.269,
  13.045, 11.417, 14.922, 421.746,  4.561,  10.93,  15.914,
  7.706,  8.755, 14.318, 578.214, 17.594, 11.854,  23.667,
  7.405,  7.317,  4.794, 561.734, 18.842, 17.045,  62.057,
  7.519,   6.36,  3.768,  301.47, 11.637,  2.682,  52.262,
  8.764,  4.188,  8.089, 379.636,  7.645, 13.008,  31.916,
  13.511,  1.996,  2.708, 478.855,  7.881, 19.623, 123.026,
  4.943,  7.268, 12.901, 433.741,  9.614,  6.534,  26.255,
  8.36,  5.839, 11.115, 525.702,  9.067,  9.397,   35.54,
  5.721,   5.16,  11.22, 513.067,  14.07, 13.188,  32.487,
  7.225,  9.145,   5.81, 408.666, 15.474,   3.34,  45.838,
  6.617,  5.034,  5.516, 192.061,  3.041,  4.716,  26.867,
  14.219,  5.926,  3.707, 462.621, 14.096, 17.141,  43.325,
  6.769,  8.187, 10.125, 312.659,  4.118,  4.695,   24.33,
  7.769,  7.193,  2.471, 400.848, 10.489,  7.639, 107.017,
  9.804, 13.315,  8.976, 392.215,  6.231,  9.089,  23.407,
  11.063,  6.874, 12.883, 377.724,  6.458, 10.346,  18.254,
  6.535, 15.533,  4.115, 343.552,  8.736,  3.901,  54.895,
  11.063,  4.477,  4.962, 301.599,  5.158,   4.35,   45.36,
  4.016,  9.231,  6.294, 294.112, 16.618,  7.371,  25.318,
  4.759,  5.907,  8.298, 365.032, 11.342,  6.507,  32.852,
  5.483,  7.077,  9.638, 256.125,  2.903,   3.77,  22.154,
  7.89,  9.942,  7.122, 184.798,  3.138,   1.36,  20.575,
  8.46,  7.043,  4.157, 359.084, 15.315,  6.497,  44.205,
  6.195,  4.142,  10.04, 629.378,  22.24, 10.963,  44.443,
  6.743,  3.369, 15.459, 306.527, 10.012,  10.14,  13.251,
  11.977,  4.806,  6.172, 347.488,  3.982,  8.637,  41.845
)

# Equation by equation OLS
fit1 <- lm(log(q1)~log(p1)+log(y), data = table.11.3)
fit2 <- lm(log(q2)~log(p2)+log(y), data = table.11.3)
fit3 <- lm(log(q3)~log(p3)+log(y), data = table.11.3)

# Eq. 11.2.31
fit1 ; fit2 ; fit3

# Testing for Contemporaneous Correlation
x <- matrix(c(resid(fit1),resid(fit2), resid(fit3)), nrow=length(table.11.3$p1), ncol = 3) # nrow =30
cor(x)

cor(x)[2,1]^2
cor(x)[3,1]^2
cor(x)[3,2]^2

# H0: s12 = s13 = s23 = 0
# LM-test, equation 11.2.34 

30*(cor(x)[2,1]^2+cor(x)[3,1]^2+cor(x)[3,2]^2) > qchisq(0.95, df=3, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# Reject H0, we have cont. corr

#' Estimated as a system
eq1 <- log(q1)~log(p1)+log(y)
eq2 <- log(q2)~log(p2)+log(y)
eq3 <- log(q3)~log(p3)+log(y)

eqSystem <- list(eq1 = eq1, eq2 = eq2, eq3=eq3)

fitols <- systemfit(eqSystem, method = "OLS", data = table.11.3)
print(fitols)
coef(summary(fitols))

fitsur <- systemfit(eqSystem, method = "SUR", data = table.11.3)
print(fitsur)
coef(summary(fitsur))
summary(fitsur)

#' Create restriction matrix to impose restrictions;
#' b11 = b22 = b33, or b11 - b22 = 0 & b11 - b33 = 0
#' same price effect in all 3 equations

?systemfit

#' Create a a zero matrix R of size, (# of restrictions) X (# of parameters in the model) = 2X9 matrix
R1 <- matrix(0, nrow = 2, ncol = 9)
R1[1, 2] <- 1
R1[1, 5] <- -1
R1[2, 2] <- 1
R1[2, 8] <- -1
R1


# Constrained SUR estimation
fitsur.r <- systemfit(eqSystem, "SUR", data = table.11.3, restrict.matrix = R1 )
summary(fitsur.r)

# Constrained OLS estimation
fitsur.r.ols <- systemfit(eqSystem, "OLS", data = table.11.3, restrict.matrix = R1 )
fitsur.r.ols

#' perform LR-test (The likelihood-ratio statistic) 

?lrtest.systemfit

lrtest(fitsur.r, fitsur) # Keep H0
qchisq(0.95, df=2, ncp = 0, lower.tail = TRUE, log.p = FALSE)

# the same hypothesis in symbolic form,
# but tested on the unrestricted model
joint.hyp <- c("eq1_log(p1) - eq2_log(p2) = 0", "eq1_log(p1) - eq3_log(p3) = 0")

## perform Theil's F test
linearHypothesis(fitsur, R1) 
linearHypothesis(fitsur, joint.hyp)

## perform Wald test with F statistic
linearHypothesis(fitsur, R1, test = "F" ) 
linearHypothesis(fitsur, joint.hyp )

## perform Wald-test with chi^2 statistic
linearHypothesis(fitsur, R1, test = "Chisq" ) 
linearHypothesis(fitsur, joint.hyp, test = "Chisq" )

# ------------------------------------------------------------

#' Estimated as a system, with all price variables in each equation

eq1 <- log(q1)~log(p1)+log(p2)+log(p3)+log(y)
eq2 <- log(q2)~log(p1)+log(p2)+log(p3)+log(y)
eq3 <- log(q3)~log(p1)+log(p2)+log(p3)+log(y)

eqSystem <- list(eq1 = eq1, eq2 = eq2, eq3=eq3)

fitsur <- systemfit(eqSystem, method = "SUR", data = table.11.3)
print(fitsur)
coef(summary(fitsur))
summary(fitsur)

#' Create restriction matrix to impose symmetry restrictions;
#' b12 = b21 ; b13 = b31 ; b23 = b32
R2 <- matrix(0, nrow = 3, ncol = 15)
R2
R2[1, 3] <- 1
R2[1, 7] <- -1
R2[2, 4] <- 1
R2[2, 12] <- -1
R2[3, 9] <- 1
R2[3, 13] <- -1
R2

fitsur.symmetry <- systemfit(eqSystem, method="SUR", data = table.11.3, restrict.matrix = R2)
summary(fitsur.symmetry)

R2.alt <- c("eq1_log(p2) - eq2_log(p1)=0",
            "eq1_log(p3) - eq3_log(p1)=0",
            "eq2_log(p3) - eq3_log(p2)=0")

fitsur.symmetry.alt <- systemfit(eqSystem, method="SUR", data = table.11.3, restrict.matrix = R2.alt)
summary(fitsur.symmetry.alt)

coef(fitsur.symmetry)-coef(fitsur.symmetry.alt)

# Homogeneity
R3 <- c("eq1_log(p1) + eq1_log(p2) + eq1_log(p3)=0",
        "eq2_log(p1) + eq2_log(p2) + eq2_log(p3)=0",
        "eq3_log(p1) + eq3_log(p2) + eq3_log(p3)=0")

fitsur.homogeneity <- systemfit(eqSystem, method="SUR", data = table.11.3, restrict.matrix = R3)
summary(fitsur.homogeneity)

# Homogeneity & Symmetry
R4 <- c("eq1_log(p1) + eq1_log(p2) + eq1_log(p3)=0",
        "eq2_log(p1) + eq2_log(p2) + eq2_log(p3)=0",
        "eq3_log(p1) + eq3_log(p2) + eq3_log(p3)=0",
        "eq1_log(p2) - eq2_log(p1)=0",
        "eq1_log(p3) - eq3_log(p1)=0",
        "eq2_log(p3) - eq3_log(p2)=0")

fitsur.homogeneity.symmetry <- systemfit(eqSystem, method="SUR", data = table.11.3, restrict.matrix = R4)
summary(fitsur.homogeneity.symmetry)


