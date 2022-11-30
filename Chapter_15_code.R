#' Code for POE5 Chapter 15
rm(list = ls())

suppressPackageStartupMessages(require(pacman))
p_load(xtable,knitr,broom,dplyr,mosaic,AER,lmtest,car,plm)


# -----------------------------------------------------------------------------
#' Example 15.1
#' This example lists observations on several variables in a microeconometric panel of individuals.
#' The nls_panel.rdata dataset includes a subset of National Longitudinal Survey which
#' is conducted by the U.S. Department of Labor. The database includes observations on women, who
#' in 1968, were between the ages of 14 and 24. It then follows them through time, recording various
#' aspects of their lives annually until 1973 and bi-annually afterwards. Here, we use a sub-sample of
#'  N=716 women who were interviewed in 1982, 1983, 1985, 1987 and 1988. The sample consists of women 
#'  who were employed, and whose schooling was completed, when interviewed. The panel is balanced and
#'   there are 3580 total observations.

#' Data definition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/nls_panel.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/nls_panel.rdata"))
head(nls_panel,10)
View(nls_panel)

str(nls_panel)
glimpse(nls_panel)

#' Create a panel data frame using plm panel data package
nlsPDF <- pdata.frame(nls_panel, index=c("id", "year"))
pdim(nlsPDF)

View(nlsPDF)

str(nlsPDF)
glimpse(nlsPDF) # note pseries

#' Table 15.1
nlsPDF %>% dplyr::filter(as.integer(id) <= 3) %>% 
select(id, year, lwage, educ, south, black, union, exper, tenure)

# add horizontal and vertical lines to the table 
nlsPDF %>% dplyr::filter(as.integer(id) <= 3) %>% 
  select(id, year, lwage, educ, south, black, union, exper, tenure) %>% 
  xtable(.) %>%  kable(., digits=4, align="c")

# -----------------------------------------------------------------------------

#' Estimation of models using panel data is relatively straightforward,
#' though there are many variants one can consider.
#' In fact, entire courses are devoted to the many possibilities. In this chapter,
#' a few estimators are discussed and data from two sets used in estimation and testing.

#' Example 15.2 The Difference Estimator
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/chemical2.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/chemical2.rdata"))

head(chemical2,10)
View(chemical2)

chemPDF <- pdata.frame(chemical2, index=c("firm", "year"))
pdim(chemPDF)

View(chemPDF)

#' The first set of models is based on the presence of fixed effects in the regression function as
#' shown in equation (15.9). When there are only two time periods, the data can be time-differenced
#' and OLS used to estimate the slopes of all time-varying regressors. Time-invariant variables and
#' the intercept drop out of the model upon differencing.

#' This is illustrated in Example 15.2 in POE5. The data are included in the chemical2.rdata dataset.
#' This dataset contains sales, capital, and labor inputs for Chinese chemical firms. There are 3 years
#' of observations on 200 firms. The model to be estimated as a log-log model of sales:
chemPDF %>% filter(year %in% c(2005:2006)) %>% do(tidy(lm(lsales~lcapital+llabor, data = .)))
View(chemPDF)

#' Create variables, use only 2005 and 2006 data:
chemPDF <- pdata.frame(filter(chemical2, year %in% c(2005:2006)), index=c("firm", "year"))
View(chemPDF)

#' Taking the time difference yields:
chemPDF$dlnsales <- diff(chemPDF$lsales)
chemPDF$dlncapital <- diff(chemPDF$lcapital)
chemPDF$dlnlabor <- diff(chemPDF$llabor)
#' diff() and plm::lag() does not work in pipe on panel data
# chemPDF %>% mutate(dlsales=diff(lsales)) %>% select(year,firm,lsales,dlsales) %>% head(.,10)
# chemPDF %>% mutate(lsales_1=dplyr::lag(lsales)) %>% select(year,firm,lsales,lsales_1) %>% head(.,10)
# chemPDF %>% mutate(lsales_1=plm::lag(lsales)) %>% select(year,firm,lsales,lsales_1) %>% head(.,10)

#' Equation 2, in first diff of logs, no intercept
chemPDF %>% do(tidy(lm(dlnsales~0+dlncapital+dlnlabor, data = .)))
chemPDF %>% do(glance(lm(dlnsales~0+dlncapital+dlnlabor, data = .)))

# -----------------------------------------------------------------------------

#' Example 15.3 The Difference Estimator, wage equation
#' The difference estimator is used to estimate a simple wage regression based on the nls_panel data. 
#' Note that the nls_panel2 data does not exist in POE5.
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/nls_panel.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/nls_panel.rdata"))

#' We have to make a panel, using the last two years:
nls_panel %>% filter(year %in% c(87,88)) -> nls_panel2

#' Create a panel data frame using plm panel data package
nls2PDF <- pdata.frame(nls_panel2, index=c("id", "year"))
pdim(nls2PDF)

# Create variables
nls2PDF$dlnwage <- diff(nls2PDF$lwage)
nls2PDF$lnexper <- nls2PDF$exper-lag(nls2PDF$exper)

summary(lm(dlnwage~0+lnexper, data = nls2PDF))

# -----------------------------------------------------------------------------

#' Example 15.4, The within estimator.
#' In this example the within transformation is used to estimate a two-period fixed effects model
#' of Chinese chemical firms.
#' Remember (in log first diff):
summary(lm(dlnsales~0+dlncapital+dlnlabor, data = chemPDF))

#' Note model now in logs, not in log diff: --------notice "within"
example15_4 <- plm(lsales~lcapital+llabor, data = chemPDF, model = "within")
summary(example15_4) 
#' Same parameters as above, se equal to the difference estimator.

summary(fixef(example15_4)) # The 200 intercepts per firm (id)

# -----------------------------------------------------------------------------

#' Example 15.5 T=3
#' In this example, the within transformation is used on the sample with T = 3 years worth of data.
chemPDF <- pdata.frame(chemical2, index=c("firm", "year"))

example15_5 <- plm(lsales~lcapital+llabor, data = chemPDF, model = "within")
summary(example15_5)
summary(fixef(example15_5))

#' An equivalent way to estimate this model is using the least squares dummy variable
#' estimator (LSDV). Here, an indicator variable is created for each individual in
#' the sample. These are added to the model (dropping the intercept) and estimated by least squares.
#' There is a good reason why this formulation of the fixed effects model is not used more often.
#' It produces a ton of output. Since there are 200 firms in the data, 200 lines of extra output will be
#' sent to the screen (or table).

lm(lsales~0+factor(firm)+ lcapital+llabor, data = chemPDF)

# -----------------------------------------------------------------------------

#' Testing for unobserved heterogeneity, i.e., what is better, one intercept (for all)
#' or an intercept per id?
#' Example 15.6
pdim(chemPDF)
#' OLS pooled, one intercept, the restricted model
mod15_6OLS <- lm(lsales~lcapital+llabor, data = chemPDF)
summary(mod15_6OLS)
#' Fixed effects, one intercept per id, the unrestricted model
mod15_6FE <- plm(lsales~lcapital+llabor, data = chemPDF, model = "within")
mean(fixef(mod15_6FE))

#' Do the F-test (equation 15.20)
glance(mod15_6OLS) # SSER
glance(mod15_6FE) # SSEU

SSER = glance(mod15_6OLS)$deviance
SSEU = glance(mod15_6FE)$deviance
N = length(fixef(mod15_6FE))
NT = length(chemPDF$year)
#' F-value
((SSER-SSEU)/(N-1))/(SSEU/(NT-N-2))
#' Critical F
qf(0.99, 199, 398)
#' The hypothesis that the fixed effects are equal to 
#' one another is rejected at 1%.

# -----------------------------------------------------------------------------

#' Example 15.7 Robust standard errors
#' Robust covariances in panel data take into account the special nature of these data.
#' Specifically they account for autocorrelation within the observations on each individual and
#' they allow the variances for different individuals to vary.
#' Since panel data have both a time series and a crosssectional dimension one might expect that,
#' in general, robust estimation of the covariance matrix would require handling both
#' heteroskedasticity and autocorrelation (at the same time).

rm(list=ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/chemical3.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/chemical3.rdata"))

chem3PDF <- pdata.frame(chemical3, index=c("firm", "year"))

#' Pooled OLS
mod15_7OLS <- lm(lsales~lcapital+llabor, data = chemical3)
summary(mod15_7OLS)

#' Correction using built in function:

# vcovHC(x, method = c("arellano", "white1", "white2"),
#        type = c("HC0", "sss", "HC1", "HC2", "HC3", "HC4"),
#        cluster = c("group", "time"))

sqrt(diag(vcovHC(mod15_7OLS, method = c("white1"), type = c("HC0"), cluster = c("group"))))
sqrt(diag(vcovHC(mod15_7OLS, method = c("white2"), type = c("HC1"), cluster = c("group"))))

#' Using the fixed effects estimator
mod15_8FE <- plm(lsales~lcapital+llabor, data = chem3PDF, model = "within")
summary(mod15_8FE)
sqrt(diag(vcovHC(mod15_8FE, method = c("white1"), type = c("HC0"), cluster = c("group"))))

# -----------------------------------------------------------------------------

#' The Random Effects Estimator.
#' The random effects estimator treats the individual differences as being randomly assigned to
#' the individuals. Rather than estimate them as parameters as we did in the fixed effects model,
#' here they are incorporated into the model????Ts error, which in a panel will have a specific structure.
#'  One of the key advantages of the random effects model is that parameters
#'  on time invariant regressors can be estimated.
#'  The parameter estimates are actually obtained through feasible generalized least squares.
#'  The transformation that is used on the variables of the model is sometimes referred to as quasi-demeaning. 
#'  The transformation parameter used is called theta first in the plm output.

#' For the 1000 Chinese chemical firms the random effects are estimated as:
mod15_9RE <- plm(lsales~lcapital+llabor, data = chem3PDF, model = "random")
summary(mod15_9RE)
#' The estimated value of α in the random effects estimator is 0.7353. plm calls this theta.

# -----------------------------------------------------------------------------

#' A Pooled Wage Model
rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/nls_panel.rdata"))
head(nls_panel,10)

#' Create a panel data frame using plm panel data package
nlsPDF <- pdata.frame(nls_panel, index=c("id", "year"))
pdim(nlsPDF)

# -------------------
#' Note: This model in not considered in the POE5 text
wage.pooled <- plm(lwage~educ+exper+I(exper^2)+tenure+I(tenure^2)+black+south+union,
                   model="pooling", data=nlsPDF)

pooled.mod <- wage.pooled %>% tidy(.) # output
pooled.mod 
pooled.mod.robust <- tidy(coeftest(wage.pooled,
                                   vcov=vcovHC(wage.pooled, type="HC0", cluster="group")))

pooled.mod %>% left_join(select(pooled.mod.robust,-estimate), by = "term") %>%
  kable(. , align="c", digits=3, col.names=c("Variable","Coefficient",
                                             "Std. Error OLS","t-value OLS","p-value OLS", "Std. Error robust",
                                             "t-value robust","p-value robust"))

#' Note that a plain OLS model would give you the same estimates as plm.
wage.ols <- lm(lwage~educ+exper+I(exper^2)+tenure+I(tenure^2)+black+south+union,
               data=nlsPDF)
pooled.mod.ols <- wage.ols %>% tidy(.)
cbind(pooled.mod$estimate,pooled.mod.ols$estimate)
all.equal(pooled.mod$estimate,pooled.mod.ols$estimate)
# -------------------

#' The Fixed Effects Model

#' Dummy Variable estimator for small N, using OLS
wage.fixed.ols <- lm(lwage~exper+I(exper^2)+tenure+I(tenure^2)+south+union+factor(id)-1,
                     data=nlsPDF)
wage.fixed.plm <- plm(lwage~exper+I(exper^2)+tenure+I(tenure^2)+south+union+factor(id)-1,
                      data=nlsPDF, model = "pooling")

kable(tidy(wage.fixed.ols), digits=4)
kable(tidy(wage.fixed.plm), digits=4)

all.equal(wage.fixed.ols$estimate,wage.fixed.plm$estimate)

# -------------------
#' Note: This model in not considered in the POE5 text

#' Alternative restricted model, with only one intercept
wage.fixed.pooled <- plm(lwage~exper+I(exper^2)+tenure+I(tenure^2)+south+union,
                         data=nlsPDF, model = "pooling")
kable(tidy(wage.fixed.pooled), digits=4)

#' Test of poolability
pFtest(wage.fixed.plm,wage.fixed.pooled)

# -------------------

#' Table 15.7
wage.within <- plm(lwage~exper+I(exper^2)+tenure+I(tenure^2)+south+union,
                   data=nlsPDF, model = "within")

wage.within %>% tidy() %>% kable(., digits=5)

#' Histogram of all 716 intercepts
plm(lwage~exper+I(exper^2)+tenure+I(tenure^2)+south+union,
    data=nlsPDF, model = "within") %>% fixef() %>% 
  histogram(., width=0.1)

#' The Random Effects Model
#' Now we can also include the time invariant variables
wage.random <- plm(lwage~educ+exper+I(exper^2)+tenure+I(tenure^2)+black+south+union,
                   data=nlsPDF, random.method="swar",model="random")
kable(tidy(wage.random), digits=4)


# -----------------------------------------------------------------------------

rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/chemical3.rdata"))
chem3PDF <- pdata.frame(chemical3, index=c("firm", "year"))

#' Using the fixed effects estimator
mod15_8FE <- plm(lsales~lcapital+llabor, data = chem3PDF, model = "within")
#' The random effects are estimated as:
mod15_9RE <- plm(lsales~lcapital+llabor, data = chem3PDF, model = "random")

#' Hausman test for endogeneity, example 15.12
phtest(mod15_8FE,mod15_9RE)

#' If the random individual effects are correlated with regressors,
#' then the random effects estimator will not be consistent.
#' A statistical test of this proposition should be done whenever this estimator
#' is used in order to reduce the chance of model misspecification.

# -----------------------------------------------------------------------------
