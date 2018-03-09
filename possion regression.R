########################################
#   GLM Models - Poisson Regression   #
########################################

# download and import the data
ceb <- read.table("http://data.princeton.edu/wws509/datasets/ceb.dat")
# plot distribution of CEB
hist(ceb$y, breaks = 50, xlab = "children ever born", main = "Distribution of CEB")


# run possion regression on the data and output the result
fit <- glm(y ~ educ + res + dur, offset = log(n), family = poisson(), data = ceb)
summary(fit)
## Call:
## glm(formula = y ~ educ + res + dur, family = poisson(), data = ceb, 
##     offset = log(n))
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.291  -0.665   0.076   0.661   3.679  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   0.0570     0.0480    1.19     0.24    
## educnone     -0.0231     0.0227   -1.02     0.31    
## educsec+     -0.3327     0.0539   -6.17  6.7e-10 ***
## educupper    -0.1247     0.0300   -4.16  3.2e-05 ***
## resSuva      -0.1512     0.0283   -5.34  9.4e-08 ***
## resurban     -0.0390     0.0246   -1.58     0.11    
## dur10-14      1.3705     0.0511   26.83  < 2e-16 ***
## dur15-19      1.6142     0.0512   31.52  < 2e-16 ***
## dur20-24      1.7855     0.0512   34.86  < 2e-16 ***
## dur25-29      1.9768     0.0500   39.50  < 2e-16 ***
## dur5-9        0.9977     0.0528   18.91  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 3731.525  on 69  degrees of freedom
## Residual deviance:   70.653  on 59  degrees of freedom
## AIC: Inf
## 
## Number of Fisher Scoring iterations: 4
exp(coef(fit))
## (Intercept)    educnone    educsec+   educupper     resSuva    resurban 
##      1.0586      0.9772      0.7170      0.8827      0.8597      0.9618 
##    dur10-14    dur15-19    dur20-24    dur25-29      dur5-9 
##      3.9374      5.0240      5.9625      7.2196      2.7119
# validate the overdispersion
require(qcc)
qcc.overdispersion.test(ceb$y, type = "poisson")                 
## Overdispersion test Obs.Var/Theor.Var Statistic p-value
##        poisson data               323     22284       0
# validate the overdispersion; these data is proven to be overdispersed. Quasipoisson is a remedy: It estimates a scale parameter as well (which is fixed for poisson models as the variance is also the mean) and will provide better fit.
fit2 <- glm(y ~ educ + res + dur, offset = log(n), family = quasipoisson(), data = ceb)
summary(fit2)
## 
## Call:
## glm(formula = y ~ educ + res + dur, family = quasipoisson(), 
##     data = ceb, offset = log(n))
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.291  -0.665   0.076   0.661   3.679  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   0.0570     0.0529    1.08  0.28596    
## educnone     -0.0231     0.0249   -0.93  0.35854    
## educsec+     -0.3327     0.0593   -5.61  5.7e-07 ***
## educupper    -0.1247     0.0330   -3.78  0.00037 ***
## resSuva      -0.1512     0.0312   -4.85  9.4e-06 ***
## resurban     -0.0390     0.0271   -1.44  0.15597    
## dur10-14      1.3705     0.0562   24.37  < 2e-16 ***
## dur15-19      1.6142     0.0564   28.64  < 2e-16 ***
## dur20-24      1.7855     0.0564   31.66  < 2e-16 ***
## dur25-29      1.9768     0.0551   35.88  < 2e-16 ***
## dur5-9        0.9977     0.0581   17.18  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 1.212)
## 
##     Null deviance: 3731.525  on 69  degrees of freedom
## Residual deviance:   70.653  on 59  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 4
# Interruption: With the increase in the marriage duration, the number of children will grow accordingly; the higher the educational level, the lower the number of expectant children; the rural population is expected to have more children than the city.

