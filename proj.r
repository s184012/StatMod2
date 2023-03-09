### Load libraries
rm(list=ls())
library( GGally)
library(car)
library(caret)
library(nlme)

### Load data and check data types
dat = read.csv("dioxin.csv", header = TRUE, sep = ",")
print(sapply(dat, typeof))

# Convert categorical variables to factor variables
dat <- as.data.frame(unclass(dat), stringsAsFactors = TRUE)
dat$TIME = as.factor(dat$TIME)

# Fill in the two NA PRSEK values
dat$PRSEK[15:16] = c("L","L")

# Remove remaining observations with NA values and check that
# data has been altered as expected
print(dim(dat))
print(sum(is.na(dat)))
dat <- na.omit(dat)
print(dim(dat))
print(sum(is.na(dat)))
print(sapply(dat, typeof))
attach(dat)

### Deprecated KNN fill-in of NA values
# m = colMeans(data[c(1:2,9:21)], na.rm = T)
# std = sapply(data[,c(1:2,9:21)],sd, na.rm = T)
# preproc = preProcess(data, method = "knnImpute")
# dat = predict(preproc, data)
# dat$PRSEK[15:16] = c("L","L")
# sum(is.na(dat))
# m = unname(m)
# std = unname(std)
# rep.row<-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }
# m = rep.row(m, 57)
# std = rep.row(std, 57)
# dat[,c(1:2,9:21)] = dat[,c(1:2,9:21)]*std + m


### EXERCISE 1 (Not complete in this script)
# Plot predictors and response
ggpairs(data = dat, columns = c(2,9:21), title ="Relationships between predictors & response",
       lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))


### EXERCISE 2
# Fit initial model
mod_1 = lm(DIOX ~ PLANT + TIME + LAB + LOAD + OXYGEN + PRSEK, data = dat)
summary(mod_1)

# Issues with NA parameter estimates due to singularities ->
# Investigate colinearity with the alias function
alias(mod_1)

# Colinearity between LOADN and OXYGENN/PRSEKN.
# One possible reason: Observations with identical explanatory variables but
#                      different response values due to the limited amount of
#                      variables considered
# Solution: Remove observations with identical explanatory variables
# (Reconsider if this is necessary or even "correct" to do)
dat_2 <- dat[c("DIOX", "PLANT", "TIME", "LAB", "LOAD", "OXYGEN", "PRSEK")]
dat_2 <- dat_2[!duplicated(dat_2[c("PLANT", "TIME", "LAB", "LOAD", "OXYGEN", "PRSEK")]), ]
mod_2 <- lm(DIOX ~ PLANT + TIME + LAB + LOAD + OXYGEN + PRSEK, data = dat_2)
summary(mod_2)
alias(mod_2)

# Issue is still present because LOAD, PRSEK and OXYGEN are always N at
# the same time - meaning they are perfectly colinear.
# Solution: Ignore it as the LOADN variable takes care of it?

corpar(mfrow=c(2,2))
plot(mod_1)
#bad model, studentized residuals are monotone
#based on the plots, we try a log-transform on DIOX
mod_1_tr = lm(log(DIOX) ~ PLANT + TIME + LAB + LOAD + OXYGEN + PRSEK, data = dat)
summary(mod_1_tr)
par(mfrow=c(2,2))
plot(mod_1_tr)


anova(mod_1_tr) ## Type I
Anova(mod_1_tr,type="II")
drop1(mod_1_tr,test="F")
fit1 <- update(mod_1_tr,.~.-PRSEK)
drop1(fit1,test="F")
par(mfrow=c(2,2))
plot(fit1)

anova(mod_1_tr, fit1) #same result that we got in model reduction

confint(fit1)

save(fit1, file = 'model_ex2.rds')
#### END OF 2


## START OF 3



mod_1rev = lm(log(DIOX) ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT, data = dat)
summary(mod_1rev)
par(mfrow=c(2,2))
plot(mod_1rev)
anova(mod_1rev) ## Type I
Anova(mod_1rev,type="II")
drop1(mod_1rev,test="F")
mod_2rev <- update(mod_1rev,.~.-QRAT)
drop1(mod_2rev,test="F")
summary(mod_2rev)
par(mfrow=c(2,2))
plot(mod_2rev)
save(mod_2rev, file="model_ex3.rds")

mod_2rev
### END OF 3

### START OF 4
attach(dat)
new_data = data.frame(PLANT = factor("RENO_N", levels = levels(PLANT)), TIME = factor("1", levels = levels(TIME)), LAB = factor("KK", levels = levels(LAB)),O2COR =c(0.5), NEFFEKT = c(-0.01))
diox_pred = predict(mod_2rev, newdata = new_data, interval = "confidence", se = TRUE)
diox_pred$fit
#should we talk about that (DIFFERENCE WITH AKAIKE)
### END OF 4

### START OF 5
# REDUCE NEFFEKT

### END OF 5


### START OF 6

#CONSIDERATIONS
#ANOVA FOR FUN

### END OF 6

### START OF 7




##
dev.new()
#Complete model

mod = lm(log(DIOX) ~ PLANT + TIME + LAB  #BLOCKS
         + O2COR + NEFFEKT + QRAT #ACTIVE
         + QROEG + TOVN + TROEG + POVN + CO2 + CO + SO2 + HCL + H2O #PASSIVE
         + PLANT:O2COR #INTERACTION PLANT O2
         + O2COR:CO2 + O2COR:H2O #INTERACTION O2 PASSIVE
         + TROEG:QROEG + CO2:H2O + QROEG:TOVN #INTERATION PASSIVE
         , data = dat)
summary(mod)

saveFig<- TRUE
if(saveFig == TRUE){pdf("MODEL7_UNREDUCED.pdf", width = 10*0.8, height = 10*0.8)}
par(mfrow=c(2,2))
plot(mod)
if(saveFig == TRUE){dev.off()}
Anova(mod,type="II")
drop1(mod,test="F")
mod2 <- update(mod,.~.-POVN)
drop1(mod2,test="F")
mod3 <- update(mod2,.~.-QRAT)
drop1(mod3,test="F")
mod4 <- update(mod3,.~.-CO)
drop1(mod4,test="F")
mod5 <- update(mod4,.~.-PLANT:O2COR)
drop1(mod5,test="F")
mod6 <- update(mod5,.~.-QROEG:TOVN)
drop1(mod6,test="F")
mod7 <- update(mod6,.~.-QROEG:TROEG)
drop1(mod7,test="F")
mod8 <- update(mod7,.~.-QROEG)
drop1(mod8,test="F")
mod9 <- update(mod8,.~.-TROEG)
drop1(mod9,test="F")
mod10 <- update(mod9,.~.-TOVN)
drop1(mod10,test="F")
mod11 <- update(mod10,.~.-O2COR:H2O)
drop1(mod11,test="F")
mod12 <- update(mod11,.~.-O2COR:CO2)
drop1(mod12,test="F")
mod13 <- update(mod12,.~.-CO2:H2O)
drop1(mod13,test="F")
mod14 <- update(mod13,.~.-H2O)
drop1(mod14,test="F")
mod15 <- update(mod14,.~.-CO2)
drop1(mod15,test="F")
summary(mod15)
if(saveFig == TRUE){pdf("MODEL7_REDUCED.pdf", width = 10*0.8, height = 10*0.8)}
par(mfrow=c(2,2))
plot(mod15)
if(saveFig == TRUE){dev.off()}
anova(mod, mod15)
hist( mod15$residuals, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
X = model.matrix( mod15)
lev = hat(X)
p = mod15$rank;
n = length(DIOX);
plot( mod13$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( mod$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

# RESIDUAL ANALYSIS
gs = summary(mod13)
res_std = mod13$residuals/sd(DIOX)
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd
plot( mod13$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( mod13$fitted.values[watchout_ids_rstd],
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( mod13$fitted.values[watchout_ids_lev],
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'),
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )
# Studentized residuals
stud = rstudent( mod13 )
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
plot( mod13$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( mod13$fitted.values[watchout_ids_stud],
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
points( mod13$fitted.values[watchout_ids_lev],
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','orange'),
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )

# Cook Distance
Cdist = cooks.distance( mod13 )
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist
if(saveFig == TRUE){pdf("Influ.pdf", width = 10*0.8, height = 10*0.8)}
par( mfrow = c( 1, 3 ) )
plot( mod13$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( mod13$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )
plot( mod13$fitted.values, stud, pch = 16, xlab = 'Fitted values',
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( mod13$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ],
        col = 'pink', pch = 16 )
plot( mod13$fitted.values, lev, pch = 16, xlab = 'Fitted values',
      ylab = 'Leverages', main = 'Leverages' )

points( mod13$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )
if(saveFig == TRUE){dev.off()}

id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gl = lm( log(DIOX) ~ PLANT + TIME + LAB + NEFFEKT +  SO2 + HCL + H2O, dat[ id_to_keep, ] )
summary( gl )
par(mfrow=c(2,2))
plot(gl)

summary(mod15)

#the improvement obtained not considering the points violating the CookDistance 
# "boundary" is not worth it. We had a good model even before.

attach(dat)

yhat = predict(mod15,dat, type = "response")

V = varIdent(form = ~1|LAB)
mo = gls(log(DIOX) ~ PLANT + TIME + LAB + O2COR + NEFFEKT + SO2 + HCL, data = dat,
    weights = V, method = "ML")
intervals(mo)
summary(mod15)
summary(mo)

l<-function(sigma){
  v = as.numeric(dat$LAB=="KK")
  v[dat$LAB=="KK"] = sigma[1]
  v[dat$LAB!="KK"] = sigma[2]
  
  -sum(dnorm(log(dat$DIOX), yhat, v, log = T))
}

nlminb(c(1,4), l, lower=c(0, 0), upper=c(Inf, Inf))
logLik(mod15)


# BONUS---> REDUCE MODEL WITH AIC/BIC
modela = step( mod, direction = "backward" , k = 2, trace = T);
summary(modela) #0.8861 r SQ
drop1(modela, test = "F")
modelb = step( mod, direction = "backward" , k = log(n), trace = T);
summary(modelb) #0.8744
drop1(modelb, test = "F")
summary(mod15) #0.8596
anova(modela, modelb, mod15) 
#our model is significantly different than the ones obtained with AIC/BIC
#when it comes to anova test (nested models)
#But R^2 is not that different


### END OF 7
