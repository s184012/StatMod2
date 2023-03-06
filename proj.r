rm(list=ls())
library( GGally)
library(car)
library(caret)
library(nlme)
data = read.csv("dioxin.csv", header = TRUE, sep = ",")
print(sapply(data, typeof))
data <- as.data.frame(unclass(data),stringsAsFactors = TRUE)
data$TIME = as.factor(data$TIME)
print(sapply(data, typeof))
m = colMeans(data[c(1:2,9:21)], na.rm = T)
std = sapply(data[,c(1:2,9:21)],sd, na.rm = T)
preproc = preProcess(data, method = "knnImpute")
dat = predict(preproc, data)
dat$PRSEK[15:16] = c("L","L")
sum(is.na(dat))
m = unname(m)
std = unname(std)
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
m = rep.row(m, 57)
std = rep.row(std, 57)
dat[,c(1:2,9:21)] = dat[,c(1:2,9:21)]*std + m
attach(dat)
ggpairs(data = dat, columns = c(2,9:21), title ="Relationships between predictors & response",
       lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))



mod_1 = lm(DIOX ~ PLANT + TIME + LAB + LOAD + OXYGEN + PRSEK, data = dat)
summary(mod_1)
par(mfrow=c(2,2))
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
mod = lm(log(DIOX) ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + QROEG + TOVN + TROEG + POVN + 
           CO2 + CO + SO2 + HCL + H2O + O2COR:CO2 + O2COR:H2O + TROEG:QROEG + CO2:H2O, data = dat)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
Anova(mod,type="II")
drop1(mod,test="F")
mod2 <- update(mod,.~.-CO)
drop1(mod2,test="F")
mod3 <- update(mod2,.~.-QROEG:TROEG)
drop1(mod3,test="F")
mod4 <- update(mod3,.~.-QROEG)
drop1(mod4,test="F")
mod5 <- update(mod4,.~.-TROEG)
drop1(mod5,test="F")
mod6 <- update(mod5,.~.-POVN)
drop1(mod6,test="F")
mod7 <- update(mod6,.~.-O2COR:H2O)
drop1(mod7,test="F")
mod8 <- update(mod7,.~.-O2COR:CO2)
drop1(mod8,test="F")
mod9 <- update(mod8,.~.-CO2:H2O)
drop1(mod9,test="F")
mod10 <- update(mod9,.~.-O2COR)
drop1(mod10,test="F")
mod11 <- update(mod10,.~.-CO2)
drop1(mod11,test="F")
mod12 <- update(mod11,.~.-QRAT)
drop1(mod12,test="F")
mod13 <- update(mod12,.~.-TOVN)
drop1(mod13,test="F")
summary(mod13)
par(mfrow=c(2,2))
plot(mod13)
shapiro.test(mod13$residuals) #data is normal
hist( mod13$residuals, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
X = model.matrix( mod13)
lev = hat(X)
p = mod13$rank;
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
id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gl = lm( log(DIOX) ~ PLANT + TIME + LAB + NEFFEKT +  SO2 + HCL + H2O, dat[ id_to_keep, ] )
summary( gl )
par(mfrow=c(2,2))
plot(gl)

#the improvement obtained not considering the points violating the CookDistance 
# "boundary" is not worth it. We had a good model even before.

t = as.numeric(dat$LAB)
w = 1/t;
modw = lm(log(DIOX) ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + QROEG + TOVN + TROEG + POVN + 
           CO2 + CO + SO2 + HCL + H2O + O2COR:CO2 + O2COR:H2O + TROEG:QROEG + CO2:H2O, data = dat, weights = w)
summary(modw)
par(mfrow=c(2,2))
plot(modw)


V1 = varIdent(c( t = 1),form = ~ 1)
modw1 = gls(log(DIOX) ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + QROEG + TOVN + TROEG + POVN + 
            CO2 + CO + SO2 + HCL + H2O + O2COR:CO2 + O2COR:H2O + TROEG:QROEG + CO2:H2O, data = dat, method = "REML", weights = V1)
summary(modw1)


# BONUS---> REDUCE MODEL WITH AIC/BIC
modela = step( mod, direction = "backward" , k = 2, trace = T);
summary(modela) #0.8861 r SQ
drop1(modela, test = "F")
modelb = step( mod, direction = "backward" , k = log(n), trace = T);
summary(modelb) #0.8744
drop1(modelb, test = "F")
summary(mod13) #0.8596
anova(modela, modelb, mod13) 
#our model is significantly different than the ones obtained with AIC/BIC
#when it comes to anova test (nested models)
#But R^2 is not that different



### END OF 7
