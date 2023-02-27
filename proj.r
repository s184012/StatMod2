rm(list=ls())
library( GGally)
library(car)
library(caret)
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
mod_1_tr = lm(log(DIOX) ~ PLANT + TIME + LAB + LOAD + OXYGEN + PRSEK)
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

new_data = data.frame(PLANT = factor("RENO_N", levels = levels(PLANT)), TIME = factor("1", levels = levels(TIME)), LAB = factor("KK", levels = levels(LAB)),O2COR =C (0.5), NEFFEKT = c(-0.01))
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
mod = lm(DIOX ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + LOAD + OXYGEN + PRSEK + QROEG + TOVN + TROEG + POVN + CO2 + CO + SO2 + HCL + H2O, data = dat)
summary(mod)
par(mar = c(1, 1, 1, 1))
plot( mod$fit, mod$res, xlab = "Fitted", ylab = "Residuals",
      main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' ) 
qqnorm( mod$res, ylab = "Raw Residuals", pch = 16 )
qqline( mod$res)
shapiro.test( mod$res )
# p value really low, data may not be gaussian (there is a strong presence of outliers that may be
# influencing this result)
hist( mod$res, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
#heavy tail, the presence of outliers may be crucial
#LEVERAGE POINTS
X = model.matrix( mod )
lev = hat(X)
p = mod$rank;
n = length(DIOX);
plot( mod$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( mod$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )
# RESIDUAL ANALYSIS
gs = summary(mod)
res_std = mod$residuals/sd(DIOX)
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd
plot( mod$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( mod$fitted.values[watchout_ids_rstd],
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( mod$fitted.values[watchout_ids_lev],
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'),
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )
# Studentized residuals
# 'rstandard' gives studentized residuals automatically
stud = rstandard( mod )
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
plot( mod$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( mod$fitted.values[watchout_ids_stud],
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
points( mod$fitted.values[watchout_ids_lev],
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','orange'),
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )
# Cook Distance
Cdist = cooks.distance( mod )
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist
par( mfrow = c( 1, 3 ) )
plot( mod$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( mod$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )
plot( mod$fitted.values, stud, pch = 16, xlab = 'Fitted values',
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( mod$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ],
        col = 'pink', pch = 16 )
plot( mod$fitted.values, lev, pch = 16, xlab = 'Fitted values',
      ylab = 'Leverages', main = 'Leverages' )
points( mod$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )
id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gl = lm( DIOX ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + LOAD + OXYGEN + PRSEK + QROEG + TOVN + TROEG + POVN + CO2 + CO + SO2 + HCL + H2O, dat[ id_to_keep, ] )
summary( gl )
# R squared is much better than before
modelg = step( mod, direction = "backward" , k = 2, trace = T);
summary(modelg)

### END OF 7
