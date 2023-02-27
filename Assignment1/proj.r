library(GGally)
library(car)
data = read.csv("dioxin.csv", header = TRUE, sep = ",");
print(sapply(data, typeof))
data <- as.data.frame(unclass(data),stringsAsFactors = TRUE)
data$TIME = as.factor(data$TIME)
print(sapply(data, typeof))

attach(data)
ggpairs(data = data, columns = c(2,9:21), title ="Relationships between predictors & response",
       lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))
data = na.omit(data)
mod_1 = lm(DIOX ~ PLANT + TIME + LAB + LOAD + OXYGEN + PRSEK)
cor(mod_1)
summary(mod_1)
AIC(mod_1); #very high akaike index, we have to reduce it
mod_2 = step( mod_1, direction = "backward" , k = log(length(DIOX)), trace = T);
summary(mod_2)
mod_3 = step( mod_1, direction = "backward" , k = 2, trace = T);
summary(mod_3)

#p-value of 0.1, could be acceptable

mod_1rev = lm(DIOX ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + LOAD + OXYGEN + PRSEK, data = data)
summary(mod_1rev)
mod_2rev = step( mod_1rev, direction = "backward" , k = log(length(DIOX)), trace = T);
mod_3rev = step( mod_1rev, direction = "backward" , k = 2, trace = T);
summary(mod_3rev)
# we notice that the measured active variables are removed

#Using the model with the measured active variables, predict the dioxin
#emission in the first visit to the RENO_N (2) plant, analysed in the KK(1)
#laboratory with O2COR = 0.5, NEFFEKT = -0.01 and QRAT = 0.5

#We use mod_3rev, so we don't need QRAT and 02COR
modi = lm(DIOX ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT)
summary(modi)
modir = lm(DIOX ~ PLANT + TIME + LAB + NEFFEKT + QRAT)
summary(modir)
modiro = lm(DIOX ~ PLANT + TIME + LAB + NEFFEKT )
summary(modiro)
#even the manual elimination gives back the result we wanted, hence
v = c(2)

new_data = data.frame(PLANT = factor("RENO_N", levels = levels(PLANT)), TIME = factor("1", levels = levels(TIME)), LAB = factor("KK", levels = levels(LAB)), NEFFEKT = c(-0.01))
diox_pred = predict(mod_3rev, newdata = new_data, interval = "confidence", se = TRUE)
diox_pred$fit

##
dev.new()
#Complete model
mod = lm(DIOX ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + LOAD + OXYGEN + PRSEK + QROEG + TOVN + TROEG + POVN + CO2 + CO + SO2 + HCL + H2O, data = data)
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
gl = lm( DIOX ~ PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT + LOAD + OXYGEN + PRSEK + QROEG + TOVN + TROEG + POVN + CO2 + CO + SO2 + HCL + H2O, data[ id_to_keep, ] )
summary( gl )
# R squared is much better than before
modelg = step( mod, direction = "backward" , k = 2, trace = T);
summary(modelg)
