library(tidyverse)
library(dplyr)
library(DHARMa)
library(car)
library(caret)
library(nlme)
library(lme4)
library(MASS)
library(SuppDists)
rm(list=ls())

cloth = read.csv("clothing.csv", header = T)
cloth$sex = as.factor(cloth$sex)
cloth$subjId = as.factor(cloth$subjId)

attach(cloth)

plot(density(sqrt(cloth$clo)))

###INVERSE GAUSSIAN 
modinv = glm(clo~(tOut + tInOp + sex)^2, family = inverse.gaussian(link = "1/mu^2"), data = cloth)
summary(modinv) 
par(mfrow=c(2,2))
plot(modinv)  
sinv <- simulateResiduals(modinv) #no fit.
plot(sinv)
drop1(modinv, test = "F")
modinv2 <- update(modinv,.~.-tInOp:sex)
drop1(modinv2, test = "F")


summary(modinv2)
par(mfrow=c(2,2))
plot(modinv2)

########

#GAUSSIAN
modgauss = glm((clo)~(tOut + tInOp + sex)^2, family = gaussian(link = "identity"), data = cloth)
summary(modgauss)
par(mfrow=c(2,2))
plot(modgauss)  
sga <- simulateResiduals(modgauss) #no good fit.
plot(sga)
modgausslog = glm((clo)~(tOut + tInOp + sex)^2, family = gaussian(link = "log"), data = cloth)
summary(modgausslog)
par(mfrow=c(2,2))
plot(modgausslog)  

modgaussinv = glm((clo)~(tOut + tInOp + sex)^2, family = gaussian(link = "inverse"), data = cloth)
summary(modgaussinv)
par(mfrow=c(2,2))
plot(modgaussinv)  
#####

mod1 = glm((clo)~(tOut + tInOp + sex)^2, family = Gamma(link = "inverse"), data = cloth)
summary(mod1) 
sma <- simulateResiduals(mod1) #still no good fit.
plot(sma)
par(mfrow=c(2,2))
plot(mod1)                                                                     
drop1(mod1, test = "F")
mod2 <- update(mod1,.~.-tInOp:sex)
drop1(mod2, test = "F")
mod3 <- update(mod2,.~.-tOut:tInOp)
drop1(mod3, test = "F")
mod4 <- update(mod3,.~.-tInOp)
drop1(mod4, test = "F")
summary(mod4)
par(mfrow=c(2,2))
plot(mod4)  
smar <- simulateResiduals(mod4)
plot(smar) 

modl1 = glm(clo~(tOut + tInOp + sex)^2, family = Gamma(link = "log"), data = cloth)
summary(modl1)
par(mfrow=c(2,2))
plot(modl1)                                                                     
drop1(modl1, test = "F")
modl2 <- update(modl1,.~.-tInOp:tOut)
drop1(modl2, test = "F")
modl3 <- update(modl2,.~.-sex:tInOp)
drop1(modl3, test = "F")
modl4 <- update(modl3,.~.-tInOp)
drop1(modl4, test = "F")
summary(modl4)  
par(mfrow=c(2,2))
plot(modl4)  

modi1 = glm(clo~(tOut*tInOp*sex), family = Gamma(link = "identity"), data = cloth)
summary(modi1)
par(mfrow=c(2,2))
plot(modi1)                                                                     
drop1(modi1, test = "F")
modi2 <- update(modi1,.~.-tInOp:sex)
drop1(modi2, test = "F")
modi3 <- update(modi2,.~.-tOut:tInOp)
drop1(modi3, test = "F")
modi4 <- update(modi3,.~.-tInOp)
drop1(modi4, test = "F")
summary(modi4)  #Worse AIC than the others

par(mfrow=c(2,2))
plot(modi1)  

#CONCLUSION: gamma identity

#RESIDUAL ANALYSIS
nfem = sum(cloth$sex=="female")
nmal = length(clo)-nfem


par(mfrow=c(1,1))
resDev <- residuals(modi1,type="pearson")
plot(jitter(as.numeric(sex), amount=0.1), resDev,
     xlab="Sex", ylab="Deviance residuals", cex=0.6,
     axes=FALSE)
box()
axis(1,label=c("Female", "Male" ),at=c(1,2))
axis(2)
sd(modi1$residuals[sex=='male'])
sd(modi1$residuals[sex=='female'])

#There seems to be higher variance when it comes to female

hist( modi4$residuals, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
X = model.matrix( modi4)
lev = hat(X)
p = modi4$rank;
n = length(clo);
plot( modi4$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( modi4$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )

# RESIDUAL ANALYSIS
gs = summary(modi4)
res_std = modi4$residuals/sd(clo)
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd
plot( modi4$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( modi4$fitted.values[watchout_ids_rstd],
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( modi4$fitted.values[watchout_ids_lev],
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'),
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )
# Studentized residuals
stud = rstudent( modi4 )
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
plot( modi4$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( modi4$fitted.values[watchout_ids_stud],
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
points( modi4$fitted.values[watchout_ids_lev],
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink','orange'),
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )

# Cook Distance
Cdist = cooks.distance( modi4 )
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist
par( mfrow = c( 1, 3 ) )
plot( modi4$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( modi4$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )
plot( modi4$fitted.values, stud, pch = 16, xlab = 'Fitted values',
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( modi4$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ],
        col = 'pink', pch = 16 )
plot( modi4$fitted.values, lev, pch = 16, xlab = 'Fitted values',
      ylab = 'Leverages', main = 'Leverages' )

points( modi4$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )

id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gl = glm(clo~(tOut + sex)^2, family = Gamma(link = "identity"), data = cloth[ id_to_keep, ])


summary( gl )
par(mfrow=c(2,2))
plot(gl)      
#Better

##

mo = glm(clo~(tOut + tInOp)^2 + subjId, family = Gamma(link = "identity"), data = cloth)
summary(mo)
so <- simulateResiduals(mo)
plot(so)
par(mfrow=c(2,2))
plot(mo)  

drop1(mo, test = "F")
mo2 <- update(mo,.~.-tOut:tInOp)
drop1(mo2, test = "Chisq")
summary(mo2)
par(mfrow=c(2,2))
plot(mo2)  
par(mfrow=c(1,1))
resDev <- residuals(mo2,type="pearson")
plot(jitter(as.numeric(day), amount=0.1), resDev,
     xlab="Day", ylab="Deviance residuals", cex=0.6,
     axes=FALSE)
box()
axis(1,label=c("DAY 1", "DAY 2" , "DAY 3", "DAY 4" ),at=c(1,2,3,4))
axis(2)
#AT DAY 4 SEEM TO BE TOO LITTLE OBSERVATION, NO EVIDENCE OF HETEROSCEDASTICITY WITHIN DAYS
####
modmw = glm(clo~sex, family = Gamma(link = "identity"), data = cloth)
summary(modmw)
par(mfrow=c(2,2))
plot(modmw)  
coefficients <- coef(modmw)
p = profile(modmw, maxsteps = 100)
plot(p$sexmale$par.vals,p$sexmale$tau)

lambda_range <- seq(0.04, 2, by = 0.01)
log_likelihood <- function(lambda){
  -sum(dgamma(clo, shape = coefficients[1]/lambda, scale = 1, log = TRUE))
}
profile_likelihood <- sapply(lambda_range, log_likelihood)
plot(lambda_range, profile_likelihood/min(profile_likelihood), type = "l", xlab = "Weight/dispersion parameter", ylab = "Profile log-likelihood")
dev.new()
m = lambda_range[profile_likelihood==min(profile_likelihood)]
w
w = as.numeric(sex=="male") + as.numeric(sex=="female")/coefficients[1]
gw = glm(clo~(tOut + sex + tInOp + tOut:sex + tOut:tInOp + tInOp:sex), family = Gamma(link = "identity"), data = cloth, weights = w )
logLik(gw)
logLik(modi1)
summary(gw)
par(mfrow=c(2,2))
plot(gw)  
par(mfrow=c(1,1))
resDevw <- residuals(gw,type="pearson")
plot(jitter(as.numeric(sex), amount=0.1), resDevw,
     xlab="Sex", ylab="Deviance residuals", cex=0.6,
     axes=FALSE)
box()
axis(1,label=c("Female", "Male" ),at=c(1,2))
axis(2)


#the problem seems to be solved now
detach(cloth)

####
swim = read.table("earinfect.txt", header = T)
swim$swimmer = as.factor(swim$swimmer)
swim$location = as.factor(swim$location)
swim$age = as.factor(swim$age)
swim$sex = as.factor(swim$sex)
attach(swim)



mod = glm(infections~(age + sex +swimmer + location)^2, offset = log(persons), family = poisson(link = "log"))
summary(mod)
par(mfrow=c(2,2))
plot(mod)  

1 - pchisq(2.5421, 9) #Really good.


mod_rev1 = glm(infections~(age + sex +swimmer + location)^2, offset = log(persons), family = poisson(link = "sqrt"))
summary(mod_rev1)
par(mfrow=c(2,2))
plot(mod)
1 - pchisq(1.8822, 9)

mod_rev2 = glm(infections~(age + sex +swimmer + location)^2, offset = log(persons), family = poisson(link = "identity"))
summary(mod_rev2)
par(mfrow=c(2,2))
plot(mod)
1 - pchisq(5.5353, 9) #even worse, the best is the second one(aic). All of three have good fit though



#When reducing, we look if the AIC is better
drop1(mod_rev1, type = "F")
modr2  <- update(mod_rev1,.~.-age:location)
drop1(modr2, type = "F")
modr3  <- update(modr2,.~.-age:swimmer)
drop1(modr3, type = "F")
modr4  <- update(modr3,.~.-sex:swimmer)
drop1(modr4, type = "F")
modr5  <- update(modr4,.~.-sex:age)
drop1(modr5, type = "F")
modr6  <- update(modr5,.~.-age)
drop1(modr6, type = "F")
modr7  <- update(modr6,.~.-swimmer:location)
drop1(modr7, type = "F")
modr8  <- update(modr7,.~.-swimmer)
drop1(modr8, type = "F")
summary(modr8)

1 - pchisq(8.917, 9) #GOF worsened, but AIC is better. The GOF measure remains still good.
anova(modr8,mod)

par(mfrow=c(2,2))
plot(modr8)

#try with quasi possion, even though it doesn't seem to be to much overdisp
modq = glm(infections~(age + sex +swimmer + location)^2, offset = log(persons), family = quasipoisson(link = "log"), data = swim)
summary(modq)
par(mfrow=c(2,2))
plot(modq)
drop1(modq, type = "F")
