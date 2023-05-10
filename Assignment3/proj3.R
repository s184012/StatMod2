library(tidyverse)
library(dplyr)
library(DHARMa)
library(car)
library(caret)
library(nlme)
library(lme4)
library(MASS)
library(SuppDists)
library(GGally)
library(bbmle)
library(lme4)
library(glmmTMB)
library(TMB)
library(influence.ME)
rm(list=ls())
save_fig = function(name, figure = last_plot()) {
  ggsave(plot = figure, file = paste(name, '.pdf', sep=''), width=10*0.8, height = 6*0.8)
}



cloth = read.csv("clothingFullAss03.csv", header = T)
cloth$sex = as.factor(cloth$sex)
cloth$subjId = as.factor(cloth$subjId)
cloth$day = as.factor(cloth$day)
cloth$subDay = as.factor(cloth$subDay)



attach(cloth)

####START OF 1
boxplot(clo~subjId)
modn <- lme(clo~(tOut+tInOp+ sex)^2
            ,random =~ 1|subjId,
             data=cloth, method="ML")

summary(modn)
drop1(modn, test = "Chisq")
modn1 <- update(modn,.~.-tInOp:tOut)
drop1(modn1, test = "Chisq")
modn2 <- update(modn1,.~.-sex:tOut)
drop1(modn2, test = "Chisq")
modn2 <- lme(clo~tOut + tInOp + sex + tInOp:sex
            ,random =~ 1|subjId,
            data=cloth)
summary(modn2)

plot(modn2)
par(mfrow=c(1,2))

qqnorm(resid(modn2))
qqline(resid(modn2))
hist(rand$`(Intercept)`, probability = TRUE, col = "gray", main = 'Random Effects', xlab = 'Random intercept')
abline(v = mean(rand$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$`(Intercept)`), col = 'green', lwd = 3)

shapiro.test(rand$`(Intercept)`)




modg <- glmmTMB(clo~(tOut+tInOp+sex)^2 + (1|subjId), family = Gamma(link = "log"),
              data=cloth)
summary(modg)
drop1(modg, test = "Chisq")
modg1 <- update(modg,.~.-sex:tOut)
drop1(modg1, test = "Chisq")
modg2 <- update(modg1,.~.-tInOp:tOut)
drop1(modg2, test = "Chisq")
summary(modg2)

###3 PART 3

mod_nest <- lme(clo~(tOut+tInOp+ sex)^2
             ,random =~ 1|subjId/day,
             data=cloth, method="ML")
summary(mod_nest) #improves dramatically
anova(modn1, mod_nest) #the models are indeed different

#Reducing
drop1(mod_nest, test = "Chisq")
mod_nest1 <- update(mod_nest,.~.-tInOp:tOut)
drop1(mod_nest1, test = "Chisq")
mod_nest2 <- update(mod_nest1,.~.-sex:tOut)
drop1(mod_nest2, test = "Chisq")
mod_nest3 <- update(mod_nest2,.~.-tOut)
drop1(mod_nest3, test = "Chisq")
summary(mod_nest3)


mod_nest3 <- lme(clo ~ tInOp + sex + tInOp:sex
                ,random =~ 1|subjId/day,
                data=cloth, method = "ML")

modn_appr3 <- lme(clo ~ tInOp + sex + tInOp:sex 
                  ,random =list(subjId=~1,subDay=~1),
                  data=cloth, method="REML") 
mod_nest3 <-modn_appr3
summary(mod_nest3)
plot(mod_nest3)
rand = ranef(mod_nest3)
par(mfrow=c(1,3))

qqnorm(resid(mod_nest3))
qqline(resid(mod_nest3))
hist(rand$subjId$`(Intercept)`, probability = TRUE, col = "gray", main = 'SubjId', xlab = 'Random intercept')
abline(v = mean(rand$subjId$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$subjId$`(Intercept)`), col = 'green', lwd = 3)
hist(rand$day$`(Intercept)`, probability = TRUE, col = "gray", main = 'Day', xlab = 'Random intercept')
abline(v = mean(rand$day$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$day$`(Intercept)`), col = 'green', lwd = 3)
shapiro.test(resid(mod_nest3))
shapiro.test(rand$day$`(Intercept)`)
shapiro.test(rand$subjId$`(Intercept)`)
#Try with the approximation of subDay
modn_appr <- lme(clo~(tOut+tInOp+ sex)^2
                 ,random =list(subjId=~1,subDay=~1),
                 data=cloth, method="ML") 
summary(modn_appr)
summary(mod_nest1)
anova(modn_appr, mod_nest1) #same model


saveFig <- TRUE
if(saveFig == TRUE){pdf("PA3_Random_Int_SubDay.pdf", width = 10*0.8, height = 8*0.8)}
par(mfrow=c(1,1))
hist(rand$subjId$`(Intercept)`, probability = TRUE, col = "gray", main = 'SubDay', xlab = 'Random intercept')
abline(v = mean(rand$subjId$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$subjId$`(Intercept)`), col = 'green', lwd = 3)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


#Reduce
drop1(modn_appr, test = "Chisq")
modn_appr1 <- update(modn_appr,.~.-tInOp:tOut)
drop1(modn_appr1, test = "Chisq")
modn_appr2 <- update(modn_appr1,.~.-sex:tOut)
drop1(modn_appr2, test = "Chisq")
modn_appr3 <- update(modn_appr2,.~.-tOut)
drop1(modn_appr3, test = "Chisq")
summary(modn_appr3)
modn_appr3 <- lme(clo ~ tInOp + sex + tInOp:sex 
                 ,random =list(subjId=~1,subDay=~1),
                 data=cloth, method="ML") 
summary(modn_appr3)
summary(mod_nest3)
anova(modn_appr3, mod_nest3)


###
modg_nest <- glmmTMB(clo~(tOut+tInOp+ sex)^2+ (1|subjId/day), family = Gamma(link = "log"),
                data=cloth)
summary(modg_nest)
drop1(modg_nest, test = "Chisq")
modg_nest1 <- update(modg_nest,.~.-sex:tOut)
drop1(modg_nest1, test = "Chisq")
modg_nest2 <- update(modg_nest1,.~.-tInOp:tOut)
drop1(modg_nest2, test = "Chisq")
summary(modg2)




modg_appro = glmmTMB(clo~tOut+ sex + (1|subDay) + (1|subjId), family = Gamma(link = "log"),
                  data=cloth)
summary(modg_appro)







#CORR STRUCTURE (4)
mod_autarima <- lme(clo~(tOut+tInOp+ sex)^2,random =~ 1|subDay,
               correlation = corAR1(form =~ as.numeric(time)|subDay),
                data=cloth, method="ML")
summary(mod_autarima)
mod_autgauss <- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                    correlation = corGaus(form =~ as.numeric(time)|subDay),
                    data=cloth, method="ML")
mod_autexp <- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                    correlation = corExp(form =~ as.numeric(time)|subDay),
                    data=cloth, method="ML")
mod_autcompsymm <- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                  correlation = corCompSymm(form =~ as.numeric(time)|subDay),
                 data=cloth, method="ML")
#Doesn't work
mod_autarma<- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                       correlation = corARMA(form =~ as.numeric(time)|subDay),
                       data=cloth, method="ML")
##
mod_autlin<- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                  correlation = corLin(form =~ as.numeric(time)|subDay),
                  data=cloth, method="ML")
mod_autratio<- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                 correlation = corRatio(form =~ as.numeric(time)|subDay),
                 data=cloth, method="ML")
mod_autsphe<- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                   correlation = corSpher(form =~ as.numeric(time)|subDay),
                   data=cloth, method="ML")

#SAVE THIS TABLE
AutoCompare <- anova(mod_autarima, mod_autgauss, mod_autcompsymm, mod_autexp, mod_autlin, mod_autsphe, mod_autratio)
AutoCompare
write.csv(AutoCompare, "PA4_AutoCompare.csv", row.names=TRUE)

par(mfrow=c(2,2))
  plot(Variogram(mod_autexp), main = 'Exp') #USE THIS ONE!!!!!!!!!!!!!???
  plot(Variogram(mod_autratio), main = 'Ratio')

#Aut Exp the best one
#PART 4 continue

#Reduce model using Chisq and ML for estimation of parameters.
mod_autexp <- lme(clo~(tOut+tInOp+ sex)^2 ,random =~ 1|subDay,
                  correlation = corExp(form =~ as.numeric(time)|subDay),
                  data=cloth, method="ML")
drop1(mod_autexp, test = "Chisq")
mod_autexp1 <- update(mod_autexp,.~.-tOut:tInOp)
drop1(mod_autexp1, test = "Chisq")
mod_autexp2 <- update(mod_autexp1,.~.-tOut:sex)
drop1(mod_autexp2, test = "Chisq")
mod_autexp3 <- update(mod_autexp2,.~.-tOut)
drop1(mod_autexp3, test = "Chisq")
summary(mod_autexp3)

mod_autoexp_final <- lme(clo~tInOp+sex+tInOp:sex,random=~1|subDay,
                         correlation = corExp(form =~ as.numeric(time)|subDay), 
                         data=cloth, method="REML")
summary(mod_autoexp_final)

#Plot model and residuals
plot(mod_autoexp_final)
rand = ranef(mod_autoexp_final)
par(mfrow=c(1,2))

qqnorm(resid(mod_autoexp_final))
qqline(resid(mod_autoexp_final))
hist(rand$`(Intercept)`, probability = TRUE, col = "gray", main = 'SubDay', xlab = 'Random intercept')
abline(v = mean(rand$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$`(Intercept)`), col = 'green', lwd = 3)

shapiro.test(resid(mod_autoexp_final))
shapiro.test(rand$`(Intercept)`)
#END PART 4

preds <- predict(mod_autoexp_final)

plot_df <- data.frame(list(preds = preds, tInOp = tInOp, sex = sex)) 
summary(mod_autoexp_final)

saveFig <- TRUE
if(saveFig == TRUE){pdf("PA4_autoexpmodel_residuals.pdf", width = 10*0.8, height = 10*0.8)}
plot(mod_autoexp_final)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("PA4_semi_variogram_expon.pdf", width = 10*0.8, height = 10*0.8)}
plot(Variogram(mod_autexp), main = 'Exp')
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


saveFig <- TRUE
if(saveFig == TRUE){pdf("PA4_QQ_plot_Random_intercepts.pdf", width = 10*0.8, height = 6*0.8)}
par(mfrow=c(1,2))
qqnorm(resid(mod_autoexp_final))
qqline(resid(mod_autoexp_final))
hist(rand$`(Intercept)`, probability = TRUE, col = "gray", main = 'SubDay', xlab = 'Random intercept')
abline(v = mean(rand$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$`(Intercept)`), col = 'green', lwd = 3)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

par(mfrow=c(1,2))
qqnorm(resid(mod_autoexp_final))
qqline(resid(mod_autoexp_final))
hist(rand$`(Intercept)`, probability = TRUE, col = "gray", main = 'SubDay', xlab = 'Random intercept')
abline(v = mean(rand$`(Intercept)`), col='red', lwd = 3)
lines(density(rand$`(Intercept)`), col = 'green', lwd = 3)

qqnorm(rand$`(Intercept)`)
qqline(rand$`(Intercept)`)







#Lorenzo stuff??
#Don't know how to remove intercepts here
modg_autdiag = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + diag(time-1|subDay), family = Gamma(link = "log"),
                     data=cloth)
modg_autarima = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + ar1(as.factor(time) - 1|subDay), family = Gamma(link = "log"),
                       data=cloth)
summary(modg_autarima)
modg_autus = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + us(time-1|subDay), family = Gamma(link = "log"),
                       data=cloth)
summary(modg_autus)

modg_autexp = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + exp(time-1|subDay), family = Gamma(link = "log"),
                       data=cloth)
modg_autgau = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + gau(time-1|subDay), family = Gamma(link = "log"),
                       data=cloth)
modg_autmat = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + mat(time-1|subDay), family = Gamma(link = "log"),
                      data=cloth)
modg_auttoep = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + toep(time-1|subDay), family = Gamma(link = "log"),
                      data=cloth)
modg_autrr = glmmTMB(clo~tOut + tInOp + sex + tInOp:sex + (1|subDay) + rr(time-1|subDay), family = Gamma(link = "log"),
                      data=cloth)
AICtab(modg_autdiag, modg_autus, modg_autarima, modg_auttoep, modg_autrr)
summary(modg_autarima)
plot(modg_autarima)

testResiduals(modg_autarima)
#€xtensions
#we could include the number of measurements within the days

mod_autocorrcomp <- lme(clo~tOut + tInOp + sex + tInOp:sex + tOut:sex,random =~ 1|subDay,
                        correlation = corExp(form =~ time2|subDay),
                        data=cloth, method="ML")
summary(mod_autocorrcomp)  
modg_autaricom= glmmTMB(clo~tOut + tInOp + sex + tInOp:sex+ (1|subDay)+ ar1(as.factor(time2) - 1|subDay), family = Gamma(link = "log"),
                        data=cloth)

summary(modg_autaricom)
detach(cloth)
































