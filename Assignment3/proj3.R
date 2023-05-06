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
rm(list=ls())
save_fig = function(name, figure = last_plot()) {
  ggsave(plot = figure, file = paste(name, '.pdf', sep=''), path = 'figs', width=10*0.8, height = 6*0.8)
}



cloth = read.csv("clothingFullAss03.csv", header = T)
cloth$sex = as.factor(cloth$sex)
cloth$subjId = as.factor(cloth$subjId)
cloth$day = as.factor(cloth$day)
cloth$subDay = as.factor(cloth$subDay)



attach(cloth)

####START OF 1
boxplot(clo~subjId)
modn <- lme(clo~tOut+tInOp+tOut:tInOp + sex
            ,random =~ 1|subjId,
             data=cloth, method="ML")
modn1 <- lme(clo~tOut+tInOp+sex
             ,random =~ 1|subjId,
             data=cloth, method="ML")
modn2 <- lme(clo~tOut+ sex
             ,random =~ 1|subjId,
             data=cloth, method="ML") #this one is the best
summary(modn)
summary(modn1)
summary(modn2)
anova(modn,modn1)

plot(modn)
modg <- glmmTMB(clo~tOut+tInOp+ sex + (1|subjId), family = Gamma(link = "log"),
              data=cloth)
summary(modg)
#no interaction because of convergence problems with GLMMTB
warnings()

###2

mod_nest <- lme(clo~tOut + sex
             ,random =~ 1|subjId/day,
             data=cloth, method="ML")
summary(mod_nest) #improves dramatically
anova(modn2, mod_nest) #the models are indeed different


#Try with the approximation of subDay
modn_appr <- lme(clo~tOut+ sex
                 ,random =list(subjId=~1,subDay=~1),
                 data=cloth, method="ML") 
summary(modn_appr)
anova(modn_appr, mod_nest) #same model



modg_nest <- glmmTMB(clo~tOut+ sex + (1|subjId/day), family = Gamma(link = "log"),
                data=cloth)
summary(modg_nest)
modg_appro = glmmTMB(clo~tOut+ sex + (1|subDay) + (1|subjId), family = Gamma(link = "log"),
                  data=cloth)
summary(modg_appro)
#the Gamma model seems to be a better fit. Once again SubDay is an exact approximation
#should we add other fixed effects to it?


#CORR STRUCTURE (4)
mod_autarima <- lme(clo~tOut+ sex,random =~ 1|subDay,
               correlation = corAR1(form =~ as.numeric(time)|subDay),
                data=cloth, method="ML")
summary(mod_autarima)
mod_autgauss <- lme(clo~tOut,random =~ 1|subDay,
                    correlation = corGaus(form =~ as.numeric(time)|subDay),
                    data=cloth, method="ML")
mod_autexp <- lme(clo~tOut,random =~ 1|subDay,
                    correlation = corExp(form =~ as.numeric(time)|subDay),
                    data=cloth, method="ML")
mod_autcompsymm <- lme(clo~tOut,random =~ 1|subDay,
                  correlation = corCompSymm(form =~ as.numeric(time)|subDay),
                  data=cloth, method="ML")
mod_autarma<- lme(clo~tOut,random =~ 1|subDay,
                       correlation = corARMA(form =~ as.numeric(time)|subDay),
                       data=cloth, method="ML")
mod_autlin<- lme(clo~tOut,random =~ 1|subDay,
                  correlation = corLin(form =~ as.numeric(time)|subDay),
                  data=cloth, method="ML")
mod_autratio<- lme(clo~tOut,random =~ 1|subDay,
                 correlation = corRatio(form =~ as.numeric(time)|subDay),
                 data=cloth, method="ML")
mod_autsphe<- lme(clo~tOut,random =~ 1|subDay,
                   correlation = corSpher(form =~ as.numeric(time)|subDay),
                   data=cloth, method="ML")
anova(mod_autarima, mod_autgauss, mod_autcompsymm, mod_autexp, mod_autlin, mod_autsphe, mod_autratio)


#Aut Exp the best one

#Don't know how to remove intercepts here
modg_autdiag = glmmTMB(clo~tOut+ (1|subDay) + sex +diag(time-1|subDay), family = Gamma(link = "log"),
                     data=cloth)
modg_autarima = glmmTMB(clo~tOut+ (1|subDay) + sex + ar1(as.factor(time) - 1|subDay), family = Gamma(link = "log"),
                       data=cloth)
summary(modg_autarima)
modg_autus = glmmTMB(clo~tOut+ (1|subDay) + sex + us(time-1|subDay), family = Gamma(link = "log"),
                       data=cloth)
summary(modg_autus)

modg_autexp = glmmTMB(clo~tOut+ (1|subDay) + exp(time-1|subDay), family = Gamma(link = "log"),
                       data=cloth)
modg_autgau = glmmTMB(clo~tOut+ (1|subDay) + gau(time-1|subDay), family = Gamma(link = "log"),
                       data=cloth)
modg_autmat = glmmTMB(clo~tOut+ (1|subDay) + mat(time-1|subDay), family = Gamma(link = "log"),
                      data=cloth)
modg_auttoep = glmmTMB(clo~tOut+ (1|subDay) + sex + toep(time-1|subDay), family = Gamma(link = "log"),
                      data=cloth)
modg_autrr = glmmTMB(clo~tOut+ (1|subDay) + sex+ rr(time-1|subDay), family = Gamma(link = "log"),
                      data=cloth)
AICtab(modg_autdiag, modg_autus, modg_autarima, modg_auttoep, modg_autrr)
summary(modg_autarima)




#€xtensions
#we could include the number of measurements within the days

mod_autocorrcomp <- lme(clo~tOut + sex,random =~ 1|subDay,
                        correlation = corExp(form =~ time2|subDay),
                        data=cloth, method="ML")
summary(mod_autocorrcomp)  
modg_autaricom= glmmTMB(clo~tOut+ (1|subDay)+ sex + ar1(as.factor(time2) - 1|subDay), family = Gamma(link = "log"),
                        data=cloth)

summary(modg_autaricom)
detach(cloth)
































