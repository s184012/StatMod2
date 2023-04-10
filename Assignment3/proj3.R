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

modi <- glmer(clo~tOut+tInOp+tOut:tInOp + (1|subjId), family = Gamma(link = "identity"),
                   data=cloth)
modni <- glmer(clo~tOut+tInOp+ (1|subjId), family = Gamma(link = "identity"),
              #orrelation=corGaus(form=~as.numeric(month)|cage,nugget=TRUE),
              data=cloth)
summary(modni)
plot(modni)
summary(modi)
plot(modi)
anova(modi,modni)
#interaction non significant

modri <- glmer(clo~tOut+tInOp + (1|subjId/day), family = Gamma(link = "identity"),
                      data=cloth)
summary(modri)
plot(modri)
anova(modri, modi)
#The models are indeed different, including that information on the days is good

mod_appro = glm(clo~tOut+tInOp+ subDay, family = Gamma(link = "identity"),
                  data=cloth)
summary(mod_appro)
par(mfrow=c(2,2))
plot(mod_appro)
logLik(mod_appro)
logLik(modri)
#Maybe reduce the models afterwards, if there is evidence to do so 

mod_autocorr <- glmer(clo~tOut+tInOp+ (1|subDay) + (time|subDay), family = Gamma(link = "identity"),
                      data=cloth)
summary(mod_autocorr)
#control = glmerControl(correlation=corAR1(form =~ time|subDay)))
#should we use nlme or lme? Should it be nested?

plot(mod_autocorr)
#we could include the number of measurements within the days

mod_autocorrcomp <- glmer(clo~tOut+tInOp+ (1|subDay) + (time/time2|subDay), family = Gamma(link = "identity"),
                      data=cloth)
summary(mod_autocorrcomp)
anova(mod_autocorrcomp, mod_autocorr)
#Including this other information improves the model

