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
install.packages("bbmle")
library(bbmle)
rm(list=ls())
setwd("C:/Users/loren/OneDrive - Politecnico di Milano/Quantitative Finance/MODEL IDENTIFICATION/StatMod2/StatMod2/Assignment2")

####
rm(list=ls())
swim = read.table("earinfect.txt", header = T)
swim$swimmer = as.factor(swim$swimmer)
swim$location = as.factor(swim$location)
swim$age = as.factor(swim$age)
swim$sex = as.factor(swim$sex)
attach(swim)

plot(persons,infections)
plot(location, infections/persons)

swim |> 
  ggplot(aes(x=infections/persons, fill=location)) +
  geom_histogram()
ggpairs(swim)
mod = glm(infections~age + sex + swimmer + location + persons, offset = log(persons), family = poisson(link = "log"))
summary(mod)
par(mfrow=c(2,2))
plot(mod)  
res = residuals(mod, type="pearson")
1 - pchisq(8.0125, 17) #Really good.


#try with negative binomial
modnb = glm.nb(infections~age + sex + swimmer + location + persons + offset(log(persons)), data = swim)
summary(modnb)
par(mfrow=c(2,2))
plot(modnb) 
#theta goes to infinity

#IN THE OTHER LINK FUNCTIONS THERE IS NO POSSIBILITY OF INCLUDING THE OFFSET IN THE CORRECT WAY
drop1(mod, test = "Chisq")
mod1 <- update(mod,.~.-age)
drop1(mod1, test = "Chisq")
mod2 <- update(mod1,.~.-sex)
drop1(mod2, test = "Chisq")
mod3 <- update(mod2,.~.-persons)
drop1(mod3, test = "Chisq")
mod4 <- update(mod3,.~.-swimmer)
drop1(mod4, test = "Chisq")
summary(mod4)
par(mfrow=c(2,2))
plot(mod4)
1 - pchisq(9.6286,22) 
exp(mod4$coefficients)



#OMIT

mod_rev1 = glm(infections~(age*sex*swimmer*location), offset = sqrt(persons), family = poisson(link = "sqrt"))
summary(mod_rev1)
par(mfrow=c(2,2))
plot(mod_rev1)
1 - pchisq(1.8822, 9)

mod_rev2 = glm(infections~(age + sex + swimmer + location) + age:sex + swimmer:location + 
                 age:swimmer + age:location + sex:location + sex:swimmer, offset = (persons), family = poisson(link = "identity"))
summary(mod_rev2)
par(mfrow=c(2,2))
plot(mod)
1 - pchisq(5.5353, 9) 
#END OMIT

plot(location,infections/persons)
drop1(mod, test = "Chisq")
mod1 <- update(mod,.~.-age:sex:swimmer:location)
drop1(mod1, test = "Chisq")
mod2 <- update(mod1,.~.-age:sex:swimmer)
drop1(mod2, test = "Chisq")
mod3 <- update(mod2,.~.-age:swimmer:location)
drop1(mod3, test = "Chisq")
mod4 <- update(mod3,.~.-location:sex:swimmer)
drop1(mod4, test = "Chisq")
mod5 <- update(mod4,.~.-sex:swimmer)
drop1(mod5, test = "Chisq")
mod6 <- update(mod5,.~.-age:swimmer)
drop1(mod6, test = "Chisq")
mod7 <- update(mod6,.~.-sex:age:location)
drop1(mod7, test = "Chisq")
mod8 <- update(mod7,.~.-age:location)
drop1(mod8, test = "Chisq")
mod9 <- update(mod8,.~.-age:sex)
drop1(mod9, test = "Chisq")
mod10 <- update(mod9,.~.-age)
drop1(mod10, test = "Chisq")
mod11 <- update(mod10,.~.-swimmer:location)
drop1(mod11, test = "Chisq")
mod12 <- update(mod11,.~.-swimmer)
drop1(mod12, test = "Chisq")
mod13 <- update(mod12,.~.-sex:location)
drop1(mod13, test = "Chisq")
mod14 <- update(mod13,.~.-sex)
drop1(mod14, test = "Chisq")
summary(mod14)
par(mfrow=c(2,2))
plot(mod14)

anova(mod,mod14, test="Chisq")


#When reducing, we look if the AIC is better
reduced_model <- step(mod)
par(mfrow=c(2,2))
plot(reduced_model)

summary(reduced_model)
1 - pchisq(8.917, 20) #OK
anova(reduced_model,mod_rev1)

par(mfrow=c(2,2))
plot(reduced_model)

#try with quasi possion
modql = glm(infections~(age+sex + swimmer + location) + age:sex + swimmer:location + 
              age:swimmer + age:location + sex:location + sex:swimmer, offset = log(persons), family = quasipoisson(link = "log"), data = swim)
summary(modql)
par(mfrow=c(2,2))
plot(modql)
drop1(modql, test = "F")
modql1 <- update(modql,.~.-sex:swimmer)
drop1(modql1, test = "F")
modql2 <- update(modql1,.~.-age:swimmer)
drop1(modql2, test = "F")
modql3 <- update(modql2,.~.-swimmer:location)
drop1(modql3, test = "F")
modql4 <- update(modql3,.~.-age:location)
drop1(modql4, test = "F")
modql5 <- update(modql4,.~.-age:sex)
drop1(modql5, test = "F")
modql6 <- update(modql5,.~.-age)
drop1(modql6, test = "F")
summary(modql6)


modqi = glm(infections~(age+sex + swimmer + location) + age:sex + swimmer:location + 
              age:swimmer + age:location + sex:location + sex:swimmer, offset = log(persons), family = quasipoisson(link = "identity"), data = swim)
summary(modqi)
par(mfrow=c(2,2))
plot(modqi) #shit

modqs = glm(infections~(age+sex + swimmer + location) + age:sex + swimmer:location + 
              age:swimmer + age:location + sex:location + sex:swimmer, offset = log(persons), family = quasipoisson(link = "sqrt"), data = swim)
summary(modqs)
par(mfrow=c(2,2))
plot(modqs) #better


sim_fmp <- simulateResiduals(reduced_model, refit=T)
testOverdispersion(sim_fmp)
#there is no evidence of overdispersion, so we choose the initial model


detach(swim)