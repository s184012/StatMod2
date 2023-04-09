library(tidyverse)
library(broom)
library(dplyr)
library(DHARMa)
library(car)
library(caret)
library(nlme)
library(lme4)
library(MASS)
library(SuppDists)
library(GGally)
#install.packages("bbmle")
library(bbmle)
rm(list=ls())




# Load data ---------------------------------------------------------------

load('data.rds')

# Contour plot ------------------------------------------------------------

save_fig = function(name, figure = last_plot()) {
  ggsave(plot = figure, file = paste(name, '.pdf', sep=''), path = 'figs', width=10*0.8, height = 6*0.8)
}

get_contour_predict <- function(..., with_formula) {
  predictions <- data.frame()
  prediction_tOut = seq(min(cloth$tOut), max(cloth$tOut), length.out=50)
  prediction_tInOp = seq(min(cloth$tInOp), max(cloth$tInOp), length.out=50)
  pred_df <- expand_grid(
    tOut = prediction_tOut,
    tInOp = prediction_tInOp,
    sex = cloth$sex
  )
  for (model in list(...)){
    name = paste(model$family$family, model$family$link, sep=", ")
    if(with_formula){
      name <- paste0(name, ': ', paste(model$call)[2])
    }
    print(name)
    pred_out <- predict(model, newdata=pred_df, type="response")
    pred_out <- cbind(pred_df, pred_out, model=name)
    predictions <- rbind(predictions, pred_out)
  }
  
  return(predictions)
  
}

plot_contour <- function(..., title, with_formula=F) {
  predictions <- get_contour_predict(..., with_formula=with_formula)
  predictions |> 
    ggplot(aes(x=tInOp, y=tOut, z=pred_out)) +
    geom_contour_filled(breaks=seq(0, 1, by=0.1), ) +
    facet_grid(cols=vars(sex), rows = vars(model)) +
    labs(
      title = title
    )
}




####### How to plot
Vplot <- data.frame(V=Vplot)
mu1 <- predict(logis.glm,newdata=Vplot,type="response",se=TRUE)$fit
se1 <- predict(logis.glm,newdata=Vplot, type="response", se=TRUE)$se.fit
mu2 <- predict(logis.glm2,newdata=Vplot,type="response",se=TRUE)$fit
se2 <- predict(logis.glm2,newdata=Vplot, type="response", se=TRUE)$se.fit
muOD <- predict(logis.glm.OD,newdata=Vplot,type="response",se=TRUE)$fit
seOD <- predict(logis.glm.OD,newdata=Vplot, type="response", se=TRUE)$se.fit

par(mfrow=c(1,1))
matplot(Vplot,cbind(mu1,mu2,muOD),type="l")
polygon(c(Vplot$V,rev(Vplot$V)),c(muOD+2*seOD,rev(muOD-2*seOD)),border=FALSE,
        col=gray(0.75))
polygon(c(Vplot$V,rev(Vplot$V)),c(mu1+2*se1,rev(mu1-2*se1)),border=FALSE,
        col=gray(0.5))
polygon(c(Vplot$V,rev(Vplot$V)),c(mu2+2*se2,rev(mu2-2*se2)),border=FALSE,
        col=gray(0.25))
matlines(Vplot,cbind(mu1,mu2,muOD),lty=1,lwd=2)
points(V,p,pch=19,col="blue")
####

dat = read.csv("clothing.csv", header = T)
str(dat)
plot(dat$clo)
dat$sex = as.factor(dat$sex)
dat$subjId = as.factor(dat$subjId)

attach(dat)

plot(density(dat$clo))
####START OF 1



###INVERSE GAUSSIAN 
modinv = glm(clo~(tOut*tInOp*sex), family = inverse.gaussian(link = "1/mu^2"), data = dat)
summary(modinv) 
anova(modinv,test="F")
modinv2 <- update(modinv,.~.-tInOp:sex:tOut)
anova(modinv2,test="F")
modinv3 <- update(modinv2,.~.-tInOp:sex)
anova(modinv3,test="F")
summary(modinv3)
plot(modinv3)
mod_invGauss_1mu2 <- modinv3


modinv = glm(clo~(tOut*tInOp*sex), family = inverse.gaussian(link = "inverse"), data = dat)
summary(modinv) 
anova(modinv,test="F")
modinv2 <- update(modinv,.~.-tInOp:sex:tOut)
anova(modinv2,test="F")
modinv3 <- update(modinv2,.~.-tInOp:sex)
anova(modinv3,test="F")
summary(modinv3) #934
plot(modinv3)
mod_invGauss_inverse <- modinv3

modinv_best = glm(clo~(tOut*tInOp*sex), family = inverse.gaussian(link = "identity"), data = dat)
summary(modinv_best) #950
anova(modinv_best,test="F") #BEST ONE FOR INVERSE!! <--- ?? identity best? 

mod_invGauss_identity <- modinv_best


modinv = glm(clo~(tOut*tInOp*sex), family = inverse.gaussian(link = "log"), data = dat)
summary(modinv) 
anova(modinv,test="F")
modinv2 <- update(modinv,.~.-tInOp:sex:tOut)
anova(modinv2,test="F")
modinv3 <- update(modinv2,.~.-tInOp:sex)
anova(modinv3,test="F")
summary(modinv3)
plot(modinv3)

mod_invGauss_log = modinv3


#what is this????
sinv <- simulateResiduals(modinv_best) #no fit.
plot(sinv) 


########

#GAUSSIAN BAD IDEA, NOT STRICTLY POSITIVE
modgauss = glm((clo)~(tOut*tInOp*sex), family = gaussian(link = "identity"), data = dat)
summary(modgauss)
modgauss$aic #958
par(mfrow=c(2,2))
plot(modgauss)  
anova(modgauss,test="F")
mod_gauss_identity <- modgauss

#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????
#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????
sga <- simulateResiduals(modgauss) #no good fit.
plot(sga)
#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????
#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????#what is this?????

modgausslog = glm((clo)~(tOut*tInOp*sex), family = gaussian(link = "log"), data = dat)
summary(modgausslog) #pretty bad fit
par(mfrow=c(2,2))
plot(modgausslog)  
anova(modgausslog,test="F")
modgausslog <- update(modgausslog,.~.-tInOp:sex:tOut)
anova(modgausslog,test="F")
modgausslog <- update(modgausslog,.~.-tOut:tInOp)
anova(modgausslog,test="F")
modgausslog <- update(modgausslog,.~.-tInOp:sex)
anova(modgausslog,test="F")
modgausslog_final<-modgausslog
summary(modgausslog_final) #952
plot(modgausslog_final)

mod_gauss_log <- modgausslog

modgaussinv = glm((clo)~(tOut*tInOp*sex), family = gaussian(link = "inverse"), data = dat)
summary(modgaussinv)
anova(modgaussinv,test="F")
modgaussinv <- update(modgaussinv,.~.-tInOp:sex:tOut)
anova(modgaussinv,test="F")
summary(modgaussinv) #946.  looks kind of trash with lots of non-significant parameters.
par(mfrow=c(2,2))
plot(modgaussinv)  
mod_gauss_inverse <- modgaussinv
#####

#Gamma functions:
mod1 = glm((clo)~(tOut*tInOp*sex), family = Gamma(link = "inverse"), data = dat)
summary(mod1)  #969
par(mfrow=c(2,2))
plot(mod1)                                                                     
drop1(mod1, test = "F")
mod2 <- update(mod1,.~.-tInOp:sex:tOut)
mod2$aic
drop1(mod2, test = "F")
mod3 <- update(mod2,.~.-sex:tInOp)
mod3$aic
drop1(mod3, test = "F")
mod4 <- update(mod3,.~.-tOut:tInOp)
mod4$aic
drop1(mod4, test = "F")
mod5 <- update(mod4,.~.-tInOp)
mod5$aic
drop1(mod5, test = "F")

summary(mod5) #968.9
par(mfrow=c(2,2))
plot(mod5)  

mod_gamma_inverse <- mod5
#####################

modl1 = glm(clo~tOut*tInOp*sex, family = Gamma(link = "log"), data = dat)
summary(modl1) #-978.85
par(mfrow=c(2,2))
plot(modl1)                                                                     
drop1(modl1, test = "F")
modl2 <- update(modl1,.~.-tInOp:tOut:sex)
drop1(modl2, test = "F")
modl3 <- update(modl2,.~.-tOut:tInOp)
drop1(modl3, test = "F")
modl4 <- update(modl3,.~.-tInOp:sex)
drop1(modl4, test = "F")
modl5 <- update(modl4,.~.-tInOp)
drop1(modl5, test = "F")
summary(modl5)   #aic = -980.51
par(mfrow=c(2,2))
plot(modl5)  

mod_gamma_log <- modl5
####

modi1 = glm(clo~(tOut*tInOp*sex), family = Gamma(link = "identity"), data = dat)
summary(modi1) #-987.36
par(mfrow=c(2,2))
plot(modi1)   
drop1(modi1, test = "F")
#No evidence to reduce, we use the complete model

summary(modi1)  #Best AIC
mod_gamma_identity <- modi1





plot_contour(mod_gauss_identity, mod_gauss_inverse, mod_gauss_log, title='Gaussian predictions')
plot_contour(mod_invGauss_log, mod_invGauss_inverse, mod_invGauss_1mu2, mod_invGauss_identity, title="Inverse Gaussian predictions")
plot_contour(mod_gamma_identity, mod_gamma_inverse, mod_gamma_log, title='Gamma predictions')


#CONCLUSION: gamma identity
mod_Gauss<-glm(clo~(tOut*tInOp*sex), family = gaussian(link = "identity"), data = dat)
mod_InvGauss<-glm(clo~(tOut*tInOp*sex), family = inverse.gaussian(link = "identity"), data = dat)
mod_Gamma<-glm(clo~(tOut*tInOp*sex), family = Gamma(link = "identity"), data = dat)

summary(mod_Gauss)$aic
summary(mod_InvGauss)$aic
summary(mod_Gamma)$aic

#CONCLUSION: gamma identity
mod_Gauss_sqr<-glm(clo~I(tOut^2)*sex+I(tInOp^2)*sex+sex*tOut*tInOp, family = gaussian(link = "identity"), data = dat)
mod_InvGauss_sqr<-glm(clo~((I(tOut^2)*I(tInOp^2)*sex)*tOut*tInOp), family = inverse.gaussian(link = "identity"), data = dat)
mod_Gamma_sqr<-glm(clo~((I(tOut^2)*I(tInOp^2)*sex)*tOut*tInOp), family = Gamma(link = "identity"), data = dat)

summary(mod_Gauss)
summary(mod_Gauss_sqr)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):sex)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):sex:tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):sex:tOut:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):tOut:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tInOp^2):sex:tOut:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):sex:tOut:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):sex:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tInOp^2):sex:tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):sex:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):sex:tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-sex:tOut:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-tOut:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tInOp^2):tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tInOp^2):sex:tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2):sex)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-sex:tOut)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tInOp^2):tInOp)
drop1(mod_Gauss_sqr,test="F")
mod_Gauss_sqr <- update(mod_Gauss_sqr,.~.-I(tOut^2):I(tInOp^2))
drop1(mod_Gauss_sqr,test="F")
summary(mod_Gauss_sqr)
plot(mod_Gauss_sqr)
plot(mod_Gauss)

plot_contour(mod_gamma_identity, mod_invGauss_identity, mod_gauss_identity, mod_Gauss_sqr, title = 'Compare best models', with_formula = T)


summary(mod_InvGauss)
summary(mod_InvGauss_sqr)

summary(mod_Gamma)
summary(mod_Gamma_sqr)

plot_contour(mod_InvGauss, mod_InvGauss_sqr, with_formula = T, title='invGaus sqr')
plot_contour(mod_Gamma, mod_Gamma_sqr, with_formula = T, title='Gamma sqr')

sga <- simulateResiduals(mod_Gauss)
plot(sga)

sga <- simulateResiduals(mod_InvGauss)
plot(sga)

sga <- simulateResiduals(mod_Gamma)
plot(sga)

saveFig <- TRUE
if(saveFig == TRUE){pdf("ResGauss.pdf", width = 10*0.8, height = 10*0.8)}
par(mfrow=c(2,2))
plot(mod_Gauss)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE
saveFig <- TRUE

if(saveFig == TRUE){pdf("ResInvGauss.pdf", width = 10*0.8, height = 10*0.8)}
par(mfrow=c(2,2))
plot(mod_InvGauss)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("ResGamma.pdf", width = 10*0.8, height = 10*0.8)}
par(mfrow=c(2,2))
plot(mod_Gamma)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


write.csv(signif(summary(mod_Gamma)$coefficients, digits = 3),"Mod_Gamma.csv")
write.csv(signif(summary(mod_Gauss)$coefficients, digits = 3),"Mod_Gauss.csv")
write.csv(signif(summary(mod_InvGauss)$coefficients, digits = 3),"Mod_InvGau.csv")

signif(summary(mod_Gamma)$coefficients, digits = 3)
signif(summary(mod_Gauss)$coefficients, digits = 3)
signif(summary(mod_InvGauss)$coefficients, digits = 3)

#predictions:
X_tOut<-seq(floor(min(tOut)),ceiling(max(tOut)),by=0.2)
X_tInOp<-seq(floor(min(tInOp))-2,ceiling(max(tInOp))+2,by=1)
  
(Newdata_male <- data.frame(tOut=X_tOut,tInOp=25,sex="male"))
(Newdata_female <- data.frame(tOut=X_tOut,tInOp=25,sex="female"))
Gauss_pred_male<-predict(mod_Gauss,newdata = Newdata_male)
Gauss_pred_female<-predict(mod_Gauss,newdata = Newdata_female)
lines(X_tOut,Gauss_pred_female,col="red")


par(mfrow=c(1,1))
plot(X_tOut,Gauss_pred_male,type="l",col="blue",ylim=c(0,1))

for (i in X_tInOp){
  (Newdata_male <- data.frame(tOut=X_tOut,tInOp=i,sex="male"))
  Gauss_pred_male<-predict(mod_Gauss,newdata = Newdata_male)
  lines(X_tOut,Gauss_pred_male,col="green")
  }

(Newdata_male <- data.frame(tOut=25,tInOp=X_tInOp,sex="male"))
(Newdata_female <- data.frame(tOut=25,tInOp=X_tInOp,sex="female"))
Gauss_pred_male<-predict(mod_Gauss,newdata = Newdata_male)
Gauss_pred_female<-predict(mod_Gauss,newdata = Newdata_female)
lines(X_tInOp,Gauss_pred_male,col="green")
lines(X_tInOp,Gauss_pred_female,col="black")


predict(mod_Gamma)
predict(mod_InvGauss)


####END OF 1

####START OF 2 OSCAR

data_fem<-filter(dat,dat$sex=="female")
data_mal<-filter(dat,dat$sex=="male")

#male data
mod_Gauss_mal<-glm(clo~(tOut*tInOp), family = gaussian(link = "identity"), data = data_mal)
mod_InvGauss_mal<-glm(clo~(tOut*tInOp), family = inverse.gaussian(link = "identity"), data = data_mal)
mod_Gamma_mal<-glm(clo~(tOut*tInOp), family = Gamma(link = "identity"), data = data_mal)


#female data
mod_Gauss_fem<-glm(clo~(tOut*tInOp), family = gaussian(link = "identity"), data = data_fem)
mod_InvGauss_fem<-glm(clo~(tOut*tInOp), family = inverse.gaussian(link = "identity"), data = data_fem)
mod_Gamma_fem<-glm(clo~(tOut*tInOp), family = Gamma(link = "identity"), data = data_fem)

summary(mod_Gauss_mal)$deviance
summary(mod_InvGauss_mal)$deviance
summary(mod_Gamma_mal)$deviance
summary(mod_Gauss_fem)$deviance
summary(mod_InvGauss_fem)$deviance
summary(mod_Gamma_fem)$deviance
####END 2 Oscar


####START OF 2
#RESIDUAL ANALYSIS
nfem = sum(dat$sex=="female")
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

hist( modi1$residuals, 10, probability = TRUE, col = 'lavender', main = 'residuals' )
X = model.matrix( modi1)
lev = hat(X)
p = modi1$rank;
n = length(clo);
plot( modi1$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
      pch = 16, col = 'black' )
abline( h = 2 * p/n, lty = 2, col = 'red' )
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( modi1$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )


# Cook Distance
Cdist = cooks.distance( modi1, type = "pearson")
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist
par( mfrow = c( 1, 2 ) )
plot( modi1$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( modi1$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],
        col = 'green', pch = 16 )
plot( modi1$fitted.values, lev, pch = 16, xlab = 'Fitted values',
      ylab = 'Leverages', main = 'Leverages' )

points( modi1$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'orange', pch = 16 )

id_to_keep = !( 1:n %in% watchout_ids_Cdist )
gl = glm(clo~(tOut*tInOp*sex), family = Gamma(link = "identity"), data = dat[ id_to_keep, ])
anova(mod_Gauss)

summary( gl ) 
#the AIC is better
par(mfrow=c(2,2))
plot(gl)      
####END OF 2

####START OF 3
par(mfrow=c(1,1))

data_mod <- data.frame(Predicted = predict(modi1),  # Create data for ggplot2
                       Observed = clo)


ggplot(data_mod,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              linewidth = 2)

pOut <- dat %>% 
  ggplot(aes(x = tOut, y = clo)) +
  geom_point(colour = "black") +
  geom_smooth(method = "glm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

pOut

pIn <- dat %>% 
  ggplot(aes(x = tInOp, y = clo)) +
  geom_point(colour = "black") +
  geom_smooth(method = "glm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

pIn

pSex <- dat %>% 
  ggplot(aes(x = sex, y = clo)) +
  geom_point(colour = "black") +
  geom_smooth(method = "glm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

pSex

confint(modi1)
predint = predict(modi1, dat, intervals="prediction", se.fit = TRUE ) 
####END OF 3

####START OF 4

mo = glm(clo~(tOut*tInOp) + subjId, family = Gamma(link = "identity"), data = dat)
summary(mo)

par(mfrow=c(2,2))
plot(mo)  

drop1(mo, test = "F")
mo2 <- update(mo,.~.-tOut:tInOp)
drop1(mo2, test = "F")
summary(mo2) #Better than before
par(mfrow=c(2,2))
plot(mo2)  
#the model with subject Id seems to be better, given that it has a lower AIC

####END OF 4



####START OF 5
resDev5 <- residuals(mo2,type="pearson")
categories <- unique(subjId) 
numberOfCategories <- length(categories)
DW = rep(0, numberOfCategories)



for (i in 1:numberOfCategories){
  acf(resDev5[subjId==categories[i]], plot=T)
  DW[i] = durbinWatsonTest(resDev5[subjId==categories[i]])
}
acf(resDev5[subjId==categories[1]], plot=T, lag.max = 4) #evidence of autocorrelation

par(mfrow=c(1,1))
plot(jitter(as.numeric(day[subjId==categories[2]]), amount=0.1), resDev5[subjId==categories[1]],
     xlab="Day", ylab="Deviance residuals", cex=0.6,
     axes=FALSE)
box()
axis(1,label=c("DAY 1", "DAY 2" , "DAY 3", "DAY 4" ),at=c(1,2,3,4))
axis(2)

durbinWatsonTest(mo2) #the residuals are autocorrelated


fit <- glmer(clo~(tOut*tInOp) + (1|subjId) + (day|subjId) ,family = Gamma(link = "identity"),
             data = dat)
summary(fit)
plot(fit)


####END OF 5
####START OF 6 IGNORE IT
modmw = glm(clo~sex, family = Gamma(link = "identity"), data = dat)
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

w = as.numeric(sex=="male") + as.numeric(sex=="female")/coefficients[1]
gw = glm(clo~(tOut + sex + tInOp + tOut:sex + tOut:tInOp + tInOp:sex), family = Gamma(link = "identity"), data = dat, weights = w )
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
detach(dat)

####
rm(list=ls())
swim = read.table("earinfect.txt", header = T)
swim$swimmer = as.factor(swim$swimmer)
swim$location = as.factor(swim$location)
swim$age = as.factor(swim$age)
swim$sex = as.factor(swim$sex)
attach(swim)

#We decided to add only until the second order terms because the higher order interactions 
#would have generated some NA's


mod = glm(infections~(age+sex + swimmer + location) + age:sex + swimmer:location + 
            age:swimmer + age:location + sex:location + sex:swimmer, offset = log(persons), family = poisson(link = "log"))
summary(mod)
par(mfrow=c(2,2))
plot(mod)  

1 - pchisq(2.5421, 9) #Really good.


mod_rev1 = glm(infections~(age+sex + swimmer + location) + age:sex + swimmer:location + 
                 age:swimmer + age:location + sex:location + sex:swimmer, offset = log(persons), family = poisson(link = "sqrt"))
summary(mod_rev1)
par(mfrow=c(2,2))
plot(mod)
1 - pchisq(1.8822, 9)

mod_rev2 = glm(infections~(age+sex + swimmer + location) + age:sex + swimmer:location + 
                 age:swimmer + age:location + sex:location + sex:swimmer, offset = log(persons), family = poisson(link = "identity"))
summary(mod_rev2)
par(mfrow=c(2,2))
plot(mod)
1 - pchisq(5.5353, 9) #even worse, the best is the second one(aic). All of three have good fit though



#When reducing, we look if the AIC is better
reduced_model <- step(mod_rev1)
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

