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
rm(list=ls())

save_fig = function(name, figure = last_plot()) {
  ggsave(plot = figure, file = paste(name, '.pdf', sep=''), path = 'figs', width=10*0.8, height = 6*0.8)
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

cloth = read.csv("clothing.csv", header = T)
cloth$sex = as.factor(cloth$sex)
cloth$subjId = as.factor(cloth$subjId)

attach(cloth)

plot(density(cloth$clo))
####START OF 1

###INVERSE GAUSSIAN 
modinv = glm(clo~(tOut*tInOp*sex), family = inverse.gaussian(link = "1/mu^2"), data = cloth)
summary(modinv) 
par(mfrow=c(2,2))
plot(modinv)  
sinv <- simulateResiduals(modinv) #no fit.
plot(sinv)
drop1(modinv, test = "F")
modinv2 <- update(modinv,.~.-tInOp:sex:tOut)
drop1(modinv2, test = "F")
modinv3 <- update(modinv2,.~.-tInOp:sex)
drop1(modinv3, test = "F")


summary(modinv3)
par(mfrow=c(2,2))
plot(modinv3)

########

#GAUSSIAN BAD IDEA, NOT STRICTLY POSITIVE
modgauss = glm((clo)~(tOut*tInOp*sex), family = gaussian(link = "identity"), data = cloth)
summary(modgauss)
par(mfrow=c(2,2))
plot(modgauss)  
sga <- simulateResiduals(modgauss) #no good fit.
plot(sga)
modgausslog = glm((clo)~(tOut*tInOp*sex), family = gaussian(link = "log"), data = cloth)
summary(modgausslog)
par(mfrow=c(2,2))
plot(modgausslog)  

modgaussinv = glm((clo)~(tOut*tInOp*sex), family = gaussian(link = "inverse"), data = cloth)
summary(modgaussinv)
par(mfrow=c(2,2))
plot(modgaussinv)  
#####

mod1 = glm((clo)~(tOut*tInOp*sex), family = Gamma(link = "inverse"), data = cloth)
summary(mod1) 
par(mfrow=c(2,2))
plot(mod1)                                                                     
drop1(mod1, test = "F")
mod2 <- update(mod1,.~.-tInOp:sex:tOut)
drop1(mod2, test = "F")
mod3 <- update(mod2,.~.-sex:tInOp)
drop1(mod3, test = "F")
mod4 <- update(mod3,.~.-tOut:tInOp)
drop1(mod4, test = "F")
mod5 <- update(mod4,.~.-tInOp)
drop1(mod5, test = "F")

summary(mod5)
par(mfrow=c(2,2))
plot(mod5)  
#####################

modl1 = glm(clo~tOut*tInOp*sex, family = Gamma(link = "log"), data = cloth)
summary(modl1)
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
summary(modl5)  
par(mfrow=c(2,2))
plot(modl5)  

####

modi1 = glm(clo~(tOut*tInOp*sex), family = Gamma(link = "identity"), data = cloth)
summary(modi1)
par(mfrow=c(2,2))
plot(modi1)   
drop1(modi1, test = "F")
#No evidence to reduce, we use the complete model

summary(modi1)  #Best AIC


get_contour_predict <- function(model, title) {
  prediction_tOut = seq(min(cloth$tOut), max(cloth$tOut), length.out=50)
  prediction_tInOp = seq(min(cloth$tInOp), max(cloth$tInOp), length.out=50)
  
  pred_df <- expand_grid(
    tOut = prediction_tOut,
    tInOp = prediction_tInOp,
    sex = cloth$sex
  )
  
  pred_out <- predict(model, newdata=pred_df, type="response")
  pred_out <- cbind(pred_df, pred_out, model=title)
}

plot_contour <- function(model, title) {
  prediction_tOut = seq(min(cloth$tOut), max(cloth$tOut), length.out=50)
  prediction_tInOp = seq(min(cloth$tInOp), max(cloth$tInOp), length.out=50)
  
  pred_df <- expand_grid(
    tOut = prediction_tOut,
    tInOp = prediction_tInOp,
    sex = cloth$sex
  )
  
  pred_out <- predict(model, newdata=pred_df, type="response")
  pred_out <- cbind(pred_df, pred_out)
  
  pred_out |> 
    ggplot(aes(x=tInOp, y=tOut, z=pred_out)) +
    geom_contour_filled(breaks=seq(0, 1, by=0.1), ) +
    facet_wrap(vars(sex)) +
    labs(
      title = title
    ) +
    scale_color_steps(breaks=seq(0, 1, by=0.1), nice.breaks = F)
}


gamma_identity <- get_contour_predict(modi1, "Gamma, identity")
inverse <- get_contour_predict(mod5, "Gamma, inverse")
log <- get_contour_predict(modl5, "Gamma, log")

link_plot_df <- rbind(inverse, log, gamma_identity)
link_comparison <- link_plot_df |> 
  ggplot(aes(x=tInOp, y=tOut, z=pred_out)) +
  geom_contour_filled(breaks=seq(0, 1, by=0.1)) +
  facet_grid(cols = vars(sex), rows=vars(model)) +
  labs(
    title = "Link Comparison"
  )
save_fig("link_comparison", link_comparison)

inv_gauss <- get_contour_predict(modinv3, "Inverse Gaussian, 1/mu^2)")
plot_df <- rbind(gamma_identity, inv_gauss)

gamma_vs_inv_gauss <- plot_df |> 
  ggplot(aes(x=tInOp, y=tOut, z=pred_out)) +
  geom_contour_filled(breaks=append(seq(0, .9, by=0.1), seq(1, 5, by=1))) +
  facet_grid(cols = vars(sex), rows=vars(model)) +
  labs(
    title = "Gamma vs Inverese Gaussian"
  )



save_fig("gamma_vs_inv_gauss", gamma_vs_inv_gauss)





#CONCLUSION: gamma identity
####END OF 1

####START OF 2

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
gl = glm(clo~(tOut*tInOp*sex), family = Gamma(link = "identity"), data = cloth[ id_to_keep, ])


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

pOut <- cloth %>% 
  ggplot(aes(x = tOut, y = clo)) +
  geom_point(colour = "black") +
  geom_smooth(method = "glm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

pOut

pIn <- cloth %>% 
  ggplot(aes(x = tInOp, y = clo)) +
  geom_point(colour = "black") +
  geom_smooth(method = "glm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

pIn

pSex <- cloth %>% 
  ggplot(aes(x = sex, y = clo)) +
  geom_point(colour = "black") +
  geom_smooth(method = "glm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

pSex

confint(modi1)
predint = predict(modi1, cloth, intervals="prediction", se.fit = TRUE ) 
####END OF 3

####START OF 4

mo = glm(clo~(tOut*tInOp) + subjId, family = Gamma(link = "identity"), data = cloth)
summary(mo)

par(mfrow=c(2,2))
plot(mo)  

drop1(mo, test = "F")
mo2 <- update(mo,.~.-tOut:tInOp)
drop1(mo2, test = "F")
summary(mo2) #Better than before
tOutpar(mfrow=c(2,2))
plot(mo2)  
#the model with subject Id seems to be better, given that it has a lower AIC

####END OF 4



####START OF 5
library(tidymodels)

resDev5 <- residuals(mo2,type="pearson")

res_frame <- tibble(subjId = cloth$subjId, day = cloth$day, residuals=resDev5)

res <- res_frame |> 
  nest(data=residuals) |> 
  mutate(
    acf_calc = map(data, ~acf(.x$residuals, lag.max=5, plot=F)),
    tidied = map(acf_calc, tidy)
  ) |> 
  unnest(cols=tidied) |> 
  select(-data, -acf_calc) |> 
  filter(lag!=0) |> 
  group_by(subjId, day) |> 
  mutate(
    n_obs = n(),
    ci = qnorm((1 + .95)/2)/sqrt(n_obs)
  ) |> 
  mutate(
    is_significant = abs(acf) > ci
  )

auto_corr_plot <- res |> 
  ggplot(aes(x=interaction(subjId, day), y = acf, color=day)) +
  geom_point() +
  geom_hline(yintercept = min(res$ci), linetype='dashed') +
  geom_hline(yintercept = -min(res$ci), linetype='dashed') +
  facet_wrap(vars(lag)) +
  labs(
    title = "Auto correlation for each subject within each day, lag 1-5",
    color = "Day"
  ) +
  scale_x_discrete(name="Subject on a given day", breaks = NULL) +
  scale_y_continuous(
    name="Auto correlation",
    breaks=c(-1, round(-min(res$ci), 2),  0, round(min(res$ci),2), 1), 
    limits = c(-1, 1), 
    minor_breaks = NULL
  )

save_fig("auto_corr_plot", auto_corr_plot)

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
             data = cloth)
summary(fit)
plot(fit)


####END OF 5
####START OF 6

nll <- function(par){
  is_female <- cloth$sex == 'female'
  weights <- is_female * par[1] + (!is_female)*par[2]
  -sum(dgamma(cloth$clo, shape = weights, scale = par[3], log = T))
}
opt <- optim(c(1,1,1), nll, method = "L-BFGS-B", lower=c(0.001, 0.001, 0.001, 0.001))

opt$par


par(mfrow=c(1,1))

xs = seq(0,2,length.out=50)
plot(xs, dgamma(xs, shape = 11.50164779, scale=0.04592705), type='l', col='blue')
lines(xs, dgamma(xs, shape = 11.50164779 + 0.97329550, scale=0.04592705), col='red')


profile_likelihood <- function(lambda, data, sex) {
  design_mat <- model.matrix(~ subjId + tOut + tInOp, data=cloth)
  n_par <- 2 #ncol(design_mat) + 2
  nll <- function(par){
    is_female <- data$sex == sex
    weights <- is_female * lambda + (!is_female)*par[1]
    -sum(dgamma(data$clo, shape = weights, scale = par[2], log = T))
  }
  optim(rep(1, n_par), nll, method = "L-BFGS-B", lower=c(rep(0.001, n_par)))$value
}

load("data.rds")
profile_2d <- function(grid){
  nll <- function(par){
    is_female <- cloth$sex == "female"
    weights <- is_female * grid[1] + (!is_female)*grid[2]
    -sum(dgamma(cloth$clo, shape = weights, scale = par, log = T))
  }
  optimize(nll, c(0.0001, 10))$objective
  
}

# male_lambda <- seq(10, 13, length.out=50)
# female_lambda <- seq(11, 14, length.out=50)
# grid = expand.grid(female_lambda = female_lambda, male_lambda=male_lambda)
# profile_values <- apply(grid, 1, profile_2d)
# grid$likelihood <- nll_to_L(profile_values)
# # contourplot(likelihood ~ female_lambda * male_lambda, grid, )
lambdas = seq(9, 15, length.ou=100)
nll_to_L <- \(nll) exp(-nll) / max(exp(-nll))
nll_female <- sapply(lambdas, profile_likelihood, cloth, 'female')
nll_male <- sapply(lambdas, profile_likelihood, cloth, 'male')

par(mfrow=c(1,1))
plot(lambdas, nll_to_L(nll_female), type='l', col='red', ylab="", xlab="")
lines(lambdas, nll_to_L(nll_male), col='blue')
abline(h=.85, lty='dashed')
title(ylab = 'Scaled Likelihood', xlab="Dispersion Parameter")
legend(9, 1, c("Female", "Male", "95% Conf."), col = c("red", "blue", "black"), lty=c("solid", "solid", "dashed"))


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

gamma(age + sex + swimmer, 1 + sex )

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
