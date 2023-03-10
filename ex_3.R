require(tidyverse)
require(car)
load('data.rds')
load('model_ex2.rds')

mod_1rev = lm(log(DIOX) ~ -1 + PLANT + TIME + LAB +  O2COR + NEFFEKT + QRAT, data = data)
summary(mod_1rev)
par(mfrow=c(2,2))
plot(mod_1rev)
anova(mod_1rev) ## Type I
Anova(mod_1rev,type="II")
drop1(mod_1rev,test="F")

mod_2rev <- update(mod_1rev,.~.-QRAT)
summary(mod_2rev)
plot(mod_2rev)
Anova(mod_2rev, type='II')

mod2_pred <- predict(mod_2rev, se.fit = T, interval = 'conf')

Model = factor("Model2")
Fit = mod2_pred$fit[,'fit']
Upper = mod2_pred$fit[,'upr']
Lower = mod2_pred$fit[,'lwr']
model2 <- data.frame(Model, Fit, Upper, Lower)



mod2_o2cor <- cbind(data, model2) |> 
  ggplot(aes(x=O2COR, y=DIOX)) +
  geom_point() +
  geom_ribbon(aes(ymin = exp(Lower), ymax=exp(Upper)), alpha=.2) +
  geom_line(aes(y=exp(Fit))) +
  scale_y_continuous(trans='log10', ) +
  facet_wrap(vars(interaction(PLANT, TIME, LAB, sep=", ")), ncol=4) +
  labs(
    title = "Model with O2COR",
    y = NULL
  )
mod2_o2cor

mod2_neffekt <- data |> 
  ggplot(aes(x=NEFFEKT, y=DIOX)) +
  geom_point() +
  geom_ribbon(aes(ymin = exp(Lower), ymax=exp(Upper)), alpha=.2) +
  geom_line(aes(y=exp(Fit))) +
  scale_y_continuous(trans='log10' ) +
  facet_wrap(vars(interaction(PLANT, TIME, LAB, sep=", ")), ncol = 4) +
  labs(
    title = "Model with NEFFEKT"
  )
mod2_neffekt
mod2_o2cor
mod2_visual <- mod2_neffekt | mod2_o2cor
save_fig('mod2_visual', mod2_visual)

