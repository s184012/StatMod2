load('model_ex2.rds')
load('data.rds')

save_fig = function(name, figure = last_plot()) {
  ggsave(plot = figure, file = paste(name, '.pdf', sep=''), path = 'figs', width=10*0.8, height = 6*0.8)
}

mod1_pred = predict(fit1, se=T, interval='prediction')
Model = factor("Model1")
Fit = mod1_pred$fit[,'fit']
Upper = mod1_pred$fit[,'upr']
Lower = mod1_pred$fit[,'lwr']

model1 <- data.frame(Model, Fit, Upper, Lower)

model1_visualization <- cbind(data, model1) |> 
  ggplot(aes(x = interaction(LOAD, OXYGEN, sep=', '), y=DIOX)) +
  geom_point() +
  geom_pointrange(aes(y = exp(Fit), ymin = exp(Lower), ymax=exp(Upper)), color='red') +
  facet_wrap(vars(interaction(PLANT, TIME, LAB, sep=', ')), scales = 'free', ncol = 4) +
  scale_y_continuous(trans='log10') +
  labs(
    title = "Visualitation of Model 1",
    x = "LOAD, OXYGEN"
  )
model1_visualization
save_fig('model1_visualization', model1_visualization)
    

