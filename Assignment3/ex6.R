library(TMB)
library(dplyr)
library(kableExtra)
load('data.rds')

save_fig = function(figure = last_plot(), name = NULL, show_plot=T) {
  if (is.null(name)) {
    fig <- enquo(figure)
    name = as_label(fig)
  }
  ggsave(plot = figure, file = paste0(name, '.pdf'), path = 'figs', width=10*0.8, height = 6*0.8)
  return(figure)
}

save_table = function(table, name) {
  cat(
    kbl(table, 'latex', booktabs=T, digits=2, escape=F, row.names = F, caption=name), 
    file=paste('tables/', name, '.tex', sep="")
  )
}

sexSubj <- cloth |> 
  dplyr::select(sex, subjId) |> 
  unique() |> 
  pull(sex)

cloth |> 
  dplyr::select(subjId, subDay) |> 
  unique() |> 
  pull(subjId) ->
  subj_from_subDay

nlevels(cloth$subDay)

data <- list(
  clo = cloth$clo,
  subj = cloth$subjId,
  subDay = cloth$subDay,
  subj_from_subDay = subj_from_subDay,
  sex = sexSubj
)

compile("ex6.cpp")
dyn.load(dynlib("ex6"))


parameters <- list(u = rep(.5, nlevels(cloth$subjId)),
                   v = rep(.5, nlevels(cloth$subDay)),
                   gamma = rep(.5, nlevels(cloth$subjId)),
                mu = .5,
                   beta = 1,
                   alpha = .1,
                   sigma_u= .1,
                   sigma_v= .1,
                   sigma_G= .1,
                   sigma = .1
                   )

obj <- MakeADFun(data = data,
                 parameters = parameters,
                 random = c("u","v", "gamma"),
                 DLL = "ex6",
                 hessian = TRUE)

opt <- optim(obj$par, obj$fn, gr=obj$gr, method="BFGS")
r <- sdreport(obj)
names(opt$par)
opt$value

r$par.fixed


result <- as.data.frame(list(
  Parameter=names(opt$par),
  Estimate=opt$par,
  Sd=sqrt(diag(r$cov.fixed))
))
gamma <- exp(-r$value)
plot_df <- data.frame(list(subjId = levels(cloth$subjId), gamma = gamma))
plot_df |> 
  mutate(
    subjId = fct_inseq(subjId)
  ) |> 
  ggplot(aes(subjId, gamma)) +
  geom_col() +
  labs(
    y="Shrinking Coefficient"
  ) -> gamma_TMB
save_fig(gamma_TMB)  


hist(gamma, probability = TRUE, xlab = 'Gamma')
plot(f(gamma), type ='l', col="green", lwd=3, xlab="SubjId", ylab="Shrinking Coefficient (exp(-gamma)")

save_table(result, 'ex6_par')
