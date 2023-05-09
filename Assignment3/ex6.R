library(TMB)
library(dplyr)
load('data.rds')

sexSubj <- cloth |> 
  select(sex, subjId) |> 
  unique() |> 
  pull(sex)



cloth |> 
  select(subjId, subDay) |> 
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
                 DLL = "ex6"
)

opt <- nlminb(obj$par, obj$fn, gradient=obj$gr, lower = c(0, -Inf, rep(1e-4, 5)))
opt$par

sdreport(obj)
