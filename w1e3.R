data <- c(13, 5, 28, 28, 15, 4, 13, 4, 10, 17, 11, 13, 12, 17, 3)

library(stats)
nll <- function(par) {
  - sum(dnbinom(data, size = par[1], prob = par[2], log = T))
}

fit <- optim(par = c(2, .5), fn=nll, lower = 0, upper=c(Inf, 1), hessian = T, method="L-BFGS-B")


n <- fit$par[1]
p <- fit$par[2]

sd = sqrt(diag(solve(fit$hessian)))

cbind(fit$par, sd)

t_mean = n*(1-p)/p


x = seq(0, 1, length.out = 100)
y = seq(1, 5, length.out = 100)
grid = expand.grid(x, y)

plot_nll <- function(x, y) {
  par = c(x, y)
  nll(par)
}

