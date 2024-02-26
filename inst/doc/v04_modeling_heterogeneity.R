## ----label = setup, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "img/",
  fig.align = "center",
  fig.dim = c(8, 6),
  out.width = "75%"
)
library("RprobitB")
options("RprobitB_progress" = FALSE)

## ----message = FALSE----------------------------------------------------------
data("Electricity", package = "mlogit")
Electricity <- as_cov_names(Electricity, c("pf", "cl", "loc", "wk", "tod", "seas"), 1:4)
data <- prepare_data(
  form = choice ~ pf + cl + loc + wk + tod + seas | 0,
  choice_data = Electricity,
  re = c("cl", "loc", "wk", "tod", "seas")
)
model_elec <- fit_model(data, R = 1000, scale = "pf := -1")

## ----coef-model-elec----------------------------------------------------------
coef(model_elec)

## ----compute-mixdistr-share---------------------------------------------------
cl_mu <- coef(model_elec)["cl", "mean"]
cl_sd <- sqrt(coef(model_elec)["cl", "var"])
pnorm(cl_mu / cl_sd)

## ----cov-mixdistr-------------------------------------------------------------
cov_mix(model_elec, cor = TRUE)

## ----plot-mixture-model-elec, fig.dim = c(8,8)--------------------------------
plot(model_elec, type = "mixture")

## ----sim-dirichlet------------------------------------------------------------
set.seed(1)
P_r <- 2
C_true <- 3
N <- c(100, 70, 30)
(b_true <- matrix(replicate(C_true, rnorm(P_r)), nrow = P_r, ncol = C_true))
(Omega_true <- matrix(replicate(C_true, rwishart(P_r + 1, 0.1 * diag(P_r))$W, simplify = TRUE),
  nrow = P_r * P_r, ncol = C_true
))
beta <- c()
for (c in 1:C_true) {
  for (n in 1:N[c]) {
    beta <- cbind(beta, rmvnorm(mu = b_true[, c, drop = F], Sigma = matrix(Omega_true[, c, drop = F], ncol = P_r)))
  }
}
z_true <- rep(1:3, times = N)

## ----dirichlet-prior----------------------------------------------------------
delta <- 0.1
xi <- numeric(P_r)
D <- diag(P_r)
nu <- P_r + 2
Theta <- diag(P_r)

## ----dirichlet-inits----------------------------------------------------------
z <- rep(1, ncol(beta))
C <- 1
b <- matrix(0, nrow = P_r, ncol = C)
Omega <- matrix(rep(diag(P_r), C), nrow = P_r * P_r, ncol = C)

## ----dirichlet-process-app----------------------------------------------------
for (r in 1:100) {
  dp <- RprobitB:::update_classes_dp(
    Cmax = 10, beta, z, b, Omega, delta, xi, D, nu, Theta, s_desc = TRUE
  )
  z <- dp$z
  b <- dp$b
  Omega <- dp$Omega
}

## ----dirichlet-example-plot, fig.dim = c(16,8)--------------------------------
par(mfrow = c(1, 2))
plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]), pch = 19)
RprobitB:::plot_class_allocation(beta, z, b, Omega, r = 100, perc = 0.95)

