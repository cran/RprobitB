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

## ----elec-model, message = FALSE----------------------------------------------
data("Electricity", package = "mlogit")
Electricity <- as_cov_names(
  Electricity, c("pf", "cl", "loc", "wk", "tod", "seas"), 1:4
)
data <- prepare_data(
  form = choice ~ pf + cl + loc + wk + tod + seas | 0,
  choice_data = Electricity,
  re = c("cl", "loc", "wk", "tod", "seas")
)
model_elec <- fit_model(data, scale = "pf := -1", R = 1000)

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
N <- c(90, 70, 40)
(b_true <- matrix(replicate(C_true, rnorm(P_r)), nrow = P_r, ncol = C_true))
(Omega_true <- matrix(replicate(C_true, oeli::rwishart(P_r + 1, 0.1 * diag(P_r)), simplify = TRUE),
  nrow = P_r * P_r, ncol = C_true
))
beta <- c()
for (c in 1:C_true) for (n in 1:N[c]) {
  beta_n <- oeli::rmvnorm(
    mean = b_true[, c], 
    Sigma = matrix(Omega_true[, c, drop = F], ncol = P_r)
  )
  beta <- cbind(beta, beta_n)
}

## ----dirichlet-prior----------------------------------------------------------
delta <- 0.1
mu_b_0 <- numeric(P_r)
Sigma_b_0 <- diag(P_r)
n_Omega_0 <- P_r + 2
V_Omega_0 <- diag(P_r)

## ----dirichlet-inits----------------------------------------------------------
z <- rep(1, ncol(beta))
C <- 1
b <- matrix(0, nrow = P_r, ncol = C)
Omega <- matrix(rep(diag(P_r), C), nrow = P_r * P_r, ncol = C)

## ----dirichlet-process-app----------------------------------------------------
set.seed(1)
R <- 500
C_seq <- numeric(R)
for (r in seq_len(R)) {
  dp <- update_classes_dp(
    beta = beta, z = z, b = b, Omega = Omega, 
    delta = delta, mu_b_0 = mu_b_0, Sigma_b_0 = Sigma_b_0, 
    n_Omega_0 = n_Omega_0, V_Omega_0 = V_Omega_0, 
    identify_classes = TRUE, Cmax = 10
  )
  z <- dp$z
  b <- dp$b
  Omega <- dp$Omega
  C_seq[r] <- dp$C
}
table(C_seq)

## ----dirichlet-example-plot, fig.dim = c(16,8)--------------------------------
par(mfrow = c(1, 2))
plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]), pch = 19)
plot_class_allocation(beta, z, b, Omega, r = R, perc = 0.95)

