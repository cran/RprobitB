## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(RprobitB)
set.seed(1)

## ----mixing-------------------------------------------------------------------
mixing <- fit(
  choice ~ price + time + comfort | 0,
  random_effects = c(price = "ln-", time = "cn", comfort = "cn"),
  n_deciders = 100,
  n_occasions = 10,
  dgp_parameters = list(
    beta = c(price = -1, time = -0.8, comfort = 0.5),
    Omega = rbind(c(0.25, 0, 0), c(0, 0.4, 0.2), c(0, 0.2, 0.3))
  ),
  iterations = 4000,
  warmup = 2000,
  thin = 2,
  chains = 1,
  save_individual_draws = TRUE
)
summary(mixing)

## ----mixing-plot, fig.height = 3----------------------------------------------
plot(mixing, type = "mixture")

## ----mixing-interpret---------------------------------------------------------
interpret(mixing, reference = "price")

## ----individual---------------------------------------------------------------
individual <- coef(mixing, level = "individual")
head(individual)
price <- -exp(individual[, "price"])
hist(
  price,
  breaks = 30, col = "grey85", border = "white",
  main = "", xlab = "price coefficient of a decider"
)

## ----electricity--------------------------------------------------------------
data("Electricity", package = "mlogit")
names(Electricity) <- sub("([a-z]+)([1-4])$", "\\1_\\2", names(Electricity))
Electricity$occasion <- ave(Electricity$id, Electricity$id, FUN = seq_along)
households <- Electricity[Electricity$id %in% unique(Electricity$id)[1:100], ]
electricity <- fit(
  choice ~ pf + cl + loc + wk + tod + seas | 0,
  data = households,
  random_effects = c("cl", "loc"),
  column_decider = "id",
  column_occasion = "occasion",
  scale = c(pf = -1),
  iterations = 4000,
  warmup = 2000,
  thin = 2,
  chains = 1,
  save_individual_draws = TRUE,
  progress = FALSE
)
summary(electricity)

## ----electricity-interpret----------------------------------------------------
interpret(electricity, effects = c("cl", "loc", "wk"))

## ----electricity-plot, fig.height = 3-----------------------------------------
plot(electricity, type = "mixture") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
  ggplot2::labs(x = "willingness to pay in cents per kWh")
draws <- posterior::as_draws_matrix(electricity)
longer <- mean(pnorm(
  0, draws[, "mu[cl]"], sqrt(draws[, "Omega[cl,cl]"]), lower.tail = FALSE
))
distant <- mean(pnorm(0, draws[, "mu[loc]"], sqrt(draws[, "Omega[loc,loc]"])))

## ----classes------------------------------------------------------------------
mixture <- fit(
  choice ~ x | 0,
  random_effects = "x",
  latent_class_effects = "x",
  classes = 2,
  n_deciders = 100,
  n_occasions = 8,
  save_individual_draws = TRUE,
  dgp_parameters = list(
    beta = list(c(x = -1), c(x = 2)),
    Omega = list(matrix(0.2), matrix(0.2)),
    weights = c(0.6, 0.4)
  ),
  iterations = 1500,
  chains = 2,
  progress = FALSE
)
summary(mixture, variables = c(
  "weight[1]", "weight[2]", "mu[x,1]", "mu[x,2]",
  "Omega[x,x,1]", "Omega[x,x,2]"
))

## ----classes-plot, fig.height = 3.2-------------------------------------------
plot(mixture, type = "mixture")

## ----class-diagnostics--------------------------------------------------------
class_diagnostics <- latent_class_diagnostics(mixture)
class_diagnostics$occupancy
class_diagnostics$membership[1:6, ]
class_diagnostics$co_clustering[1:6, 1:6]

## ----train-classes------------------------------------------------------------
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60
train_small <- Train[Train$id %in% unique(Train$id)[1:60], ]
train_classes <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = train_small,
  latent_class_effects = "time",
  classes = 2,
  column_decider = "id",
  column_occasion = "choiceid",
  scale = c(price = -1),
  iterations = 1500,
  chains = 2,
  progress = FALSE
)
summary(train_classes, variables = c(
  "weight[1]", "weight[2]", "beta[time,1]", "beta[time,2]"
))

## ----train-classes-interpret--------------------------------------------------
time_by_class <- interpret(train_classes, effects = "time")
time_by_class

## ----recover-classes----------------------------------------------------------
recover_classes <- function(x) {
  variables <- c("weight[1]", "weight[2]", "mu[x,1]", "mu[x,2]")
  occupancy <- latent_class_diagnostics(x)$occupancy
  data.frame(
    variable = c("n_classes", variables),
    dgp = c(2, 0.6, 0.4, -1, 2),
    estimate = round(c(
      occupancy$n_classes[which.max(occupancy$probability)],
      coef(x)[variables]
    ), 2),
    row.names = NULL
  )
}
recover_classes(mixture)

## ----weight-based-------------------------------------------------------------
weight_based <- update(mixture, class_update = "weight_based")
recover_classes(weight_based)

## ----sparse-------------------------------------------------------------------
sparse <- update(mixture, classes = 6, class_update = "sparse")
summary(sparse)
latent_class_diagnostics(sparse)$occupancy
recover_classes(sparse)

## ----dirichlet----------------------------------------------------------------
dynamic <- update(
  mixture, class_update = "dirichlet_process", max_classes = 15,
  iterations = 1000
)
summary(dynamic)
latent_class_diagnostics(dynamic)$occupancy
recover_classes(dynamic)

## ----refits-plot, fig.height = 3----------------------------------------------
bayesplot::bayesplot_grid(
  plot(weight_based, type = "mixture"),
  plot(sparse, type = "mixture"),
  plot(dynamic, type = "mixture"),
  titles = c("weight-based", "sparse", "Dirichlet process"),
  grid_args = list(nrow = 1)
)

## ----ranked-random------------------------------------------------------------
ranked_random <- fit(
  rank ~ x | 0,
  choice_type = "ranked",
  random_effects = "x",
  n_deciders = 100,
  n_occasions = 5,
  n_alternatives = 3,
  dgp_parameters = list(beta = c(x = -1), Omega = matrix(0.3)),
  iterations = 2000,
  chains = 1,
  progress = FALSE
)
summary(ranked_random)

