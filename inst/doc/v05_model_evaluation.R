## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(RprobitB)
set.seed(1)

## ----fit----------------------------------------------------------------------
data("TravelMode", package = "AER")
TravelMode$choice <- TravelMode$choice == "yes"
TravelMode$vcost <- TravelMode$vcost / 1.6196
TravelMode$income <- TravelMode$income / 1.6196
full_model <- fit(
  choice ~ wait + vcost + travel | income + size,
  data = TravelMode,
  format = "long",
  column_decider = "individual",
  column_alternative = "mode",
  iterations = 6000,
  warmup = 3000,
  thin = 30,
  chains = 2,
  progress = FALSE
)
reduced_model <- update(full_model, . ~ wait + vcost + travel | 0)

## ----loglik-------------------------------------------------------------------
logLik(full_model)
logLik(reduced_model)

## ----waic---------------------------------------------------------------------
WAIC(full_model)
WAIC(reduced_model)

## ----loo----------------------------------------------------------------------
loo_full <- loo(full_model)
loo_reduced <- loo(reduced_model)
loo_full

## ----loo-plot-----------------------------------------------------------------
plot(loo_full)

## ----compare------------------------------------------------------------------
loo::loo_compare(loo_full, loo_reduced)

## ----bf-----------------------------------------------------------------------
set.seed(1)
bayes_factor(full_model, reduced_model, log = TRUE)

## ----train--------------------------------------------------------------------
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60
train_small <- Train[Train$id %in% unique(Train$id)[1:100], ]
train_fixed <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = train_small,
  column_decider = "id",
  column_occasion = "choiceid",
  iterations = 2000,
  warmup = 1000,
  thin = 20,
  chains = 2,
  progress = FALSE
)
train_random <- update(train_fixed, random_effects = c(price = "n"))
loo_fixed <- loo(train_fixed, progress = FALSE)
loo_random <- loo(train_random, progress = FALSE)
loo::loo_compare(loo_fixed, loo_random)

## ----ordered-comparison-------------------------------------------------------
data("survey", package = "MASS")
smoking_full <- fit(
  Smoke ~ Age + Exer | 0,
  data = survey,
  alternatives = c("Never", "Occas", "Regul", "Heavy"),
  choice_type = "ordered",
  column_decider = NULL,
  chains = 1
)
smoking_age <- update(smoking_full, . ~ Age | 0)
loo::loo_compare(
  loo(smoking_full, progress = FALSE), loo(smoking_age, progress = FALSE)
)

