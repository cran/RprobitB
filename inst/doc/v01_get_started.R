## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4,
  fig.align = "center"
)

## ----data---------------------------------------------------------------------
library(RprobitB)
set.seed(1)
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60
head(Train)

## ----formula------------------------------------------------------------------
formula <- choice ~ price + time + change + factor(comfort) | 0

## ----fit----------------------------------------------------------------------
model <- fit(
  formula = formula,
  data = Train,
  column_decider = "id",
  column_occasion = "choiceid",
  iterations = 2000,
  warmup = 1000,
  chains = 2,
  progress = FALSE
)
model

## ----summary------------------------------------------------------------------
summary(model)

## ----interpret----------------------------------------------------------------
compensation <- interpret(model, reference = "price")
compensation

## ----summary-statistics-------------------------------------------------------
summary(
  model,
  statistics = c("mean", "mcse_mean", "ess_bulk", "ess_tail"),
  probs = c(0.05, 0.95)
)

## ----coef---------------------------------------------------------------------
coef(model)
confint(model, level = 0.9)

## ----vcov---------------------------------------------------------------------
round(vcov(model), 5)

## ----interval-----------------------------------------------------------------
plot(model, type = "interval")

## ----trace--------------------------------------------------------------------
plot(model, type = "trace", variables = c("beta[price]", "beta[time]"))
plot(model, type = "density", variables = c("beta[price]", "beta[time]"))

## ----rank---------------------------------------------------------------------
plot(model, type = "rank", variables = c("beta[price]", "beta[time]"))
plot(model, type = "acf", variables = c("beta[price]", "beta[time]"))

## ----pairs--------------------------------------------------------------------
plot(model, type = "pairs", variables = c("beta[price]", "beta[time]"))

## ----draws--------------------------------------------------------------------
draws <- posterior::as_draws(model)
dim(draws)
posterior::summarise_draws(draws, "mean", "quantile2")

## ----accessors----------------------------------------------------------------
formula(model)
nobs(model)
head(model.frame(model))

## ----parallel, eval=FALSE-----------------------------------------------------
# future::plan(future::multisession, workers = 4) # run chains on four cores
# parallel_model <- fit(
#   formula = formula,
#   data = Train,
#   column_decider = "id",
#   column_occasion = "choiceid"
# )
# future::plan(future::sequential) # return to sequential evaluation

