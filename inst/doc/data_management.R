## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  form = choice ~ cost | income | travel_time

## ---- eval = FALSE------------------------------------------------------------
#  re = "cost"

## ---- eval = FALSE------------------------------------------------------------
#  data = prepare(form = form, choice_data = choice_data)

## ---- eval = FALSE------------------------------------------------------------
#  data("Train", package = "mlogit")
#  data = prepare(form = choice ~ price | 0 | time + comfort + change,
#                 choice_data = Train,
#                 re = c("price","time"))

## ---- eval = FALSE------------------------------------------------------------
#  data = simulate(form = form, N = N, T = T, J = J)

## ---- eval = FALSE------------------------------------------------------------
#  form = choice ~ cost | income | travel_time
#  re = "cost"

## ---- eval = FALSE------------------------------------------------------------
#  N = 100
#  T = 10
#  J = 3
#  alternatives = c("car", "bus", "train")
#  distr = list("cost" = list("name" = "rnorm", sd = 3),
#               "income" = list("name" =  "sample", x = (1:10)*1e3, replace = TRUE),
#               "travel_time_car" = list("name" = "rlnorm", meanlog = 1),
#               "travel_time_bus" = list("name" = "rlnorm", meanlog = 2))
#  data = simulate(form = form, N = N, T = T, J = J, re = re,
#                  alternatives = alternatives, distr = distr, C = 2)

## ---- eval = FALSE------------------------------------------------------------
#  standardize = c("income", "travel_time_car", "travel_time_bus",
#                  "travel_time_train")
#  data = simulate(form = form, N = N, T = T, J = J, re = re,
#                  alternatives = alternatives, parm = parm, distr = distr,
#                  standardize = standardize)

## ---- eval = FALSE------------------------------------------------------------
#  summary(data)

