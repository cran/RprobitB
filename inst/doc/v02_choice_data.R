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

## ----overview-train-choice-data-----------------------------------------------
str(train_choice)

## ----train-formula------------------------------------------------------------
form <- choice ~ price + time + comfort + change | 0

## ----train-re-----------------------------------------------------------------
re <- c("price", "time")

## ----eval = FALSE-------------------------------------------------------------
#  data <- prepare_data(form = form, choice_data = choice_data)

## ----prepare-train-choice-data------------------------------------------------
data <- prepare_data(form = form, choice_data = train_choice, re = re, id = "deciderID", idc = "occasionID")

## ----summary-train-choice-data------------------------------------------------
summary(data)
plot(data)

## ----eval = FALSE-------------------------------------------------------------
#  data <- simulate_choices(form = form, N = N, T = T, J = J)

## ----data-sim-meta------------------------------------------------------------
N <- 100
T <- 10
alternatives <- c("A", "B")
base <- "B"
form <- choice ~ var1 | var2 | var3
re <- c("ASC", "var2")

## ----data-sim-overview--------------------------------------------------------
overview_effects(form = form, re = re, alternatives = alternatives, base = base)

## ----data-sim-----------------------------------------------------------------
data <- simulate_choices(
  form = form,
  N = N,
  T = T,
  J = 2,
  re = re,
  alternatives = alternatives,
  seed = 1,
  true_parameter = list(
    alpha = c(-1, 0, 1),
    b = matrix(c(2, -0.5), ncol = 1)
  )
)
summary(data)

## ----data-plot-by-choice------------------------------------------------------
plot(data, by_choice = TRUE)

## ----data-split-deciders------------------------------------------------------
train_test(data, test_proportion = 0.3, by = "N")

## ----data-split-occasions-----------------------------------------------------
train_test(data, test_number = 2, by = "T", random = TRUE, seed = 1)

