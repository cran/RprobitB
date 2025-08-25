## ----setup, include = FALSE---------------------------------------------------
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

## ----model-train, message = FALSE---------------------------------------------
form <- choice ~ price + time + change + comfort | 0
data <- prepare_data(form = form, choice_data = train_choice, id = "deciderID", idc = "occasionID")
model_train <- fit_model(
  data = data,
  scale = "price := -1",
  R = 1000, B = 500, Q = 10
)

## ----model-train-sparse, message = FALSE--------------------------------------
model_train_sparse <- update(model_train, form = choice ~ price | 0)

## ----model-selection-example--------------------------------------------------
model_selection(model_train, model_train_sparse)

## ----npar-example-------------------------------------------------------------
npar(model_train, model_train_sparse)

## ----loglik-example-----------------------------------------------------------
logLik(model_train)
logLik(model_train_sparse)

## ----aic-example--------------------------------------------------------------
AIC(model_train, model_train_sparse, k = 2)

## ----bic-example--------------------------------------------------------------
BIC(model_train, model_train_sparse)

## ----compute-p-si-------------------------------------------------------------
model_train <- compute_p_si(model_train, ncores = 1)
model_train_sparse <- compute_p_si(model_train_sparse, ncores = 1)

## ----waic-example-------------------------------------------------------------
WAIC(model_train)
WAIC(model_train_sparse)

## ----compute-mml--------------------------------------------------------------
model_train <- mml(model_train)
attr(model_train$mml, "mmll")

## ----bayes-factor-example-----------------------------------------------------
model_selection(model_train, model_train_sparse, criteria = "BF")

## ----pred-acc-example---------------------------------------------------------
pred_acc(model_train, model_train_sparse)

