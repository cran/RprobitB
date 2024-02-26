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
form <- choice ~ price + time + change + comfort | 0
data <- prepare_data(form = form, choice_data = train_choice, id = "deciderID", idc = "occasionID")
model_train <- fit_model(
  data = data,
  scale = "price := -1"
)

## ----predict-model-train------------------------------------------------------
predict(model_train)

## ----predict-model-train-indlevel---------------------------------------------
pred <- predict(model_train, overview = FALSE)
head(pred, n = 10)

## ----model-train-covs---------------------------------------------------------
get_cov(model_train, id = 1, idc = 8)

## ----model-train-coeffs-------------------------------------------------------
coef(model_train)

## ----model-train-Sigma--------------------------------------------------------
point_estimates(model_train)$Sigma

## ----roc-example, warning = FALSE, message = FALSE, out.width = "50%", fig.dim = c(6,6)----
library(plotROC)
ggplot(data = pred, aes(m = A, d = ifelse(true == "A", 1, 0))) +
  geom_roc(n.cuts = 20, labels = FALSE) +
  style_roc(theme = theme_grey)

## ----predict-model-train-given-covs-1-----------------------------------------
predict(
  model_train,
  data = data.frame(
    "price_A" = c(100, 110),
    "price_B" = c(100, 100)
  ),
  overview = FALSE
)

## ----predict-model-train-given-covs-2-----------------------------------------
predict(
  model_train,
  data = data.frame(
    "price_A" = c(100, 110),
    "comfort_A" = c(1, 0),
    "price_B" = c(100, 100),
    "comfort_B" = c(1, 1)
  ),
  overview = FALSE
)

