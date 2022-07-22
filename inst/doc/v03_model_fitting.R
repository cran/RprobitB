## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "",
  fig.align = "center",
  fig.dim = c(8, 6), 
  out.width = "75%"
)
library("RprobitB")
options("RprobitB_progress" = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  fit_model(data = data)

## ---- transform-Train---------------------------------------------------------
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 * 2.20371
Train$price_B <- Train$price_B / 100 * 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60

## ---- echo = FALSE------------------------------------------------------------
set.seed(1)

## ---- message = FALSE---------------------------------------------------------
form <- choice ~ price + time + change + comfort | 0
data <- prepare_data(form = form, choice_data = Train)
model_train <- fit_model(
  data = data,
  scale = "price := -1"
)

## ---- coef-model-train--------------------------------------------------------
coef(model_train)

## ---- plot-coef-model-train---------------------------------------------------
plot(coef(model_train), sd = 3)

## ---- str-gibbs-samples-------------------------------------------------------
str(model_train$gibbs_samples, max.level = 2, give.attr = FALSE)

## ---- summary-model-train-----------------------------------------------------
summary(model_train, 
        FUN = c("mean"        = mean, 
                "sd"          = stats::sd, 
                "R^"          = R_hat,
                "custom_stat" = function(x) abs(mean(x) - median(x))
                )
       )

## ---- plot-trace-model-train--------------------------------------------------
par(mfrow = c(2,1))
plot(model_train, type = "trace")

## ---- plot-acf-model-train----------------------------------------------------
par(mfrow = c(2,3))
plot(model_train, type = "acf")

## ---- transform-model-train---------------------------------------------------
model_train <- transform(model_train, B = 1)

## ---- eval = FALSE------------------------------------------------------------
#  model_train <- transform(model_train, Q = 100)

## ---- eval = FALSE------------------------------------------------------------
#  model_train <- transform(model_train, scale = "Sigma_1 := 1")

