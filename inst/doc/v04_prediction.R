## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(RprobitB)
set.seed(1)

## ----fit----------------------------------------------------------------------
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60
train_small <- Train[Train$id %in% unique(Train$id)[1:100], ]
model <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = train_small,
  random_effects = "price",
  column_decider = "id",
  column_occasion = "choiceid",
  iterations = 1500,
  warmup = 750,
  thin = 15,
  chains = 2,
  save_individual_draws = TRUE,
  progress = FALSE
)
summary(model)

## ----population---------------------------------------------------------------
population <- predict(model)
head(population)

## ----uncertainty--------------------------------------------------------------
head(predict(model, uncertainty = TRUE, level = 0.9))

## ----conditional--------------------------------------------------------------
head(coef(model, level = "individual"))
conditional <- predict(model, type = "conditional")
head(conditional)

## ----accuracy-----------------------------------------------------------------
observed <- model.frame(model)$choice
c(
  population = mean(population$.prediction == observed, na.rm = TRUE),
  conditional = mean(conditional$.prediction == observed, na.rm = TRUE)
)

## ----roc, fig.width=6, fig.height=4-------------------------------------------
library(ggplot2)
library(plotROC)
roc_data <- rbind(
  data.frame(
    prediction = "population", chose_B = as.integer(observed == "B"),
    probability = population$probability_B
  ),
  data.frame(
    prediction = "conditional", chose_B = as.integer(observed == "B"),
    probability = conditional$probability_B
  )
)
roc_data$prediction <- factor(
  roc_data$prediction, levels = c("population", "conditional")
)
roc_plot <- ggplot(
  roc_data, aes(d = chose_B, m = probability, color = prediction)
) +
  geom_roc(n.cuts = 0) +
  style_roc()
roc_plot

## ----wind-formula-------------------------------------------------------------
wind_formula <- choice ~ turbines + height + powerline + compensation |
  psychological_ownership

## ----wind---------------------------------------------------------------------
data("wind_power_choice", package = "choicedata")
respondents <- unique(wind_power_choice$respondent)[1:150]
wind_small <- wind_power_choice[
  wind_power_choice$respondent %in% respondents,
]
wind <- fit(
  formula = wind_formula,
  data = wind_small,
  column_decider = "respondent",
  column_occasion = "occasion",
  iterations = 10000,
  warmup = 5000,
  thin = 10,
  chains = 1,
  progress = FALSE
)
coef(wind)[c("beta[compensation]", "beta[psychological_ownership_2]")]

## ----scenario-----------------------------------------------------------------
tasks <- model.frame(wind)[1:4, ]
tasks$choice <- NULL
scenario <- tasks
scenario$compensation_2 <- 2 * scenario$compensation_2
scenario$compensation_3 <- 2 * scenario$compensation_3
cbind(
  before = predict(wind, newdata = tasks)$probability_1,
  after = predict(wind, newdata = scenario)$probability_1
)

## ----berserk-formula----------------------------------------------------------
berserk_formula <- berserk ~ 0 | white + rating + ratingDifference +
  minutesRemaining + streak

## ----lichess------------------------------------------------------------------
data("lichess_berserk_choice", package = "choicedata")
players <- unique(lichess_berserk_choice$deciderID)
first_players <- lichess_berserk_choice$deciderID %in% players[1:300]
split <- train_test(lichess_berserk_choice[first_players, ], test_number = 60)
berserk <- fit(
  formula = berserk_formula,
  data = split$train,
  column_occasion = "occasionID",
  iterations = 1000,
  warmup = 500,
  chains = 2,
  progress = FALSE
)
coef(berserk)

## ----holdout------------------------------------------------------------------
holdout_prediction <- predict(berserk, newdata = split$test)
holdout_choice <- as.character(split$test$berserk)
c(
  accuracy = mean(holdout_prediction$.prediction == holdout_choice),
  share_berserk = mean(split$test$berserk)
)

## ----calibration--------------------------------------------------------------
predicted <- holdout_prediction$probability_TRUE
bins <- cut(predicted, breaks = seq(0, 1, by = 0.1))
calibration <- data.frame(
  games = as.vector(table(bins)),
  predicted = as.vector(tapply(predicted, bins, mean)),
  observed = as.vector(tapply(split$test$berserk, bins, mean)),
  row.names = levels(bins)
)
large <- calibration[calibration$games >= 50, ]
round(large, 2)

## ----calibration-plot, fig.width=5, fig.height=5------------------------------
plot(
  large$predicted, large$observed,
  xlim = c(0, 1), ylim = c(0, 1), pch = 19,
  cex = 0.5 + 2 * large$games / max(large$games),
  xlab = "predicted Berserk probability",
  ylab = "observed Berserk rate"
)
abline(0, 1, lwd = 2, col = "grey50")

## ----residuals----------------------------------------------------------------
model_residuals <- residuals(model)
head(model_residuals)
by_decider <- tapply(
  model_residuals[, "A"], model.frame(model)$id, mean, na.rm = TRUE
)
round(quantile(by_decider, c(0, 0.25, 0.5, 0.75, 1)), 3)

## ----residuals-covariate------------------------------------------------------
price_group <- cut(
  model.frame(model)$price_A,
  breaks = quantile(model.frame(model)$price_A, seq(0, 1, 0.25)),
  include.lowest = TRUE
)
round(tapply(model_residuals[, "A"], price_group, mean, na.rm = TRUE), 3)

## ----marginal-effects---------------------------------------------------------
interpret(model, type = "mea")
average_effects <- interpret(model, type = "ame")
average_effects

## ----marginal-effects-at------------------------------------------------------
interpret(model, type = "mea", at = c(price_A = 30, price_B = 10))

## ----ordered-prediction-------------------------------------------------------
data("survey", package = "MASS")
smoking <- fit(
  Smoke ~ Age + Exer | 0,
  data = survey,
  alternatives = c("Never", "Occas", "Regul", "Heavy"),
  choice_type = "ordered",
  column_decider = NULL,
  chains = 1
)
head(predict(smoking))

## ----ordered-scenario---------------------------------------------------------
students <- model.frame(smoking)[1:3, ]
students$Smoke <- NULL
older <- students
older$Age <- older$Age + 10
cbind(
  before = predict(smoking, newdata = students)$probability_Never,
  after = predict(smoking, newdata = older)$probability_Never
)

