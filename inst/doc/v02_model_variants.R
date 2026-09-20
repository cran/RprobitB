## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(RprobitB)
set.seed(1)

## ----normalization------------------------------------------------------------
scale_default <- fit(
  choice ~ x + z | 0,
  dgp_parameters = list(beta = c(x = 1, z = -0.5)),
  n_deciders = 300,
  chains = 1
)
summary(scale_default)
scale_z <- update(scale_default, scale = c(z = -1))
summary(scale_z)

## ----prior--------------------------------------------------------------------
default_prior <- fit(
  choice ~ x | 0,
  dgp_parameters = list(beta = c(x = -1)),
  chains = 1
)
default_prior$prior
summary(default_prior)
moderate_prior <- update(
  default_prior, prior = list(fixed_mean = 1, fixed_covariance = matrix(0.5))
)
tight_prior <- update(
  default_prior, prior = list(fixed_mean = 1, fixed_covariance = matrix(0.01))
)
data.frame(
  variable = "beta[x]", dgp = -1, default = coef(default_prior),
  moderate = coef(moderate_prior), tight = coef(tight_prior),
  row.names = NULL
)

## ----covariate-types-sim------------------------------------------------------
covariate_types <- fit(
  choice ~ x | z | w,
  n_deciders = 500,
  dgp_parameters = list(beta = c(
    x = 0.5, z_B = -0.5, ASC_B = 0.25, w_A = -0.5, w_B = 0.5
  )),
  chains = 1
)
summary(covariate_types)

## ----base---------------------------------------------------------------------
base_b <- update(covariate_types, base = "B")
coef(base_b)[c("beta[x]", "beta[z_A]", "beta[ASC_A]")]

## ----travel-formula-----------------------------------------------------------
travel_formula <- choice ~ wait + vcost + travel | income + size

## ----travel-------------------------------------------------------------------
data("TravelMode", package = "AER")
TravelMode$choice <- TravelMode$choice == "yes"
TravelMode$vcost <- TravelMode$vcost / 1.6196
TravelMode$income <- TravelMode$income / 1.6196
travel <- fit(
  formula = travel_formula,
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
summary(travel)

## ----travel-income------------------------------------------------------------
mode_effects <- interpret(travel, type = "mea")
mode_effects[mode_effects$covariate == "income", ]

## ----canada-load--------------------------------------------------------------
data("ModeCanada", package = "mlogit")
head(ModeCanada)

## ----canada-data--------------------------------------------------------------
ModeCanada$cost <- ModeCanada$cost / 1.6151
ModeCanada$income <- ModeCanada$income / 1.6151
bus_trips <- ModeCanada$case[ModeCanada$alt == "bus" & ModeCanada$choice == 1]
canada_data <- ModeCanada[
  ModeCanada$alt != "bus" & !(ModeCanada$case %in% bus_trips),
]
set_size <- table(canada_data$case)
canada_data <- canada_data[set_size[as.character(canada_data$case)] > 1, ]
canada_data <- canada_data[
  canada_data$case %in% unique(canada_data$case)[1:1000],
]
table(table(canada_data$case))

## ----canada-------------------------------------------------------------------
canada <- fit(
  choice ~ cost + ivt + ovt + freq | income + urban,
  data = canada_data,
  format = "long",
  column_decider = "case",
  column_alternative = "alt",
  iterations = 6000,
  warmup = 3000,
  thin = 15,
  chains = 2,
  progress = FALSE
)
summary(canada)

## ----ordered-sim--------------------------------------------------------------
ordered_sim <- fit(
  choice ~ x | 0,
  alternatives = c("low", "middle", "high"),
  choice_type = "ordered",
  n_deciders = 500,
  dgp_parameters = list(beta = c(x = 1), gamma = c(0, 1)),
  chains = 1
)
summary(ordered_sim)

## ----ordered------------------------------------------------------------------
data("survey", package = "MASS")
levels(survey$Smoke)
smoking_levels <- c("Never", "Occas", "Regul", "Heavy")
smoking <- fit(
  Smoke ~ Age + Exer | 0,
  data = survey,
  alternatives = smoking_levels,
  choice_type = "ordered",
  column_decider = NULL,
  chains = 1
)
summary(smoking)

## ----ordered-figure-----------------------------------------------------------
thresholds <- coef(smoking)[c("gamma[2]", "gamma[3]")]
cuts <- c(-Inf, 0, thresholds, Inf)
shades <- grey(seq(0.45, 0.9, length.out = length(smoking_levels)))
utility <- seq(-3.5, 3.5, length.out = 400)
plot(
  utility, dnorm(utility),
  type = "n", axes = FALSE, ylab = "",
  xlab = "latent utility of a student"
)
for (k in seq_along(smoking_levels)) {
  inside <- utility >= cuts[k] & utility <= cuts[k + 1]
  polygon(
    c(max(cuts[k], -3.5), utility[inside], min(cuts[k + 1], 3.5)),
    c(0, dnorm(utility[inside]), 0),
    col = shades[k], border = NA
  )
}
lines(utility, dnorm(utility), lwd = 2)
axis(1, at = c(-3, 0, 3))
legend(
  "topright", legend = smoking_levels, fill = shades, border = NA, bty = "n"
)

## ----ordered-interpret--------------------------------------------------------
age_effects <- interpret(smoking, type = "ame")
age_effects

## ----ranked-sim---------------------------------------------------------------
ranked_sim <- fit(
  rank ~ x | 0,
  choice_type = "ranked",
  n_deciders = 300,
  dgp_parameters = list(
    beta = c(x = 1),
    Sigma = rbind(c(0, 0, 0), c(0, 1, 0.2), c(0, 0.2, 1))
  ),
  chains = 1
)
summary(ranked_sim, variables = c("beta[x]", "Sigma[C,B]", "Sigma[C,C]"))

## ----ranked-------------------------------------------------------------------
data("Game", package = "mlogit")
gaming <- fit(
  ch ~ own | age + hours,
  data = Game,
  alternatives = c(
    "Xbox", "PlayStation", "PSPortable", "GameCube", "GameBoy", "PC"
  ),
  choice_type = "ranked",
  delimiter = ".",
  column_decider = NULL,
  iterations = 1000,
  warmup = 500,
  thin = 20,
  chains = 2,
  progress = FALSE
)
coef(gaming)[1:6]

## ----ranked-interpret---------------------------------------------------------
platform_effects <- interpret(gaming, type = "mea")
platform_effects[platform_effects$covariate == "hours", ]

## ----ranked-numbers, include=FALSE--------------------------------------------
pc_hours <- platform_effects$mean[
  platform_effects$covariate == "hours" & platform_effects$alternative == "PC"
]

