## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  model = mcmc(data = data)

## ---- eval = FALSE------------------------------------------------------------
#  model = transform(model = model, scale = scale)

## ---- eval = FALSE------------------------------------------------------------
#  ### probit model
#  p = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 2)
#  m1 = mcmc(data = p)

## ---- eval = FALSE------------------------------------------------------------
#  ### multinomial probit model
#  mnp = simulate(form = choice ~ var | 0, N = 100, T = 10, J = 3)
#  m2 = mcmc(data = mnp)

## ---- eval = FALSE------------------------------------------------------------
#  ### mixed multinomial probit model
#  mmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3, re = "var")
#  m3 = mcmc(data = mmnp)

## ---- eval = FALSE------------------------------------------------------------
#  ### latent classes mixed multinomial probit model
#  lcmmnp = simulate(form = choice ~ 0 | var, N = 100, T = 10, J = 3, re = "var",
#                    parm = list("C" = 2))
#  m4 = mcmc(data = lcmmnp, latent_classes = list("C" = 2))

## ---- eval = FALSE------------------------------------------------------------
#  ### update of latent classes
#  m5 = mcmc(data = lcmmnp, latent_classes = list("update" = TRUE))

