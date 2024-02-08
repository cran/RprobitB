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

## ----RprobitB-parameter-example-----------------------------------------------
RprobitB:::RprobitB_parameter(
  P_f = 1,
  P_r = 2,
  J = 3,
  N = 10,
  C = 2, # the number of latent classes
  alpha = c(1), # the fixed coefficient vector of length 'P_f'
  s = c(0.6, 0.4), # the vector of class weights of length 'C'
  b = matrix(c(-1, 1, 1, 2), nrow = 2, ncol = 2),
  # the matrix of class means as columns of dimension 'P_r' x 'C'
  Omega = matrix(c(diag(2), 0.1 * diag(2)), nrow = 4, ncol = 2),
  # the matrix of class covariance matrices as columns of dimension 'P_r^2' x 'C'
  Sigma = diag(2), # the differenced error term covariance matrix of dimension '(J-1)' x '(J-1)'
  # the undifferenced error term covariance matrix is labeled 'Sigma_full'
  z = rep(1:2, 5) # the vector of the allocation variables of length 'N'
)

