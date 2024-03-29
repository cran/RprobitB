% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_selection.R
\name{draw_from_prior}
\alias{draw_from_prior}
\title{Sample from prior distributions}
\usage{
draw_from_prior(prior, C = 1)
}
\arguments{
\item{prior}{An object of class \code{RprobitB_prior}, which is the output of
\code{\link{check_prior}}.}

\item{C}{The number of latent classes.}
}
\value{
A list of draws for \code{alpha}, \code{s}, \code{b}, \code{Omega}, and
\code{Sigma} (if specified for the model).
}
\description{
This function returns a sample from each parameter's prior distribution.
}
\examples{
prior <- check_prior(P_f = 1, P_r = 2, J = 3)
RprobitB:::draw_from_prior(prior, C = 2)
}
\keyword{internal}
