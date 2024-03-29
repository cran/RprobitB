% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{update_classes_wb}
\alias{update_classes_wb}
\title{Weight-based update of latent classes}
\usage{
update_classes_wb(Cmax, epsmin, epsmax, distmin, s, b, Omega)
}
\arguments{
\item{Cmax}{The maximum number of classes.}

\item{epsmin}{The threshold weight (between 0 and 1) for removing a class.}

\item{epsmax}{The threshold weight (between 0 and 1) for splitting a class.}

\item{distmin}{The (non-negative) threshold difference in class means for joining two
classes.}

\item{s}{The vector of class weights of length \code{C}.
Set to \code{NA} if \code{P_r = 0}.
For identifiability, the vector must be non-ascending.}

\item{b}{The matrix of class means as columns of dimension \code{P_r} x \code{C}.
Set to \code{NA} if \code{P_r = 0}.}

\item{Omega}{The matrix of class covariance matrices as columns of dimension
\code{P_r*P_r} x \code{C}.
Set to \code{NA} if \code{P_r = 0}.}
}
\value{
A list of updated values for \code{s}, \code{b}, and \code{Omega}.
}
\description{
This function updates the latent classes based on their class weights.
}
\details{
The updating scheme bases on the following rules:
\itemize{
\item We remove class \eqn{c}, if \eqn{s_c<\epsilon_{min}}, i.e. if the
class weight \eqn{s_c} drops below some threshold
\eqn{\epsilon_{min}}.
This case indicates that class \eqn{c} has a negligible impact on the
mixing distribution.
\item We split class \eqn{c} into two classes \eqn{c_1} and \eqn{c_2}, if
\eqn{s_c>\epsilon_{max}}.
This case indicates that class \eqn{c} has a high influence on the mixing
distribution whose approximation can potentially be improved by
increasing the resolution in directions of high variance.
Therefore, the class means \eqn{b_{c_1}} and \eqn{b_{c_2}} of the new
classes \eqn{c_1} and \eqn{c_2} are shifted in opposite directions from
the class mean \eqn{b_c} of the old class \eqn{c} in the direction of the
highest variance.
\item We join two classes \eqn{c_1} and \eqn{c_2} to one class \eqn{c}, if
\eqn{||b_{c_1} - b_{c_2}||<\epsilon_{distmin}}, i.e. if
the euclidean distance between the class means \eqn{b_{c_1}} and
\eqn{b_{c_2}} drops below some threshold \eqn{\epsilon_{distmin}}.
This case indicates location redundancy which should be repealed. The
parameters of \eqn{c} are assigned by adding the values of \eqn{s} from
\eqn{c_1} and \eqn{c_2} and averaging the values for \eqn{b} and
\eqn{\Omega}.
The rules are executed in the above order, but only one rule per iteration
and only if \code{Cmax} is not exceeded.
}
}
\examples{
### parameter settings
s <- c(0.8,0.2)
b <- matrix(c(1,1,1,-1), ncol=2)
Omega <- matrix(c(0.5,0.3,0.3,0.5,1,-0.1,-0.1,0.8), ncol=2)

### Remove class 2
RprobitB:::update_classes_wb(Cmax = 10, epsmin = 0.3, epsmax = 0.9, distmin = 1,
                             s = s, b = b, Omega = Omega)

### Split class 1
RprobitB:::update_classes_wb(Cmax = 10, epsmin = 0.1, epsmax = 0.7, distmin = 1,
                             s = s, b = b, Omega = Omega)

### Join classes
RprobitB:::update_classes_wb(Cmax = 10, epsmin = 0.1, epsmax = 0.9, distmin = 3,
                             s = s, b = b, Omega = Omega)
}
\keyword{internal}
