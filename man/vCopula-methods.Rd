% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/utils.R
\docType{methods}
\name{vCopula}
\alias{vCopula}
\alias{vCopula,matrix,matrix,Copula-method}
\alias{vCopula,matrix,matrix,Copula}
\title{Copula volume on hyper-boxes}
\usage{
vCopula(u, v, copula, ...)

\S4method{vCopula}{matrix,matrix,Copula}(u, v, copula)
}
\arguments{
\item{u}{numeric matrix : minimum point of the hyper-rectangles, one row per observation.}

\item{v}{numeric matrix : maximum point of the hyper-rectangle, one row per observation.}

\item{copula}{the copula to calcule it's measure on [u,v]}

\item{...}{other parameter to be passed to methods for this generic.}
}
\value{
the measure of the copula
}
\description{
u must be piecewise smaller than v, otherwise the function will return an error.
}
\details{
A method is currently implemented for the main virtual class 'Copula', but it assumes
that a pCopula method is avaliable for the given copula.

This function calculates the measure of the copula according to the algorythme proposed by :
Umberto Cherubini & Silvia Romagnoli (2009) Computing the
Volume of n-Dimensional Copulas, Applied Mathematical Finance, 16:4, 307-314, DOI:
  10.1080/13504860802597311 link : \url{http://dx.doi.org/10.1080/13504860802597311}
}
\examples{
# For a simple one-dimentional input :
cop = copula::archmCopula('Clayton',0.7,3)
vCopula(rep(0,3),rep(1,3),cop)

# the function is vectorised :
v=matrix(seq(0,1,length.out=12),ncol=3)
u=matrix(rep(0,12),ncol=3)
vCopula(u,v,cop)
}
