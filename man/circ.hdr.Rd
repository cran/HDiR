\encoding{utf8}
\name{circ.hdr}
\alias{circ.hdr}
\title{Computation of HDRs for a circular density}
\description{This function computes HDRs for a circular density.}
\usage{
circ.hdr(f,tau=NULL,level=NULL,plot.hdr=TRUE,col=NULL,
         lty=NULL,shrink=NULL,cex=NULL,pch=NULL)
}
\arguments{
  \item{f}{Circular density function.}
  \item{tau}{Numeric probability. According to Saavedra-Nieves and Crujeiras (2020), \code{1-tau} represents the probability coverage required for HDR. If \code{tau=NULL}, a value for the threshold \code{level} of the HDR must be provided.}
  \item{level}{Numeric threshold of the HDR provided by the user. If \code{level} is larger than the maximum value of the density, the HDR is equal to the emptyset. If \code{level} is smaller than the minimum of the density, the HDR coincides with the support distribution. If \code{level=NULL}, a value for the probability coverage \code{1-tau} of the HDR must be provided.}
  \item{plot.hdr}{Logical string. If \code{TRUE}, the circular density and the level set or the HDR are represented graphically. Default \code{plot.density=TRUE}.}
  \item{col}{Color for plotting the HDR. Default \code{col="darkgray"} is used.}
  \item{lty}{A numeric value indicating the line type to represent the threshold of HDR. Line type can be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash). Default \code{lty=2}.}
  \item{shrink}{Parameter that controls the size of the plotted circle. Default is 2. Larger than 1 values shrink the circle, while smaller values enlarge the circle.}
  \item{cex}{Point character size for representing the data on the scatterplot. Default is 0.5.}
  \item{pch}{Plotting character. Default \code{19}.}
}
\details{A detailed definition of HDRs for circular and spherical densities is given in Saavedra-Nieves and Crujeiras (2020). Trapezoidal rule is used to compute the threshold of HDR when \code{tau} is provided.}
\value{
If \code{tau} is provided, a list with the next components:
\item{hdr}{Boundaries of the HDR.}
\item{prob.content}{Probability coverage \code{1-tau}.}
\item{threshold}{Threshold or level of the HDR associated to the probability content \code{1-tau}.}
If \code{level} is provided, a list with the next components:
\item{levelset}{Boundaries of the level set or a character indicating if the level set is equal to the emptyset or the support distribution.}
\item{level}{Level of the level set.}
}
\references{
Saavedra-Nieves, P. and Crujeiras, R. M. (2020). Nonparametric estimation of directional highest density regions. arXiv preprint arXiv:2009.08915.}
\author{
Paula Saavedra-Nieves and Rosa M. Crujeiras.
}

\examples{
# HDRs of model 11 in library NPCirc
library(NPCirc)
f<-function(x){return(dcircmix(x,11))}
circ.hdr(f,tau=0.2,shrink=1.5)
circ.hdr(f,tau=0.8,shrink=1.5)
}
