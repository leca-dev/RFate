% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PRE_FATE.abundBraunBlanquet.R
\name{PRE_FATE.abundBraunBlanquet}
\alias{PRE_FATE.abundBraunBlanquet}
\title{Transform Braun-Blanquet values into relative abundances}
\usage{
PRE_FATE.abundBraunBlanquet(abund)
}
\arguments{
\item{abund}{a \code{vector} with abundance values from Braun-Blanquet
(\code{+, r, 1, 2, 3, 4, 5}), and with \code{NA} when no information}
}
\value{
A \code{vector} with numerical values between \code{0} and 
\code{100} corresponding to the median of each recovery class.
}
\description{
This script is designed to transform Braun-Blanquet abundance 
information into relative abundances or average recovery values
(between 0 and 100).
}
\details{
Braun-Blanquet values allow to estimate the abundance-dominance of a 
species based on an estimation of the number of individuals and the 
covering surface. A correspondence has been defined between this index and 
average recovery values :

\code{| Braun-Blanquet | Recovery class (\%) | Average recovery (\%) |} \cr
____________________________________________________________ \cr
\code{| + ............ | ............... <1 | ................ 0.5 |} \cr
\code{| 1 ............ | .............. 1-5 | .................. 3 |} \cr
\code{| 2 ............ | ............. 5-25 | ................. 15 |} \cr
\code{| 3 ............ | ............ 25-50 | ............... 37.5 |} \cr
\code{| 4 ............ | ............ 50-75 | ............... 62.5 |} \cr
\code{| 5 ............ | ........... 75-100 | ............... 87.5 |} \cr
##' ____________________________________________________________ \cr
\code{| 0 ............ | .................. | .................. 0 |} \cr
\code{| NA ........... | .................. | ................. NA |} \cr


\emph{\cr \cr
Braun-Blanquet J., Roussine N. & Nègre R., 1952. Les groupements végétaux de 
la France méditerranéenne. Dir. Carte Group. Vég. Afr. Nord , CNRS, 292 p.}

\emph{Baudière A. & Serve L., 1975. Les groupements végétaux du Plade 
Gorra-Blanc (massif du Puigmal, Pyrénées Orientales). Essai d'interprétation 
phytosociologique et phytogéographique. Nat. Monsp., sér. Bot., 25, 5-21.}

\emph{Foucault B. (de), 1980. Les prairies du bocage virois (Basse-Normandie, 
France). Typologie phytosociologique et essai de reconstitution des séries 
évolutives herbagères. Doc. Phytosoc., N.S., 5, 1-109.}
}
\examples{

abund_BB = sample(x = c(NA, "+", "r", 1:5), size = 100, replace = TRUE)
abund = PRE_FATE.abundBraunBlanquet(abund_BB)
summary(abund)

}
\author{
Maya Guéguen
}
\keyword{Braun-Blanquet}
\keyword{abundance,}
