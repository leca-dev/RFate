% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/POST_FATE.graphic_evolutionPixels.R
\name{POST_FATE.graphic_evolutionPixels}
\alias{POST_FATE.graphic_evolutionPixels}
\title{Create a graphical representation of the evolution of PFG abundance 
through time for 5 (or more) pixels of a \code{FATE} simulation}
\usage{
POST_FATE.graphic_evolutionPixels(
  name.simulation,
  file.simulParam = NULL,
  opt.cells_ID = NULL,
  opt.doPlot = TRUE
)
}
\arguments{
\item{name.simulation}{a \code{string} corresponding to the main directory 
or simulation name of the \code{FATE} simulation}

\item{file.simulParam}{default \code{NULL}. \cr A \code{string} 
corresponding to the name of a parameter file that will be contained into 
the \code{PARAM_SIMUL} folder of the \code{FATE} simulation}

\item{opt.cells_ID}{(\emph{optional}) default \code{NULL}. \cr The cells ID 
of the studied area for which PFG abundances will be extracted}

\item{opt.doPlot}{(\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
plot(s) will be processed, otherwise only the calculation and reorganization 
of outputs will occur, be saved and returned}
}
\value{
A \code{list} containing one \code{data.frame} object with the 
following columns, and one \code{ggplot2} object :

\describe{
  \item{tab}{
    \describe{
      \item{\code{TYPE}}{concerned information (either '\code{light}', 
      '\code{abundance}' or '\code{soil}')}
      \item{\code{GROUP}}{concerned entity (either 
      '\code{STRATUM_[...]}', PFG name or '\code{soil}')}
      \item{\code{ID.pixel}}{number of the concerned pixel}
      \item{\code{HAB}}{habitat of the concerned pixel}
      \item{\code{YEAR}}{concerned simulation year}
      \item{\code{value}}{concerned value extracted from \code{.csv} files 
      produced by \code{\link{POST_FATE.temporalEvolution}}}
    }
  }
  \item{plot}{\code{ggplot2} object, representing the evolution of each PFG 
  abundance, \emph{and light and soil resources if those modules were 
  activated} \cr \cr}
}


One \code{POST_FATE_TABLE_PIXEL_evolution_pixels_[...].csv} file is created : 
\describe{
  \item{\emph{pixels ids}}{always, containing the \code{data.frame} detailed 
  above}
}


One \code{POST_FATE_[...].pdf} file is created : 
\describe{
  \item{\file{GRAPHIC_A \cr pixels}}{to visualize for each PFG the evolution 
  of its abundance within each selected pixel through simulation time}
}
}
\description{
This script is designed to produce one graphical representation 
for a \code{FATE} simulation : the evolution through time of the 
abundance of each PFG for 5 (or more) randomly selected cells of the studied 
area.
}
\details{
This function allows to obtain, for a specific \code{FATE} simulation and 
a specific parameter file within this simulation, one preanalytical graphic :

\itemize{
  \item{the evolution of \strong{abundance} of each Plant Functional Group 
  through simulation time, within 5 (or more) randomly selected pixels of 
  the studied area (\code{FATE} \emph{arbitrary unit})
  }
  \item{\strong{if light was activated} (see 
  \code{\link{PRE_FATE.params_globalParameters}}), \cr evolution of 
  \strong{light resources} within the selected pixels is also represented 
  (\emph{\code{1}: Low, \code{2}: Medium, \code{3}: High})
  }
  \item{\strong{if soil was activated} (see 
  \code{\link{PRE_FATE.params_globalParameters}}), \cr evolution of 
  \strong{soil resources} within the selected pixels is also represented 
  (user-defined scale) \cr \cr
  }
}

\strong{It requires} that the \code{\link{POST_FATE.temporalEvolution}} 
function has been run and that the file 
\code{POST_FATE_TABLE_PIXEL_evolution_abundance.csv} exists (as well as the 
\code{POST_FATE_TABLE_PIXEL_evolution_light.csv} and 
\code{POST_FATE_TABLE_PIXEL_evolution_soil.csv} files if those modules were 
activated).
}
\seealso{
\code{\link{POST_FATE.temporalEvolution}}
}
\author{
Maya Guéguen
}
\keyword{FATE,}
\keyword{abundance}
\keyword{outputs,}
\keyword{through}
\keyword{time}
