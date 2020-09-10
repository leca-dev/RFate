### HEADER #####################################################################
##' @title Dataset Bauges PFG
##' 
##' @name DATASET_Bauges_PFG
##' 
##' @author Maya Guéguen
##'
##' @description \strong{Dataset :} Bauges
##' 
##' \strong{Step :} find dominant species and build PFG
##' 
##' \emph{Base de données du Conservatoire Botanique National Alpin - 2018}
##' 
##' @format A \code{list} object with 7 elements to help finding dominant 
##' species and building Plant Functional Groups in Bauges area :
##'   
##'   \describe{
##'     \item{sp.names}{a \code{data.frame} of dimension 
##'     \code{2913 x 3} \cr
##'     containing information about names of plant species in Bauges \cr
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{GENUS} : species' genus
##'       \item \strong{NAME} : species' full name
##'     }
##'     }
##'     \item{sp.observations}{a \code{data.frame} of dimension 
##'     \code{288654 x 6} \cr
##'     containing releves data about plant species in Bauges \cr
##'     to be used with the \code{\link{PRE_FATE.selectDominant}} function \cr
##'     \itemize{
##'       \item \strong{sites} : sites ID
##'       \item \strong{species} : species ID
##'       \item \strong{abund} : relative abundance obtained with the 
##'       \code{\link{PRE_FATE.abundBraunBlanquet}} function
##'       \item \strong{X} : x-axis coordinates in Lambers (lcc)
##'       \item \strong{Y} : y-axis coordinates in Lambers (lcc)
##'       \item \strong{habitat} : habitat ID
##'       \itemize{
##'         \item 1 : open areas (fields, lawns, meadows...)
##'         \item 2 : closed areas (forests...)
##'         \item 3 : urban areas
##'       }
##'     }
##'     }
##'     \item{dom.traits}{\code{data.frame} of dimension \code{284 x 7} \cr
##'     containing traits for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function \cr
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item Herbaceous
##'         \item Chamaephyte
##'         \item Phanerophyte
##'       }
##'       \item \strong{HEIGHT} : mean plant height (cm)
##'       \item \strong{DISPERSAL} : classes (from 1 to 7) based on 
##'       dispersal distances and types (Vittoz & Engler)
##'       \item \strong{LIGHT} : Flora Indicativa value for light preference 
##'       (from 1 to 5)
##'       \item \strong{NITROGEN} : Flora Indicativa value for soil fertility 
##'       preference (from 1 to 5)
##'       \item \strong{MOISTURE} : Flora Indicativa value for soil moisture 
##'       preference (from 1 to 5)
##'     }
##'     }
##'     \item{dom.dist_overlap}{\code{matrix} of dimension \code{275 x 275} \cr
##'     containing niche overlap distance for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function \cr}
##'     \item{dom.dist_total}{\code{list} of three \code{matrix} containing 
##'     (traits + niche overlap) distance for dominant species :
##'       \itemize{
##'         \item Herbaceous : dimension \code{204 x 204}
##'         \item Chamaephyte : dimension \code{28 x 28}
##'         \item Phanerophyte : dimension \code{30 x 30}
##'       }
##'     to be used with the \code{\link{PRE_FATE.speciesClustering_step1}} 
##'     function \cr}
##'     \item{dom.determ}{\code{data.frame} of dimension \code{262 x 10} \cr
##'     containing dominant species information relative to PFG \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step2}} 
##'     function \cr
##'     \itemize{
##'       \item \strong{PFG} : Plant Functional Group name
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item Herbaceous
##'         \item Chamaephyte
##'         \item Phanerophyte
##'       }
##'       \item \strong{ID.cluster} : Plant Functional Group ID
##'       \item \strong{species} : species ID
##'       \item \strong{ID} : species ID within its PFG
##'       \item \strong{sp.mean.dist} : 
##'       \item \strong{allSp.mean} : 
##'       \item \strong{allSp.min} : 
##'       \item \strong{allSp.max} : 
##'       \item \strong{DETERMINANT} : is the species kept as determinant 
##'       species within the PFG ? (see 
##'       \code{\link{PRE_FATE.speciesClustering_step2}} function for details)
##'     }
##'     }
##'     \item{PFG.observations}{a \code{data.frame} of dimension 
##'     \code{31666 x 20} \cr
##'     containing gathered releves data by PFG for each releve site \cr}
##'     \item{PFG.traits}{\code{data.frame} of dimension \code{20 x 8} \cr
##'     containing traits for plant functional groups \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step3}} 
##'     function \cr
##'     \itemize{
##'       \item \strong{PFG} : Plant Functional Group name
##'       \item \strong{no.species} : number of dominant species within each 
##'       Plant Functional Group
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item Herbaceous
##'         \item Chamaephyte
##'         \item Phanerophyte
##'       }
##'       \item \strong{HEIGHT} : mean value
##'       \item \strong{DISPERSAL} : median value
##'       \item \strong{LIGHT} : median value
##'       \item \strong{NITROGEN} : median value
##'       \item \strong{MOISTURE} : median value
##'     }
##'     }
##'   }
##' 
## END OF HEADER ###############################################################

"DATASET_Bauges_PFG"