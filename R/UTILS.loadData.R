### HEADER #####################################################################
##' @title Load a dataset
##' 
##' @name .loadData
##'
##' @author Maya Guéguen
##' 
##' @description This function loads one of the available data sets, 
##' containing :
##' 
##' \describe{
##'   \item{PNE_PFG}{elements to create the Plant Functional Groups (PFG) 
##'   over the Ecrins National Park (PNE) \cr
##'   (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2012_GCB.pdf}{Boulangeat et al. 2012 GCB}})}
##'   \item{PNE_PARAM}{all necessary files to build the \code{FATE} 
##'   simulation folder as well as the parameter files \cr
##'   (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2014_GCB.pdf}{Boulangeat et al. 2014 GCB}})}
##'   \item{PNE_RESULTS}{results obtained from outputs of \code{FATE} 
##'   simulation \cr
##'   (\emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2014_GCB.pdf}{Boulangeat et al. 2014 GCB}})}
##' }
##' 
##' @param data.name a \code{string} corresponding to the name of the dataset 
##' that will be loaded
##' 
##' @return 
##' 
##' \strong{\cr PNE_PFG}
##' 
##' A \code{list} object with 6 elements to help building the Plant Functional 
##' Group :
##'   
##'   \describe{
##'     \item{sp.observations}{a \code{data.frame} of dimension 
##'     \code{168313 x 7} \cr
##'     containing releves data about plant species in the PNE \cr
##'     to be used with the \code{\link{PRE_FATE.selectDominant}} function \cr
##'     \itemize{
##'       \item \strong{sites} : sites ID
##'       \item \strong{X} : x-axis coordinates in Lambers (lcc)
##'       \item \strong{Y} : y-axis coordinates in Lambers (lcc)
##'       \item \strong{habitat} : habitat ID from 
##'       \href{http://www.ecrins-parcnational.fr/sites/ecrins-parcnational.com/files/fiche_doc/12083/2006-atlas-delphine.pdf}{DELPHINE} :
##'       \itemize{
##'         \item 0 : glaciers and eternal snows
##'         \item 31 : uncolonized rocks
##'         \item 40 : lawns and meadows
##'         \item 50 : low moors
##'         \item 60 : open areas, brush
##'         \item 70 : semi-closed areas
##'         \item 81 : closed areas
##'         \item 83 : forests
##'       }
##'       \item \strong{species} : species ID
##'       \item \strong{abund_BB} : Braun-Blanquet abundance \cr 
##'       (see \code{\link{PRE_FATE.abundBraunBlanquet}} function for details)
##'       \item \strong{abund} : relative abundance obtained from the 
##'       \code{abund_BB} column with the 
##'       \code{\link{PRE_FATE.abundBraunBlanquet}} function
##'     }
##'     }
##'     \item{dom.traits}{\code{data.frame} of dimension \code{359 x 6} \cr
##'     containing traits for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function \cr
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item H : herbaceous
##'         \item C : chamaephytes
##'         \item P : phanerophytes
##'       }
##'       \item \strong{height} : mean plant height (cm)
##'       \item \strong{dispersal} : classes (from 1 to 7) based on 
##'       dispersal distances and types (Vittoz & Engler)
##'       \item \strong{palatability} : classes (from 0 to 3) (CBNA)
##'       \item \strong{light} : Ellenberg indicator value for light (from 1 
##'       to 9)
##'     }
##'     }
##'     \item{dom.dist_overlap}{\code{matrix} of dimension \code{358 x 358} \cr
##'     containing niche overlap distance for dominant species \cr
##'     to be used with the \code{\link{PRE_FATE.speciesDistance}} function \cr}
##'     \item{dom.determ}{\code{data.frame} of dimension \code{359 x 5} \cr
##'     containing dominant species information relative to PFG \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step2}} 
##'     function \cr
##'     \itemize{
##'       \item \strong{species} : species ID
##'       \item \strong{name} : species name (taxonomic)
##'       \item \strong{GROUP} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item H : herbaceous
##'         \item C : chamaephytes
##'         \item P : phanerophytes
##'       }
##'       \item \strong{PFG} : name of assigned Plant Functional Group
##'       \item \strong{determinant} : is the species kept as determinant 
##'       species within the PFG ? (see 
##'       \code{\link{PRE_FATE.speciesClustering_step2}} function for details)
##'     }
##'     }
##'     \item{nb.clusters}{\code{vector} of length \code{3} \cr
##'     number of groups kept for each life-form class, obtained by cutting 
##'     the hierarchical tree obtained from species distances}
##'     \item{PFG.traits}{\code{data.frame} of dimension \code{24 x 10} \cr
##'     containing traits for plant functional groups \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step3}} 
##'     function \cr
##'     \itemize{
##'       \item \strong{PFG_name} : full descriptive Plant Functional Group name
##'       \item \strong{PFG} : Plant Functional Group short name
##'       \item \strong{type} : rough generalization of Raunkier life-forms :
##'       \itemize{
##'         \item H : herbaceous
##'         \item C : chamaephytes
##'         \item P : phanerophytes
##'       }
##'       \item \strong{strata} : maximum height stratum that can be reached by 
##'       the PFG (from 1 to 5)
##'       \item \strong{disp} : MEDIAN classes (from 1 to 7) based on 
##'       dispersal distances and types (Vittoz & Engler)
##'       \item \strong{light} : MEDIAN Ellenberg indicator value for light 
##'       (from 1 to 9)
##'       \item \strong{height} : MEAN PFG height (cm)
##'       \item \strong{palatability} : MEDIAN classes (from 0 to 3) (CBNA)
##'       \item \strong{longevity} : MEAN age of lifespan
##'       \item \strong{maturity} : MEAN age of maturity
##'     }
##'     }
##'   }
##'   
##' 
##' \strong{\cr\cr PNE_PARAM}
##' 
##' A \code{list} object with 13 elements to help building the simulation 
##' files and folders to run a \code{FATE} simulation :
##'   
##'   \describe{
##'     \item{masks}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{782 x 619} with a resolution of \code{100m} and Lambers (lcc) 
##'     projection, containing 7 mask layers with binary values (0 or 1) to be 
##'     used in a \code{FATE} simulation :
##'     \itemize{
##'       \item \strong{maskEcrins} : simulation map, where occurs succession
##'       \item \strong{noDisturb} : perturbation map, when there is none
##'       \item \strong{mowing} : perturbation map, where occurs mowing
##'       \item \strong{grazing1} : perturbation map, where occurs light grazing
##'       \item \strong{grazing2} : perturbation map, where occurs extensive 
##'       grazing
##'       \item \strong{grazing3} : perturbation map, where occurs intensive 
##'       grazing
##'       \item \strong{grazingAll} :  perturbation map, where occurs grazing, 
##'       all types combined
##'     }
##'     }
##'     \item{HS_0}{a \code{\link[raster]{stack}} object of dimension 
##'     \code{782 x 619} with a resolution of \code{100m} and Lambers (lcc) 
##'     projection, containing 24 layers with probability values (between 0 and 
##'     1) representing Habitat Suitability for initialization phase and 
##'     \emph{equilibrium} or \emph{current} time (0) for each PFG 
##'     and to be used in a \code{FATE} simulation. \cr
##'     These maps are coming from Species Distribution Models and methods to 
##'     obtain them are described in Supplementary Materials of 
##'     \emph{\href{https://mayagueguen.github.io/FATE-WEBSITE/papers/Boulangeat_2014_GCB.pdf}{Boulangeat et al. 2014 GCB}}.}
##'     \item{HS_15}{same as HS_0 but 15 years after equilibrium}
##'     \item{HS_30}{same as HS_0 but 30 years after equilibrium}
##'     \item{HS_45}{same as HS_0 but 45 years after equilibrium}
##'     \item{HS_60}{same as HS_0 but 60 years after equilibrium}
##'     \item{HS_75}{same as HS_0 but 75 years after equilibrium}
##'     \item{HS_90}{same as HS_0 but 90 years after equilibrium}
##'     \item{strata_limits}{a \code{vector} of length \code{5} \cr
##'     containing height of stratum limits in centimeters \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGsuccession}} and 
##'     \code{\link{PRE_FATE.params_PFGlight}} functions \cr}
##'     \item{succ_light}{a \code{data.frame} of dimension \code{24 x 6} \cr
##'     containing traits for plant functional groups \cr
##'     obtained from the \code{\link{PRE_FATE.speciesClustering_step3}} 
##'     function \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGsuccession}} and 
##'     \code{\link{PRE_FATE.params_PFGlight}} functions \cr}
##'     \item{disp}{a \code{data.frame} of dimension \code{24 x 4} \cr
##'     containing dispersal values (in meters) for plant functional groups \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGdispersal}} function 
##'     \cr}
##'     \item{dist}{a \code{data.frame} of dimension \code{384 x 5} \cr
##'     containing response of plant functional groups to disturbances \cr
##'     to be used with the \code{\link{PRE_FATE.params_PFGdisturbance}} 
##'     function \cr}
##'     \item{global}{a \code{vector} of length \code{18} \cr
##'     containing global parameter values for a \code{FATE} simulation in 
##'     the PNE \cr
##'     to be used with the \code{\link{PRE_FATE.params_globalParameters}} 
##'     function \cr
##'     }
##'   }
##' 
##' 
##' \strong{\cr\cr PNE_RESULTS}
##' 
##' A \code{list} object with 5 elements :
##' 
##' \describe{
##'   \item{evaluation}{a \code{data.frame} of dimension \code{24 x 11} \cr
##'     containing statistical metrics to evaluate the predictive quality of 
##'     both Habitat Suitability models (HS) and \code{FATE} simulation 
##'     outputs \cr
##'     \itemize{
##'       \item \strong{PFG} : name of Plant Functional Group
##'       \item \strong{nb.obs.absences} : number of observed absences
##'       \item \strong{nb.obs.presences} : number of observed presences
##'       \item \strong{specificity.FATE} : proportion of actual negatives that 
##'       are correctly identified as such by the \code{FATE} model (true 
##'       negative rate)
##'       \item \strong{specificity.HS} : proportion of actual negatives that 
##'       are correctly identified as such by the Habitat Suitability model 
##'       (true negative rate)
##'       \item \strong{sensitivity.FATE} : proportion of actual positives that 
##'       are correctly identified as such by the \code{FATE} model (true 
##'       positive rate)
##'       \item \strong{sensitivity.HS} : proportion of actual positives that 
##'       are correctly identified as such by the Habitat Suitability model 
##'       (true positive rate)
##'       \item \strong{TSS.FATE} : True Skill Statistic for \code{FATE} 
##'       model
##'       \item \strong{TSS.HS} : True Skill Statistic for Habitat Suitability 
##'       model
##'       \item \strong{error.rate.FATE} : percentage of bad predictions for 
##'       \code{FATE} model
##'       \item \strong{error.rate.HS} : percentage of bad predictions for 
##'       Habitat Suitability model
##'     }
##'     }
##'   \item{abund_str.equilibrium}{a \code{\link[raster]{stack}} object of 
##'   dimension \code{782 x 619} with a resolution of \code{100m} and Lambers 
##'   (lcc) projection, containing 120 layers representing \code{FATE} 
##'   \strong{abundances for year 800 of initialization phase} (= 
##'   \emph{equilibrium} or \emph{current} time). \cr
##'   Maps are per PFG and per height stratum.}
##'   \item{forest_cover.init}{a \code{\link[raster]{stack}} object of 
##'   dimension \code{782 x 619} with a resolution of \code{100m} and Lambers 
##'   (lcc) projection, containing 16 layers representing \code{FATE} 
##'   \strong{relative forest cover through initialization phase (from year 50 
##'   to 800)}. \cr
##'   Maps are summing all PFGs through all height strata above 1.5 meters. \cr
##'   }
##'   \item{forest_cover \cr CC_BAU}{a \code{\link[raster]{stack}} object of 
##'   dimension \code{782 x 619} with a resolution of \code{100m} and Lambers 
##'   (lcc) projection, containing 14 layers representing \code{FATE} 
##'   \strong{relative forest cover through Climate Change (CC) + 
##'   Business-As-Usual (BAU) scenario (from year 850 to 1500)}. \cr
##'   Maps are summing all PFGs through all height strata above 1.5 meters. \cr
##'     }
##'   \item{forest_cover\cr CC_Abandon}{a \code{\link[raster]{stack}} object of 
##'   dimension \code{782 x 619} with a resolution of \code{100m} and Lambers 
##'   (lcc) projection, containing 14 layers representing \code{FATE} 
##'   \strong{relative forest cover through Climate Change (CC) + Abandonment 
##'   (BAU) scenario (from year 850 to 1500)}. \cr
##'   Maps are summing all PFGs through all height strata above 1.5 meters. \cr
##'   }
##' }
##'
##' @export
##'
##' @importFrom utils download.file
##'
## END OF HEADER ###############################################################

.loadData = function(data.name)
{
  .testParam_notInValues.m("data.name", data.name, c("PNE_PFG", "PNE_PARAM", "PNE_RESULTS"))
  
  if (!file.exists(paste0(data.name, ".RData")))
  {
    cat("\n > Downloading `",data.name,"` dataset...\n")
    download.file(url = paste0("https://raw.githubusercontent.com/MayaGueguen/RFate/master/data-raw/DATASET_PNE/"
                               , data.name, ".RData")
                  , destfile = paste0(data.name, ".RData")
                  , method = "wget")
    if (!file.exists(paste0(data.name, ".RData")))
    {
      stop(paste0("Download of `", data.name,"` dataset failed!"))
    } else
    {
      res = get(load(file = paste0("./", data.name, ".RData")))
      message(paste0("Download of `", data.name,"` dataset succeeded!"))
      return(res)
    }
  } else
  {
    res = get(load(file = paste0("./", data.name, ".RData")))
    message(paste0("Download of `", data.name,"` dataset succeeded!"))
    return(res)
  }
}

